#' Update the Monorepo
#'
#' Synchronizes submodules in the monorepo from a registry file.
#'
#' @export
#' @param monorepo_url full git URL of monorepo
sync_from_registry <- function(monorepo_url = Sys.getenv('MONOREPO_URL')){
  # Clone and cd into the monorepo
  monorepo_url <- sub("\\.git$", "", monorepo_url)
  monorepo_name <- basename(monorepo_url)
  repo <- file.path(tempdir(), paste0(monorepo_name, '-universe'))
  unlink(repo, recursive = TRUE)
  #gert::git_clone(monorepo_url, path = repo)
  git_cmd("clone", monorepo_url, repo, timeout = 300)
  options('gert.use.repo.cache' = TRUE)
  pwd <- getwd()
  setwd(repo)

  # Try to set the commit status, even in case of total failure
  action_success <- FALSE
  on.exit({
    set_registry_commit_status(monorepo_url, action_success)
    setwd(pwd)
  })

  # Test if we have an app
  if(nchar(Sys.getenv('GH_APP_KEY'))){
    tryCatch({
      out <- ghapps::gh_app_installation_info(monorepo_name)
      ghapp <- out[c('id', 'created_at', 'repository_selection', 'permissions')]
      if(ghapp$repository_selection == 'selected'){
        repolist <- ghapps::gh_app_installation_repositories(monorepo_name)
        ghapp$repositories <- I(vapply(repolist$repositories,function(x)x$name, character(1)))
      }
      jsonlite::write_json(ghapp, '.ghapp', pretty = TRUE, auto_unbox = TRUE)
    }, http_error_404 = function(e){
      unlink('.ghapp')
      print_message("Did not find an app installation for: %s", monorepo_name)
    }, error = function(e){
      print_message("Error checking for app installation for: %s", monorepo_name)
      print(e)
    })
    gert::git_add('.ghapp')
  } else {
    print_message("No GH_APP_KEY found, skipping app checks")
  }

  # Check for changes in GHA scripts
  update_workflows(monorepo_name)

  # Check for potentially missing source packages
  missings <- setdiff(list.files(), universe_ls(monorepo_name))
  if(length(missings)){
    message("Missing source packages: ", paste(missings, collapse = ', '))
    lapply(missings, function(pkg){
      try(trigger_rebuild(monorepo_name, pkg))
    })
  }

  # Consider switching to a personal registry if available
  current_registry <- url_to_repo(gert::git_submodule_info(".registry")$url)
  message("Registry is set to: ", current_registry)
  if(basename(current_registry) != paste0(monorepo_name, '.r-universe.dev')){
    update_registry_repo(monorepo_name, current_registry)
  }

  # Sync with the user registry file (currently libgit2 does not support shallow clones, sadly)
  res <- sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))

  # If this fails, check if the personal registry still exists
  if(res != 0 || !file.exists('.registry/packages.json')){
    if(basename(current_registry) != 'cran-to-git' && is_deleted_registry(current_registry)){
      newrepo <- update_registry_repo(monorepo_name, current_registry)
      if(is.null(newrepo)){
        switch_to_registry('r-universe-org/cran-to-git', validate = FALSE)
      }
      sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
    } else {
      stop("Failure cloning .registry repository")
    }
  }

  gert::git_reset_hard('origin/HEAD', repo = I('.registry'))
  registry_commit <- gert::git_log(repo = I('.registry'), max = 1)
  update_gitmodules()
  write_metadata_json()

  # Very basic fair use check for now
  registry_name <- basename(gert::git_submodule_info(".registry")$url)
  if(!(registry_name %in% c("roregistry", "cran-to-git", "community", "production", "cranhaven.r-universe.dev"))){
    if(length(read_registry_list()) > 150){
      stop("Personal universes are currently limited to 150 packages")
    }
  }
  gert::git_add(c('.gitmodules', '.metadata.json'))
  if(any(gert::git_status()$staged)){
    print_message("Sync registry with upstream")
    if('.gitmodules' %in% gert::git_status(staged = TRUE)$file)
      gert::git_add('.registry')
    gert::git_commit(message = "Sync registry", registry_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    gert::git_reset_hard('HEAD', repo = I('.registry'))
    print_message("Registry is up-to-date")
  }

  # First update all packages from the registry
  registry <- read_registry_list()
  registry_pkgs <- vapply(registry, function(x){x$package}, character(1))
  registry_dups <- duplicated(registry_pkgs)
  if(any(registry_dups)){
    print_message("Registry has DUPLICATED packages: '%s'", registry_pkgs[registry_dups])
  }
  check_new_release_tags()
  skiplist <- submodules_up_to_date(skip_broken = FALSE)
  print_message("Submodules up-to-date:\n %d", length(skiplist))
  dirty <- Filter(function(x){is.na(match(x$package, skiplist))}, registry[!registry_dups])
  results1 <- lapply(dirty, try_update_package, update_pkg_remotes = TRUE)

  # Should not be needed but sometimes remotes linger around
  cleanup_remotes_list()

  # Now update all remotes (possibly new ones from package updates)
  # Filter out packages that already exist in the main package registry
  remotes <- read_remotes_list()
  remotes <- Filter(function(x){is.na(match(x$package, registry_pkgs))}, remotes)

  # Filter duplicates
  remotes_pkgs <- vapply(remotes, function(x){x$package}, character(1))
  remotes_dups <- duplicated(remotes_pkgs)
  dirty_remotes <- Filter(function(x){is.na(match(x$package, skiplist))}, remotes[!remotes_dups])
  results2 <- lapply(dirty_remotes, try_update_package)

  # Finally get rid of deleted packages
  packages <- vapply(c(registry, remotes), function(x){x$package}, character(1))
  remove_packages <- setdiff(list.files(repo), packages)
  if(length(remove_packages)){
    msg <- paste("Deleting packages:", paste0(remove_packages, collapse = ', '))
    lapply(remove_packages, delete_one_package)
    gert::git_commit(msg, registry_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("No packages to delete. Everything is up-to-date")
  }

  # Check for update failures
  failures <- Filter(function(x){
    inherits(x, 'update_failure')
  }, c(results1, results2))

  if(length(failures) > 0){
    pkgs <- vapply(failures, function(x){
      errmsg <- sprintf("ERROR updating %s from %s (%s)", x$package, x$url, attr(x, 'error'))
      cat(sprintf("::error file=%s::%s\n", x$package, gsub("\\s+", " ", errmsg)))
      #message(errmsg)
      x$package
    }, character(1))
    stop("Failed to update packages: ", paste(pkgs, collapse = ', '))
  } else if(length(registry) == 0){
    stop(sprintf("GitHub user '%s' does not have a packages.json file, nor any CRAN packages.
Please create a package registry as explained on: https://ropensci.org/blog/2021/06/22/setup-runiverse/", monorepo_name), call. = FALSE)
  } else {
    action_success <- TRUE
  }
}

# Update commit status for upstream universe registry repo
set_registry_commit_status <- function(monorepo_url, success){
  run_id <- Sys.getenv('GITHUB_RUN_ID')
  registry_submodule <- gert::git_submodule_info(".registry")
  registry_repo <- registry_submodule$url
  if(basename(registry_repo) != "cran-to-git" && nchar(run_id) && nchar(Sys.getenv('GH_APP_KEY'))){
    tryCatch({
      repo <- sub("https?://github.com/", "", registry_repo)
      repo <- sub("\\.git$", "", repo)
      token <- ghapps::gh_app_token(repo)
      sha <- tryCatch({
        # try 'dirty' .registry repo HEAD, if unavail, use staged submodule state
        gert::git_info(I(".registry"))$commit
      }, error = function(e){registry_submodule$head})
      endpoint <- sprintf('/repos/%s/statuses/%s', repo, sha)
      context <- sprintf('r-universe/%s/sync', dirname(repo))
      description <- 'Update R-universe monorepo from registry'
      state <- ifelse(isTRUE(success), 'success', 'failure')
      url <- sprintf('%s/actions/runs/%s', monorepo_url, run_id)
      gh::gh(endpoint, .method = 'POST', .token = token, state = state,
             target_url = url, context = context, description = description)
      cat("Succesfully set commit status to:", url, "\n")
    }, error = function(e){
      cat("Failed to set commit status:", e$message, "\n")
    })
  }
}

try_update_package <- function(x, ...){
  tryCatch(update_one_package(x = x, ...), error = function(e){
    gert::git_reset_hard()
    cat("ERROR", e$message, '\n', file = stderr())
    structure(x, class = 'update_failure', error = e$message)
  })
}

delete_one_package <- function(pkg_dir){
  print_message("Deleting %s", pkg_dir)
  gert::git_rm(pkg_dir)
  unlink(pkg_dir, recursive = TRUE)
  update_remotes_json(desc = list(package = pkg_dir))
  update_gitmodules()
  gert::git_add(c('.remotes.json', '.gitmodules'))
}

# Sync the registry packages with the monorepo
update_one_package <- function(x, update_pkg_remotes = FALSE, cleanup_after = FALSE){
  pkg_dir <- x$package
  if(grepl("/", pkg_dir)){
    stop("Package name should not contain slashes: ", pkg_dir)
  }
  pkg_url <- x$url
  pkg_branch <- ifelse(length(x$branch), x$branch, 'HEAD')
  if(isFALSE(x$available)){
    print_message("Skipping unavailable package %s", pkg_dir)
    return()
  }
  submodule <- sys::exec_internal("git", c("submodule", "status", pkg_dir), error = FALSE)
  if(submodule$status != 0){
    print_message("Adding new package '%s' from: %s", pkg_dir, pkg_url)
    git_cmd_assert("submodule", "add", "--force", pkg_url, pkg_dir)
    if(pkg_branch == '*release')
      pkg_branch <- update_release_branch(pkg_dir, pkg_url)
    gert::git_submodule_set_to(submodule = pkg_dir, ref = pkg_branch)
  } else {
    submodule_head <- sub("^[+-]", "", sys::as_text(submodule$stdout))
    if(pkg_branch == '*release')
      pkg_branch <- update_release_branch(pkg_dir, pkg_url)
    out <- git_cmd_assert("ls-remote", pkg_url, pkg_branch)
    if(length(out$stdout)){
      remote_head <- strsplit(sys::as_text(out$stdout), '\\W')[[1]][1]
    } else {
      if(grepl(paste0("^", pkg_branch), submodule_head)){
        print_message("Package '%s' already at commit '%s'", pkg_dir, submodule_head)
        return()
      } else if(grepl('^[0-9a-f]{6,100}$', tolower(pkg_branch))){
        # Assume pkg_branch is a raw commit hash
        remote_head <- tolower(pkg_branch)
      } else {
        stop(sprintf("No such branch '%s' for package '%s'. Skipping...", pkg_branch, pkg_dir))
        gert::git_reset_hard()
        return()
      }
    }
    if(!(pkg_dir %in% gert::git_status()$file) && grepl(remote_head, submodule_head, fixed = TRUE)){
      print_message("Submodule %s unchanged (%s)", pkg_dir, remote_head)
      return()
    }
    print_message("Updating package '%s' from: %s", pkg_dir, pkg_url)
    git_cmd_assert("update-index", "--cacheinfo", "160000", remote_head, pkg_dir)

    git_cmd_assert("submodule", "update", "--init", pkg_dir)
  }
  gert::git_add(pkg_dir)
  if(!any(gert::git_status()$staged)){
    print_message("Submodule '%s' already up-to-date", pkg_dir)
  } else {
    r_pkg_dir <- ifelse(length(x$subdir) > 0, file.path(pkg_dir, x$subdir), pkg_dir)
    desc <- get_description_data(r_pkg_dir)
    if(!identical(desc$package, pkg_dir)){
      delete_one_package(pkg_dir) #NB: deleting here makes files out of sync with registry/.gitmodules
      errmsg <- sprintf("Package '%s' from registry does not match package name in description file: '%s'", pkg_dir, paste(desc$package, collapse = ""))
      if(nrow(gert::git_status(staged = TRUE))){
        # Pkg was already in the universe and needs to be removed now
        commit_as_bot(errmsg)
        gert::git_push(verbose = TRUE)
      }
      stop(errmsg)
    }
    if('config/runiverse/noindex' %in% names(desc)){
      stop(sprintf("Package '%s' has noindex in description file", desc$package))
    }
    if(isTRUE(update_pkg_remotes)){
      update_remotes_json(desc)
      update_gitmodules()
      gert::git_add(c('.remotes.json', '.gitmodules'))
    }
    subrepo <- gert::git_open(pkg_dir)
    stopifnot(basename(gert::git_info(repo = subrepo)$path) == pkg_dir)
    pkg_commit <- gert::git_log(repo = subrepo, max = 1)
    sig <- normalize_maintainer(desc$maintainer)
    validate_signature(sig) # validates email syntax from description
    sig <- paste(sig, unclass(pkg_commit$time)) # add timestamp
    git_cmd("pull", "--rebase")
    gert::git_commit(message = paste(desc$package, desc$version), author = sig)
    gert::git_push(verbose = TRUE)
    if(cleanup_after){
      sys::exec_wait("git", c("submodule", "deinit", pkg_dir), std_out = FALSE)
    }
  }
}

validate_signature <- function(str){
  tryCatch(gert::git_signature_parse(str), error = function(e){
    stop(sprintf("Error parsing '%s'\n%s", str, e$message))
  })
}

normalize_email <- function(x){
  sub("[+].+@gmail.com", '@gmail.com', x)
}

normalize_maintainer <- function(x){
  x <- sub(">.*", ">", x) # in Bioc multiple maintainers are allowed. Get the first one.
  person <- utils::as.person(x)[1]
  if(!length(person$email)){
    message(sprintf("ERROR parsing Maintainer field for %s", x))
    return(x)
  }
  person$email <- normalize_email(person$email)
  format(person, include = c("given", "family", "email"))
}

update_remotes_json <- function(desc){
  new_remotes <- get_all_remotes(desc)
  old_remotes <- read_remotes_list()
  cur_remotes <- vapply(old_remotes, function(x){x$from == desc$package}, logical(1))
  if(any(cur_remotes) || length(new_remotes)){
    other_remotes <- old_remotes[!cur_remotes]
    all_remotes <- c(other_remotes, new_remotes)
    write_remotes_json(all_remotes)
  }
}

write_remotes_json <- function(all_remotes){
  if(length(all_remotes)){
    sort_key <- vapply(all_remotes, function(x){
      paste0(x$package, '-', x$from)
    }, character(1))
    all_remotes <- lapply(all_remotes[order(sort_key)], function(x){
      x$via = I(x$via)
      return(x)
    })
    jsonlite::write_json(all_remotes, '.remotes.json', auto_unbox = TRUE, pretty = TRUE)
  } else {
    unlink('.remotes.json')
  }
}

get_all_remotes <- function(desc){
  out <- get_recursive_remotes(desc)
  # Some package networks with extensive remotes structures depend on others via many paths
  # for example 'displayr'. We only record the first one to prevent remotes.json from blowing up.
  dups <- duplicated(vapply(out, function(x) x$package, character(1)))
  out <- out[!dups]
  lapply(out, function(x){
    x$from = desc$package
    x$via <- I(x$via)
    return(x)
  })
}

# Recursively download description files and get remotes.
# Skip already known packages to prevent infinite recursion.
# Also skip 'CRAN' remotes
get_recursive_remotes <- function(desc, via = NULL){
  via <- c(via, desc$package)
  if(!length(desc$remotes) || !nchar(desc$remotes))
    return(NULL)
  remotes_repos <- trimws(strsplit(desc$remotes, ',')[[1]])
  all_lists <- lapply(remotes_repos, function(x){
    info <- try(remotes::parse_repo_spec(x))
    if(inherits(info, 'try-error')){
      return(NULL)
    }
    info$username <- sub("^github::", "", info$username)
    if(info$repo %in% via)
      return(NULL)
    if(identical(tolower(info$username), 'cran')){
      # Ignore /cran remote if package is on CRAN (not archived)
      if(exists_on_cran(info$repo)){
        return(NULL)
      }
    }
    desc <- try(get_github_description_cached(info))
    if(inherits(desc, 'try-error')){
      message("Failed to get remote: ", x)
      return(NULL)
    }
    out <- list(
      package = desc$package,
      url = sprintf("https://github.com/%s/%s", info$username, info$repo),
      via = via
    )
    if(length(info$ref) && nchar(info$ref)){
      out$branch = info$ref
    }
    if(length(info$subdir) && nchar(info$subdir)){
      out$subdir = info$subdir
    }
    sub_remotes <- get_recursive_remotes(desc, via = via)
    c(list(out), sub_remotes)
  })
  do.call(c, all_lists)
}

get_github_description <- function(x){
  branch <- ifelse(length(x$branch) > 0, x$branch, 'HEAD')
  filename <- ifelse(length(x$subdir) > 0, paste0(x$subdir, '/DESCRIPTION'), 'DESCRIPTION')
  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s",
                 x$username, x$repo, branch, filename)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  print_message(paste("Looking for remotes in:", url))
  curl::curl_download(url, tmp)
  read_description_file(tmp)
}

exists_on_cran <- function(pkg){
  req <- curl::curl_fetch_memory(sprintf('https://cran.r-project.org/web/packages/%s/DESCRIPTION', pkg))
  return(req$status_code == 200)
}

get_github_description_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(x){
    key <- digest::digest(x)
    if(!exists(key, envir = cache)){
      val <- get_github_description(x)
      assign(key, val, envir = cache)
    }
    get0(key, cache)
  }
})

read_remotes_list <- function(){
  if(file.exists('.remotes.json'))
    jsonlite::read_json('.remotes.json', simplifyVector = TRUE, simplifyDataFrame = FALSE)
}

cleanup_remotes_list <- function(){
  registry <- read_registry_list()
  packages <- vapply(registry, function(x){x$package}, character(1))
  remotes <- Filter(function(x){
    x$from %in% packages
  }, read_remotes_list())
  write_remotes_json(remotes)
}

print_message <- function(...){
  message(paste(sprintf(...), collapse = '\n'))
}

read_registry_list <- function(){
  monorepo_url <- gert::git_remote_info()$url
  universe <- sub("_", "@", basename(monorepo_url), fixed = TRUE)
  if(universe == 'cran'){
    return(metacran_dummy_registry())
  }
  if(universe == 'bioc'){
    return(metabioc_dummy_registry())
  }
  jsonfile <- sprintf('.registry/%s.json', universe)
  registry <- if(file.exists(jsonfile)){
    jsonlite::read_json(jsonfile)
  } else {
    stopifnot("packages.json does not exist" = file.exists('.registry/packages.json'))
    jsonlite::read_json('.registry/packages.json')
  }
  Filter(function(x){!isFALSE(x$available)}, registry)
}

# Should we tolowercase here?
# I think git urls could be case sensitive?
normalize_git_url <- function(url){
  url <- sub("\\.git$", "", url)
  url <- sub("/$", "", url)
  trimws(url)
}

update_gitmodules <- function(){
  registry <- lapply(read_registry_list(), function(x){c(x, registered = TRUE)})
  remotes <- lapply(read_remotes_list(), function(x){c(x, registered = FALSE)})
  registry_url <- gert::git_remote_list(repo = I('.registry'))$url
  pkgs <- c(list(list(
    package = '.registry',
    url = registry_url,
    branch = 'HEAD' #git assumes 'master' otherwise!
  )), registry, remotes)
  pkgs_names <- vapply(pkgs, function(x){x$package}, character(1))
  pkgs <- pkgs[!duplicated(pkgs_names)]
  new_names <- vapply(pkgs, function(x){x$package}, character(1))
  pkgs <- pkgs[order(new_names, method = 'radix')]
  lines <- vapply(pkgs, function(x){
    if(!length(x$package))
      stop("Field 'package' missing from registry entry")
    if(grepl("/", x$package))
      stop("Package name should not contain slashes: ", x$package)
    if(!length(x$url))
      stop("Field 'url' missing from registry entry")
    str <- sprintf('[submodule "%s"]\n\tpath = %s\n\turl = %s',
            x$package, x$package, normalize_git_url(x$url))
    if(length(x$branch)){
      if(identical(x$branch, '*release')) # keep release we have currently
        x$branch <- get_release_version(x$package)
      str <- paste0(str, '\n\tbranch = ', x$branch[1])
    }
    if(length(x$subdir))
      str <- paste0(str, '\n\tsubdir = ', x$subdir[1])
    if(x$package != '.registry' && isFALSE(x$registered))
      str <- paste0(str, '\n\tregistered = false')
    return(str)
  }, character(1))
  writeLines(lines, '.gitmodules')
}

get_description_data <- function(pkg_dir){
  path <- file.path(pkg_dir, 'DESCRIPTION')
  out <- read_description_file(path)
  if(!length(out$maintainer) || !nchar(out$maintainer))
    stop("Failed to extract maintainer from description: ", pkg_dir)
  return(out)
}

read_description_file <- function(path){
  desc <- tryCatch(as.list(tools:::.read_description(path)), error = function(err){
    stop("Failed to read DESCRIPTION: ", err)
  })
  names(desc) <- tolower(names(desc))
  if(!length(desc[['maintainer']]) || identical(tolower(desc$maintainer), 'orphaned')){
    authors <- desc[['authors@r']]
    if(length(authors)){
      maintainer <- tryCatch(find_maintainer_safe(authors), error = function(e){
        stop(sprintf("Failed to parse Authors@R for package '%s': %s", desc$package, e$message))
      })
      desc$maintainer <- as.character(maintainer)
    }
  }
  if(identical(tolower(desc$maintainer), 'orphaned')){
    desc$maintainer <- desc[['x-cran-original-maintainer']]
  }
  # Hack for CRAN failing to set x-cran-original-maintainer
  if(basename(dirname(path)) == 'fastclime'){
    desc$maintainer <- 'Haotian Pang <hpang@princeton.edu>'
  }
  return(desc)
}

# Adapted from tools:::.expand_package_description_db_R_fields
# The latter uses eval which can be abused for code injection
find_maintainer_safe <- function(authors){
  expr <- parse(text = authors)
  env <- new.env(parent = emptyenv())
  env$c <- base::c
  env$list <- base::list
  env$paste <- base::paste
  env$paste0 <- base::paste0
  env$as.person <- utils::as.person
  env[['(']] = utils::getFromNamespace('(', 'base') # Some people use e.g: role = ("aut")
  env$person <- function(..., comment = NULL){
    utils::person(...)
  }
  aar <- eval(expr, envir = env)
  maintainer <- utils:::.format_authors_at_R_field_for_maintainer(aar)
  if(length(maintainer) && nchar(maintainer))
    return(c(maintainer = maintainer))
}

write_metadata_json <- function(){
  registry <- read_registry_list()
  registry <- Filter(function(x){!isFALSE(x$available)}, registry)
  packages <- vapply(registry, function(x){x$package}, character(1))
  isrelease <- vapply(registry, function(x){
    ifelse(identical(x$branch, '*release'), TRUE, NA)
  }, logical(1))
  df <- data.frame(package = packages, releasetag = isrelease)

  # Filter empty entries
  df <- df[which(isrelease),]
  if(nrow(df) == 0){
    unlink('.metadata.json')
  } else {
    jsonlite::write_json(df, '.metadata.json', pretty = TRUE)
  }
}

test_if_package_on_cran <- function(x){
  cran_url <- package_cran_url(x$package)
  if(length(cran_url)) {
    # Test if url is a substring of cran url (ignore subdirs)
    pkgurl <- tolower(sub("^http://", "https://", x$url))
    cmpurl <- tolower(substr(pkgurl,1,nchar(cran_url)))
    if(identical(cran_url, cmpurl))
      return(TRUE)
  }
  return(FALSE)
}

package_cran_url <- local({
  crandata <- NULL
  function(pkg){
    if(is.null(crandata)){
      crandata <<- utils::read.csv('https://r-universe-org.github.io/cran-to-git/crantogit.csv')
    }
    df <- crandata[crandata$package == pkg,]
    if(nrow(df) == 0){
      return(NULL)
    }
    return(df$url[1])
  }
})

lookup_github_release <- function(pkg_url){
  tryCatch({
    p <- remotes::parse_github_url(pkg_url)
    release <- gh::gh(sprintf("/repos/%s/%s/releases/latest", p$username, p$repo))
    if(!length(release$tag_name) || !nchar(release$tag_name))
      stop("Did not find any tag_name in output")
    return(release$tag_name)
  }, error = function(e){
    message(sprintf("Failed to find *release for: %s: %s", pkg_url, e$message))
    return('*release') # Fall back to non existing branch behavior
  })
}

lookup_gitlab_release <- function(pkg_url){
  tryCatch({
    p <- remotes::parse_github_url(sub("gitlab.com", "github.com", pkg_url, fixed = TRUE))
    endpoint <- sprintf("https://gitlab.com/api/v4/projects/%s%%2F%s/releases", p$username, p$repo)
    releases <- jsonlite::fromJSON(endpoint, simplifyVector = FALSE)
    releases <- Filter(function(x){
      !isTRUE(x$upcoming_release)
    }, releases)
    if(!length(releases))
      stop("No releases found for ", pkg_url)
    releases[[1]]$tag_name
  }, error = function(e){
    message(sprintf("Failed to find *release for: %s: %s", pkg_url, e$message))
    return('*release') # Fall back to non existing branch behavior
  })
}

get_release_version <- function(pkg){
  field <- sprintf("submodule.%s.branch", pkg)
  out <- sys::exec_internal('git', c("config", "-f", ".gitmodules", "--get", field), error = FALSE)
  ifelse(out$status, '*release', sys::as_text(out$stdout))
}

set_release_version <- function(pkg, value){
  field <- sprintf("submodule.%s.branch", pkg)
  sys::exec_internal('git', c("config", "-f", ".gitmodules", field, value))
  gert::git_add(".gitmodules")
}

set_submodule_shallow <- function(pkg){
  field <- sprintf("submodule.%s.shallow", pkg)
  sys::exec_internal('git', c("config", "-f", ".gitmodules", field, 'true'))
}

update_release_branch <- function(pkg_dir, pkg_url){
  pkg_branch <- if(grepl('gitlab.com', pkg_url)){
    lookup_gitlab_release(pkg_url)
  } else if(grepl('github.com', pkg_url)) {
    lookup_github_release(pkg_url)
  } else {
    stop('A "branch":"*release" is only supported github or gitlab URLs')
  }
  if(identical(pkg_branch, get_release_version(pkg_dir))){
    print_message("Latest release version unchanged: %s %s", pkg_dir, pkg_branch)
  } else {
    print_message("Updating release version: %s %s", pkg_dir, pkg_branch)
    set_release_version(pkg_dir, pkg_branch)
  }
  return(pkg_branch)
}

is_archived <- function(repo){
  res <- gh::gh(paste0('/repos/', repo))
  print_message("Repo %s %s archived.", repo, ifelse(res$archived, "is", "is NOT"))
  return(isTRUE(res$archived))
}

not_a_fork <- function(repo){
  res <- gh::gh(paste0('/repos/', repo))
  print_message("Repo %s %s a fork.", repo, ifelse(res$fork, "is", "is NOT"))
  print_message("Repo %s %s archived.", repo, ifelse(res$archived, "is", "is NOT"))
  return(!isTRUE(res$fork) && !isTRUE(res$archived))
}

validate_registry_repo <- function(repo){
  res <- gh::gh(paste0('/repos/', repo))
  if(tolower(res$full_name) != tolower(repo)){
    print_message("Registry %s does not match repo name %s (redirect??)", repo, tolower(res$name))
    return(FALSE)
  }
  print_message("Repo %s %s archived.", repo, ifelse(res$archived, "is", "is NOT"))
  return(!isTRUE(res$archived))
}

is_valid_registry <- function(repo_name){
  if(repo_name == 'r-universe-org/cran-to-git'){
    return(TRUE)
  }
  pkgsurl <- sprintf('https://raw.githubusercontent.com/%s/HEAD/packages.json', repo_name)
  success <- curl::curl_fetch_memory(pkgsurl)$status_code == 200
  print_message("Checking if a registry exists at %s: %s", repo_name, ifelse(success, 'yes', "no"))
  return(success && validate_registry_repo(repo_name))
}

# specifically test for 404, not some temporary network error
is_deleted_registry <- function(repo_name){
  pkgsurl <- sprintf('https://raw.githubusercontent.com/%s/HEAD/packages.json', repo_name)
  req <- curl::curl_fetch_memory(pkgsurl)
  return(req$status_code == 404 || is_archived(repo_name))
}

switch_to_registry <- function(repo_name, validate = TRUE){
  message("Switching universe to registry: ", repo_name)
  regrepo <- sprintf('https://github.com/%s', repo_name)
  sys::exec_wait("git", c("submodule", "deinit", "--force", ".registry"), std_err = FALSE)
  unlink(".git/modules/.registry", recursive = TRUE)
  sys::exec_wait("git", c("submodule", "set-url", ".registry", regrepo))
  sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
  if(isTRUE(validate)){
    pkgdf <- jsonlite::fromJSON('.registry/packages.json')
    if(!is.data.frame(pkgdf))
      stop("The package.json file in personal registry does not have expected structure")
    if(!all(c('package', 'url') %in% names(pkgdf)))
      stop("The package.json file does not have expected 'package and' 'url' fields")
  }
  options('gert.use.repo.cache' = FALSE) #flush cache
  gert::git_add('.registry')
}

# Consider switching to personal registry
update_registry_repo <- function(monorepo_name, current_registry){
  if(monorepo_name == 'ropensci'){
    return('ropensci/roregistry')
  }
  if(monorepo_name == 'r-multiverse-community'){
    return('r-multiverse/community')
  }
  if(monorepo_name == 'r-multiverse-production'){
    return('r-multiverse/production')
  }
  if(monorepo_name == 'ropensci-champions'){
    return('ropensci/champions-program')
  }
  personal_registry_repos <- c(
    sprintf('%s/%s.r-universe.dev', monorepo_name, monorepo_name))
  for(x in personal_registry_repos){
    if(is_valid_registry(x)){
      if(current_registry != x){
        switch_to_registry(x)
      } else {
        message("Keeping current registry: ", x)
      }
      return(x)
    }
  }
  return(NULL)
}

update_workflows <- function(monorepo_name){
  workflows <- tempfile()
  gert::git_clone('https://github.com/r-universe/workflows', workflows)
  if(monorepo_name == "test" || monorepo_name == 'jeroen'){
    print_message("This is the 'test' universe. Looking for a 'test' branch in workflows.")
    tryCatch(gert::git_branch_checkout('test', repo = I(workflows)), error = function(e){
      print_message("No special 'test' workflows currently. Using defaults.")
    })
  }
  workflow_commit <- gert::git_log(repo = I(workflows), max = 1)
  infiles <- list.files(workflows, pattern = 'yml$', full.names = TRUE)
  destfiles <- file.path('.github/workflows', basename(infiles))
  unlink(".github/workflows/*")
  file.copy(infiles, destfiles)
  gert::git_add(".github")
  if(any(gert::git_status()$staged)){
    changed_files <- paste(gert::git_status(staged = TRUE)$file, collapse = ', ')
    print_message("Committing changes for: %s", changed_files)
    commit_as_bot(workflow_commit$message)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("GHA workflows are up-to-date")
  }
}

check_new_release_tags <- function(){
  if(file.exists('.metadata.json')){
    lst <- Filter(function(x){isTRUE(x$releasetag)}, jsonlite::read_json('.metadata.json'))
    lapply(lst, function(x){
      pkg <- x$package
      info <- gert::git_submodule_info(pkg)
      if(grepl('github.com', info$url, fixed = TRUE)){
        update_release_branch(pkg, info$url)
      }
    })
  }
}

url_to_repo <- function(url){
  sprintf('%s/%s', basename(dirname(url)), basename(url))
}

commit_as_bot <- function(txt){
  gert::git_commit(message = paste("GHA update:", trimws(txt)), "r-universe[bot] <74155986+r-universe[bot]@users.noreply.github.com>")
}

# Use same rules as 'cranscraper'
metacran_dummy_registry <- function(archived_days = 60, skip = 'request'){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  curl::curl_download('https://cloud.r-project.org/web/packages/packages.rds', destfile = tmp)
  cran <- as.data.frame(readRDS(tmp), stringsAsFactors = FALSE)
  winonly <- which(cran$OS_type == 'windows')
  if(length(winonly)){
    cran <- cran[-winonly,]
  }
  archived <- read.csv('https://r-universe-org.github.io/cran-to-git/archived.csv')
  archived$age <- Sys.Date() - as.Date(archived$Date)
  archived <- archived[archived$age < archived_days & !grepl(skip, archived$Reason, ignore.case = TRUE),]
  pkgs <- sort(unique(c(cran$Package, archived$Package)))
  stopifnot(length(pkgs) > 20000)
  lapply(pkgs, function(x){list(package = x, url = paste0("https://github.com/cran/", x ))})
}

# Same list as 'cranscraper'
metabioc_dummy_registry <- function(){
  nomirror <- c('SwathXtend', 'h5vc') # large git files
  skiplist <- c('IntOMICS') # package was renamed bc trademarks
  yml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  bioc_version <- yml$devel_version
  bioc <- jsonlite::read_json(sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', bioc_version))
  stopifnot(length(bioc) > 2100)
  bioc <- Filter(function(x) !identical(x$PackageStatus, 'Deprecated'), bioc)
  lapply(setdiff(names(bioc), skiplist), function(x){
    baseurl <- ifelse(x %in% nomirror, "https://git.bioconductor.org/packages/", "https://github.com/bioc/")
    list(package = x, url = paste0(baseurl, x ))
  })
}

universe_ls <- function(universe){
  jsonlite::fromJSON(sprintf('https://%s.r-universe.dev/api/ls', universe))
}

trigger_rebuild <- function(repository, pkg){
  cat(sprintf("Rebuilding %s/%s\n", repository, pkg))
  url <- sprintf('/repos/r-universe/%s/actions/workflows/build.yml/dispatches', repository)
  gh::gh(url, .method = 'POST', ref = 'master', inputs = list(package = pkg))
}
