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
  gert::git_clone(monorepo_url, path = repo)
  pwd <- getwd()
  setwd(repo)

  # Try to set the commit status, even in case of total failure
  success <- FALSE
  on.exit({
    set_registry_commit_status(monorepo_url, success)
    setwd(pwd)
  })

  # Test if we have an app
  if(nchar(Sys.getenv('GH_APP_KEY'))){
    tryCatch({
      out <- ghapps::gh_app_installation_info(monorepo_name)
      ghapp <- out[c('id', 'created_at', 'repository_selection', 'permissions')]
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

  # Sync the workflow files
  workflows <- tempfile()
  gert::git_clone('https://github.com/r-universe-org/workflows', workflows)
  workflow_commit <- gert::git_log(repo = I(workflows), max = 1)
  infiles <- list.files(workflows, full.names = TRUE)
  destfiles <- file.path('.github/workflows', basename(infiles))
  unlink(".github/workflows/*")
  file.copy(infiles, destfiles)
  gert::git_add(".github")
  if(any(gert::git_status()$staged)){
    changed_files <- paste(gert::git_status(staged = TRUE)$file, collapse = ', ')
    print_message("Committing changes for: %s", changed_files)
    gert::git_commit(message = paste("GHA update:", trimws(workflow_commit$message)), workflow_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("GHA workflows are up-to-date")
  }

  # Consider switching to personal registry
  if(basename(gert::git_submodule_info(".registry")$url) == "cran-to-git"){
    pkgsurl <- sprintf('https://raw.githubusercontent.com/%s/universe/HEAD/packages.json', monorepo_name)
    req <- curl::curl_fetch_memory(pkgsurl)
    if(req$status_code == 200){
      message("Switching universe to personal registry!")
      regrepo <- sprintf('https://github.com/%s/universe', monorepo_name)
      sys::exec_wait("git", c("submodule", "set-url", ".registry", regrepo))
      sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
      pkgdf <- jsonlite::fromJSON('.registry/packages.json')
      if(!is.data.frame(pkgdf))
        stop("The package.json file in personal registry does not have expected structure")
      if(!all(c('package', 'url') %in% names(pkgdf)))
        stop("The package.json file does not have expected 'package and' 'url' fields")
      gert::git_add('.registry')
    }
  }

  # Sync with the user registry file
  sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
  gert::git_reset_hard('origin/HEAD', repo = I('.registry'))
  registry_commit <- gert::git_log(repo = I('.registry'), max = 1)
  update_gitmodules()
  write_metadata_json()
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
  results1 <- lapply(registry, try_update_package, update_pkg_remotes = TRUE)

  # Should not be needed but sometimes remotes linger around
  cleanup_remotes_list()

  # Now update all remotes (possibly new ones from package updates)
  # Filter out packages that already exist in the main package registry
  remotes <- read_remotes_list()
  registry_pkgs <- vapply(registry, function(x){x$package}, character(1))
  remotes <- Filter(function(x){is.na(match(x$package, registry_pkgs))}, remotes)

  # Filter duplicates
  remotes_pkgs <- vapply(remotes, function(x){x$package}, character(1))
  remotes_dups <- duplicated(remotes_pkgs)
  results2 <- lapply(remotes[!remotes_dups], try_update_package)

  # Finally get rid of deleted packages
  packages <- vapply(c(registry, remotes), function(x){x$package}, character(1))
  remove_packages <- setdiff(list.files(repo), packages)
  if(length(remove_packages)){
    lapply(remove_packages, function(pkg_dir){
      print_message("Deleting %s", pkg_dir)
      gert::git_rm(pkg_dir)
      unlink(pkg_dir, recursive = TRUE)
      update_remotes_json(desc = list(package = pkg_dir))
      update_gitmodules()
      gert::git_add(c('.remotes.json', '.gitmodules'))
    })
    msg <- paste("Deleting packages:", paste0(remove_packages, collapse = ', '))
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
      message(sprintf("\nERROR updating %s from %s (%s)\n", x$package, x$url, attr(x, 'error')))
      x$package
    }, character(1))
    stop("Failed to update packages: ", paste(pkgs, collapse = ', '))
  } else {
    success <- TRUE
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
      token <- ghapps::gh_app_token(repo, app_id = '87942')
      endpoint <- sprintf('/repos/%s/statuses/%s', repo, registry_submodule$head)
      context <- sprintf('r-universe/%s/sync', basename(repo))
      description <- 'Update R-universe monorepo from registryr'
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

try_update_package <- function(x, update_pkg_remotes = FALSE){
  tryCatch(update_one_package(x = x, update_pkg_remotes = update_pkg_remotes), error = function(e){
    gert::git_reset_hard()
    structure(x, class = 'update_failure', error = e$message)
  })
}

# Sync the registry packages with the monorepo
update_one_package <- function(x, update_pkg_remotes = FALSE){
  pkg_dir <- x$package
  pkg_url <- x$url
  pkg_branch <- ifelse(length(x$branch), x$branch, 'HEAD')
  if(isFALSE(x$available)){
    print_message("Skipping unavailable package %s", pkg_dir)
    return()
  }
  submodule <- sys::exec_internal("git", c("submodule", "status", pkg_dir), error = FALSE)
  if(submodule$status != 0){
    print_message("Adding new package '%s' from: %s", pkg_dir, pkg_url)
    sys::exec_wait("git", c("submodule", "add", "--force", pkg_url, pkg_dir))
    submodule <- sys::exec_internal("git", c("submodule", "status", pkg_dir), error = FALSE)
  }
  if(submodule$status == 0){
    submodule_head <- sub("^[+-]", "", sys::as_text(submodule$stdout))
    if(pkg_branch == '*release')
      pkg_branch <- lookup_github_release(pkg_url)
    out <- sys::exec_internal('git', c("ls-remote", pkg_url, pkg_branch))
    if(!length(out$stdout)){
      if(grepl(paste0("^", pkg_branch), submodule_head)){
        print_message("Package '%s' already at commit '%s'", pkg_dir, submodule_head)
      } else {
        print_message("No such branch '%s' for package '%s'. Skipping...", pkg_branch, pkg_dir)
        gert::git_reset_hard()
      }
      return()
    }
    remote_head <- strsplit(sys::as_text(out$stdout), '\\W')[[1]][1]
    if(!(pkg_dir %in% gert::git_status()$file) && grepl(remote_head, submodule_head, fixed = TRUE)){
      print_message("Submodule %s unchanged (%s)", pkg_dir, remote_head)
      return()
    }
    print_message("Updating package '%s' from: %s", pkg_dir, pkg_url)
    sys::exec_wait("git", c("update-index", "--cacheinfo", "160000", remote_head, pkg_dir))
    sys::exec_wait("git", c("submodule", "update", "--init", pkg_dir))
  } else {
    print_message("FAILED to init submodule %s", pkg_dir)
  }
  gert::git_add(pkg_dir)
  if(!any(gert::git_status()$staged)){
    print_message("Submodule '%s' already up-to-date", pkg_dir)
  } else {
    r_pkg_dir <- ifelse(length(x$subdir) > 0, file.path(pkg_dir, x$subdir), pkg_dir)
    desc <- get_description_data(r_pkg_dir)
    if(isTRUE(update_pkg_remotes)){
      update_remotes_json(desc)
      update_gitmodules()
      gert::git_add(c('.remotes.json', '.gitmodules'))
    }
    subrepo <- gert::git_open(pkg_dir)
    stopifnot(basename(gert::git_info(repo = subrepo)$path) == pkg_dir)
    pkg_commit <- gert::git_log(repo = subrepo, max = 1)
    person <- utils::as.person(desc$maintainer)[1]
    sig <- paste(format(person, include = c("given", "family", "email")), unclass(pkg_commit$time))
    gert::git_commit(message = paste(desc$package, desc$version), author = sig)
    gert::git_push(verbose = TRUE)
  }
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
  lapply(out, function(x){
    x$from = desc$package
    x$via <- I(x$via)
    return(x)
  })
}

# Recursively download description files and get remotes.
# Skip already known packages to prevent infinite recursion.
get_recursive_remotes <- function(desc, via = NULL){
  via <- c(via, desc$package)
  if(!length(desc$remotes) || !nchar(desc$remotes))
    return(NULL)
  remotes_repos <- trimws(strsplit(desc$remotes, ',')[[1]])
  all_lists <- lapply(remotes_repos, function(x){
    info <- remotes::parse_repo_spec(x)
    info$username <- sub("^github::", "", info$username)
    if(info$repo %in% via)
      return(NULL)
    out <- list(
      package = info$repo,
      url = sprintf("https://github.com/%s/%s", info$username, info$repo),
      via = via
    )
    if(length(info$ref) && nchar(info$ref)){
      out$branch = info$ref
    }
    if(length(info$subdir) && nchar(info$subdir)){
      out$subdir = info$subdir
    }
    sub_remotes <- get_recursive_remotes(get_github_description(info), via = via)
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
  message(sprintf(...))
}

read_registry_list <- function(){
  monorepo_url <- gert::git_remote_info()$url
  jsonfile <- sprintf('.registry/%s.json', sub("_", "@", basename(monorepo_url), fixed = TRUE))
  if(file.exists(jsonfile)){
    jsonlite::read_json(jsonfile)
  } else {
    stopifnot("packages.json does not exist" = file.exists('.registry/packages.json'))
    jsonlite::read_json('.registry/packages.json')
  }
}

update_gitmodules <- function(){
  registry <- lapply(read_registry_list(), function(x){c(x, registered = TRUE)})
  remotes <- read_remotes_list()
  registry_url <- gert::git_remote_list(repo = I('.registry'))$url
  pkgs <- c(list(list(
    package = '.registry',
    url = registry_url,
    branch = 'HEAD' #git assumes 'master' otherwise!
  )), registry, remotes)
  pkgs_names <- vapply(pkgs, function(x){x$package}, character(1))
  pkgs <- pkgs[!duplicated(pkgs_names)]
  lines <- vapply(pkgs, function(x){
    if(!length(x$package))
      stop("Field 'package' missing from registry entry")
    if(!length(x$url))
      stop("Field 'url' missing from registry entry")
    str <- sprintf('[submodule "%s"]\n\tpath = %s\n\turl = %s\n\tshallow = true',
            x$package, x$package, x$url)
    if(length(x$branch))
      str <- paste0(str, '\n\tbranch = ', x$branch)
    if(length(x$subdir))
      str <- paste0(str, '\n\tsubdir = ', x$subdir)
    if(x$package != '.registry')
      str <- paste0(str, '\n\tregistered = ', tolower(isTRUE(x$registered)))
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
  desc <- as.list(tools:::.read_description(path))
  names(desc) <- tolower(names(desc))
  if(!length(desc$maintainer)){
    authors <- desc[['authors@r']]
    if(length(authors)){
      maintainer <- tryCatch(find_maintainer_safe(authors), error = function(e){
        stop(sprintf("Failed to parse Authors@R for package '%s': %s", desc$package, e$message))
      })
      desc <- c(desc, maintainer)
    }
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
  env$person <- function(..., comment = NULL){
    utils::person(...)
  }
  aar <- eval(expr, envir = env)
  maintainer <- utils:::.format_authors_at_R_field_for_maintainer(aar)
  if(length(maintainer) && nchar(maintainer))
    return(c(maintainer = maintainer))
}

write_metadata_json <- function(){
  allcran <- row.names(utils::available.packages('https://cloud.r-project.org/src/contrib'))
  registry <- read_registry_list()
  packages <- vapply(registry, function(x){x$package}, character(1))
  oncran <- vapply(registry, function(x){
    if(x$package %in% allcran)
      return(test_if_package_on_cran(x))
    return(NA)
  }, logical(1))
  df <- data.frame(package = packages, oncran = oncran)
  jsonlite::write_json(df, '.metadata.json', pretty = TRUE)
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
    if(!grepl("github.com", pkg_url))
      stop('A "branch":"*release" is only supported github.com URLs')
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
