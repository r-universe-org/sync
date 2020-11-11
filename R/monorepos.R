#' Update the Monorepo
#'
#' Synchronizes submodules in the monorepo from a registry file.
#'
#' @export
#' @param monorepo_url full git URL of monorepo
sync_from_registry <- function(monorepo_url = Sys.getenv('MONOREPO_URL')){
  # Clone and cd into the monorepo
  repo <- file.path(tempdir(), paste0(basename(monorepo_url), '-universe'))
  unlink(repo, recursive = TRUE)
  gert::git_clone(monorepo_url, path = repo)
  pwd <- getwd()
  on.exit(setwd(pwd))
  setwd(repo)

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
    gert::git_commit(message = "Updating GHA workflow scripts", workflow_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("GHA workflows are up-to-date")
  }

  # Sync with the user registry file
  sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
  gert::git_reset_hard('origin/HEAD', repo = I('.registry'))
  registry_commit <- gert::git_log(repo = I('.registry'), max = 1)
  update_gitmodules()
  gert::git_add('.gitmodules')
  if(any(gert::git_status()$staged)){
    print_message("Sync registry with upstream")
    gert::git_add('.registry')
    gert::git_commit(message = "Sync registry", registry_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    gert::git_reset_hard('HEAD', repo = I('.registry'))
    print_message("Registry is up-to-date")
  }

  # First update all packages from the registry
  registry <- read_registry_list()
  lapply(registry, update_one_package, update_pkg_remotes = TRUE)

  # Now update all remotes (possibly new ones from package updates)
  # Filter out packages that already exist in the main package registry
  remotes <- read_remotes_list()
  registry_pkgs <- vapply(registry, function(x){x$package}, character(1))
  remotes <- Filter(function(x){is.na(match(x$package, registry_pkgs))}, remotes)

  # Filter duplicates
  remotes_pkgs <- vapply(remotes, function(x){x$package}, character(1))
  remotes_dups <- duplicated(remotes_pkgs)
  lapply(remotes[!remotes_dups], update_one_package)

  # Finally get rid of deleted packages
  packages <- vapply(c(registry, remotes), function(x){x$package}, character(1))
  remove_packages <- setdiff(list.files(repo), packages)
  if(length(remove_packages)){
    lapply(remove_packages, function(pkg_dir){
      print_message("Deleting %s", pkg_dir)
      gert::git_rm(pkg_dir)
      unlink(pkg_dir, recursive = TRUE)
    })
    msg <- paste("Deleting packages:", paste0(remove_packages, collapse = ', '))
    gert::git_commit(msg, registry_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("No packages to delete. Everything is up-to-date")
  }
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
  if(submodule$status == 0){
    out <- sys::exec_internal('git', c("ls-remote", pkg_url, pkg_branch))
    if(!length(out$stdout)){
      print_message("No such branch '%s' for package '%s'. Skipping...", pkg_branch, pkg_dir)
      return()
    }
    remote_head <- strsplit(sys::as_text(out$stdout), '\\W')[[1]][1]
    if(grepl(remote_head, sys::as_text(submodule$stdout), fixed = TRUE)){
      print_message("Submodule %s unchanged (%s)", pkg_dir, remote_head)
      return()
    }
    print_message("Updating package '%s' from: %s", pkg_dir, pkg_url)
    sys::exec_wait("git", c("submodule", "update", "--init", "--remote", pkg_dir))
  } else {
    print_message("Adding new package '%s' from: %s", pkg_dir, pkg_url)
    sys::exec_wait("git", c("submodule", "add", "--force", pkg_url, pkg_dir))
  }
  gert::git_reset_hard('origin/HEAD', repo = I(pkg_dir))
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
    sig <- paste(trimws(desc$maintainer), unclass(pkg_commit$time))
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

print_message <- function(...){
  message(sprintf(...))
}

read_registry_list <- function(){
  monorepo_url <- gert::git_remote_info()$url
  jsonfile <- sprintf('.registry/%s.json', sub("_", "@", basename(monorepo_url), fixed = TRUE))
  if(file.exists(jsonfile)){
    jsonlite::read_json(jsonfile)
  } else {
    jsonlite::read_json('.registry/packages.json')
  }
}

update_gitmodules <- function(){
  registry <- read_registry_list()
  remotes <- read_remotes_list()
  registry_url <- gert::git_remote_list(repo = I('.registry'))$url
  pkgs <- c(list(list(
    package = '.registry',
    url = registry_url
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
    branch <- ifelse(length(x$branch), x$branch, 'HEAD')
    str <- paste0(str, '\n\tbranch = ', branch)
    if(length(x$subdir))
      str <- paste0(str, '\n\tsubdir = ', x$subdir)
    return(str)
  }, character(1))
  writeLines(lines, '.gitmodules')
}

get_description_data <- function(pkg_dir){
  path <- file.path(pkg_dir, 'DESCRIPTION')
  out <- read_description_file(path)
  if(!length(out$maintainer))
    stop("Failed to extract maintainer from description: ", pkg_dir)
  return(out)
}

read_description_file <- function(path){
  desc <- tools:::.read_description(path)
  extra <- tools:::.expand_package_description_db_R_fields(desc)
  out <- as.list(c(desc, extra))
  names(out) <- tolower(names(out))
  return(out)
}
