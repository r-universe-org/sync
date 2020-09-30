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
    gert::git_commit(message = "Updating GHA workflow scripts", workflow_commit$author)
    gert::git_push()
  } else {
    print_message("GHA workflows are up-to-date")
  }

  # Sync with the user registry file
  sys::exec_wait("git", c("submodule", "update", "--init", "--remote", '.registry'))
  gert::git_reset_hard('origin/HEAD', repo = I('.registry'))
  jsonfile <- sprintf('.registry/%s.json', sub("_", "@", basename(monorepo_url)))
  registry <- if(file.exists(jsonfile)){
    jsonlite::read_json(jsonfile)
  } else {
    jsonlite::read_json('.registry/packages.json')
  }
  registry_url <- gert::git_remote_list(repo = I('.registry'))$url
  generate_gitmodules(pkgs = registry, registry_url = registry_url)
  registry_commit <- gert::git_log(repo = I('.registry'), max = 1)
  gert::git_add('.gitmodules')
  if(any(gert::git_status()$staged)){
    print_message("Sync registry with upstream")
    gert::git_add('.registry')
    gert::git_commit(message = "Sync registry", registry_commit$author)
    gert::git_push()
  } else {
    gert::git_reset_hard('HEAD', repo = I('.registry'))
    print_message("Registry is up-to-date")
  }

  # First get rid of deleted packages
  packages <- vapply(registry, function(x){x$package}, character(1))
  remove_packages <- setdiff(list.files(repo), packages)
  if(length(remove_packages)){
    lapply(remove_packages, function(pkg_dir){
      print_message("Deleting %s", pkg_dir)
      gert::git_rm(pkg_dir)
      unlink(pkg_dir, recursive = TRUE)
    })
    msg <- paste("Deleting packages:", paste0(remove_packages, collapse = ', '))
    gert::git_commit(msg, registry_commit$author)
    gert::git_push()
  }

  # Update all the submodules!
  # sys::exec_wait("git", c("submodule", "update", "--init", "--recursive", "--remote", "--jobs", "8"))
  # Todo: can we update the module without cloning?

  # Sync the registry packages with the monorepo
  lapply(registry, function(x){
    pkg_dir <- x$package
    pkg_url <- x$url
    if(isFALSE(x$available)){
      print_message("Skipping unavailable package %s", pkg_dir)
      return()
    }
    submodule <- sys::exec_internal("git", c("submodule", "status", pkg_dir), error = FALSE)
    if(submodule$status == 0){
      branch <- ifelse(length(x$branch) > 0, x$branch, 'HEAD')
      out <- sys::exec_internal('git', c("ls-remote", pkg_url, branch))
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
      desc <- get_description_data(pkg_dir)
      subrepo <- gert::git_open(pkg_dir)
      stopifnot(basename(gert::git_info(repo = subrepo)$path) == pkg_dir)
      pkg_commit <- gert::git_log(repo = subrepo, max = 1)
      sig <- paste(trimws(desc$maintainer), unclass(pkg_commit$time))
      gert::git_commit(message = paste(desc$package, desc$version), author = sig)
      gert::git_push()
    }
  })
  invisible(packages)
}

print_message <- function(...){
  message(sprintf(...))
}

generate_gitmodules <- function(pkgs, registry_url){
  pkgs <- c(list(list(
    package = '.registry',
    url = registry_url
  )), pkgs)
  lines <- vapply(pkgs, function(x){
    if(!length(x$package))
      stop("Field 'package' missing from registry entry")
    if(!length(x$url))
      stop("Field 'url' missing from registry entry")
    str <- sprintf('[submodule "%s"]\n\tpath = %s\n\turl = %s\n\tshallow = true',
            x$package, x$package, x$url)
    if(length(x$branch))
      str <- paste0(str, '\n\tbranch = ', x$branch)
    return(str)
  }, character(1))
  writeLines(lines, '.gitmodules')
}

get_description_data <- function(pkg_dir){
  path <- file.path(pkg_dir, 'DESCRIPTION')
  desc <- tools:::.read_description(path)
  extra <- tools:::.expand_package_description_db_R_fields(desc)
  out <- as.list(c(desc, extra))
  names(out) <- tolower(names(out))
  if(!length(out$maintainer))
    stop("Failed to extract maintainer from description: ", pkg_dir)
  return(out)
}
