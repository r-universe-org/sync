#' Update the CRAN repo
#'
#' Synchronizes submodules in the cranrepo from a the global registry
#'
#' @export
#' @aliases sync
#' @rdname sync
#' @param path path to monorepo
update_registry <- function(path = '.'){

  # Get current state
  withr::local_dir(path)
  repo <- gert::git_open(path)
  gert::git_submodule_init('.registry', repo = repo)
  submodules <- gert::git_submodule_list(repo = repo)

  # Update .registry submodule (libgit2 does not support shallow clones)
  git_cmd("submodule", "update", "--init", "--remote", "--recommend-shallow", "-f", '.registry')
  registry_commit <- gert::git_log(repo = I('.registry'), max = 1)

  # read registry
  universe <- basename(normalizePath(path))
  registry <- if(universe == 'cran'){
    utils::read.csv('.registry/crantogit.csv')
  } else {
    regfile <- sprintf('.registry/%s.json', universe)
    jsonlite::fromJSON(ifelse(file.exists(regfile), regfile, '.registry/packages.json'))
  }
  packages <- merge(submodules, registry,by.x = 'name', by.y = 'package', all = TRUE, suffixes = c(".old",".new"))


  # Remove old packages
  pkgs_rm <- setdiff(packages$name[is.na(packages$url.new)], '.registry')
  for(pkg in pkgs_rm){
    print_message("Removing package '%s'", pkg)
    remove_submodule(pkg)
    update_remotes_json(desc = list(package = pkg))
  }

  # Find newly added packages
  pkgs_new <- which(is.na(packages$url.old))
  for(i in pkgs_new){
    pkg <- as.list(packages[i,])
    print_message("Adding new package '%s'", pkg$name)
    remove_submodule(pkg$name) # Just in case
    gert:::git_submodule_setup(pkg$url.new, pkg$name, repo = repo)
    #set_module_config(pkg$name, 'shallow', 'true')
    if(is_string(pkg$branch.new)){
      set_module_config(pkg$name, 'branch', pkg$branch.new)
    }
    if(is_string(pkg$subdir)){
      set_module_config(pkg$name, 'subdir', pkg$subdir)
    }
  }

  # Find packages with changed URLs
  for(i in which(packages$url.old != packages$url.new)){
    pkg <- as.list(packages[i,])
    print_message("Updating package '%s' to url '%s'", pkg$name, pkg$url.new)
    set_module_config(pkg$name, 'url', pkg$url.new)
  }

  # Find packages with changed branches
  for(i in which(packages$branch.old != packages$branch.new)){
    pkg <- as.list(packages[i,])
    print_message("Updating package '%s' to branch '%s'", pkg$name, pkg$branch.new)
    set_module_config(pkg$name, 'branch', pkg$branch.new)
  }

  gert::git_add(".gitmodules", '.remotes.json')
  status <- gert::git_status(staged = TRUE)
  if('.gitmodules' %in% status$file){
    status <- gert::git_add('.registry')
    gert::git_commit(message = "Sync registry", registry_commit$author)
    gert::git_push(verbose = TRUE)
  } else {
    print_message("No relevant changes in registry")
    git_cmd('submodule', 'update', '-f', '--init', '.registry')
  }
  status
}

remove_submodule <- function(pkg){
  git_cmd("rm", pkg, std_err = FALSE)
  git_cmd("config", "--remove-section", paste0('submodule.', pkg), std_err = FALSE)
  unlink(sprintf('.git/modules/%s', pkg), recursive = TRUE)
  unlink(pkg, recursive = TRUE)
}

git_cmd <- function(..., std_err = TRUE){
  # Timeout is mostly in case of unexpected password prompts
  sys::exec_wait('git', args = c(...), std_err = std_err, timeout = 60)
}

git_clone <- function(url, dest = NULL){
  for(i in 1:3){
    if(!git_cmd('clone', '--depth', '1', url, dest)){
      return(TRUE)
    }
    message("Retrying to clone: ", url)
    unlink(dest, recursive = TRUE)
    Sys.sleep(3)
  }
  stop("Failed to clone: ", url)
}

git_submodule_shallow <- function(dest){
  for(i in 1:3){
    if(!git_cmd("submodule", "update", "--init", "--remote", "--recommend-shallow", "-f", dest)){
      return(TRUE)
    }
    message("Retrying to submodule: ", dest)
    Sys.sleep(3)
  }
  stop("Failed to init submodule: ", dest)
}

set_module_config <- function(pkg, key, value){
  git_cmd('config', '--file=.gitmodules', sprintf('submodule.%s.%s', pkg, key), value)
}

get_module_config <- function(pkg, key){
  res <- sys::exec_internal('git', c('config', '--file=.gitmodules', sprintf('submodule.%s.%s', pkg, key)), error = FALSE)
  if(res$status == 0) sys::as_text(res$stdout)
}

is_string <- function(x){
  is.character(x) && !is.na(x) && nchar(x) > 0
}

retry <- function(x, times = 3, wait = 1){
  cl <- substitute(x)
  for(i in seq_len(times)){
    tryCatch({
      return(eval(cl))
    }, error = function(err){
      if(i < times){
        message(sprintf("Error '%s' in %s: Retrying...", err$message, deparse(cl)))
        Sys.sleep(wait)
      } else {
        err$message <- sprintf("%s. (tried %d times, giving up)", err$message, times)
        stop(err)
      }
    })
  }
}
