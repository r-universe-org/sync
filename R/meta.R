#' Sync repositories
#'
#' Find all universes that need an update and trigger sync workflow.
#'
#' @export
trigger_syncs <- function(){
  universes <- list_universes()
  results <- lapply(universes, function(universe){
    if(universe=='cran') return()
    dirty <- needs_update(universe)
    if(length(dirty)){
      print_message("[%s]: updates needed for %s", universe, paste(dirty, collapse = ', '))
      trigger_workflow(universe)
    } else {
      print_message("[%s]: universe is up-to-date", universe)
    }
    return(dirty)
  })
  names(results) <- universes
  Filter(length, results)
}

needs_update <- function(universe){
  git_cmd('clone', '--depth', '1', paste0('https://github.com/r-universe/', universe))
  fullpath <- normalizePath(universe)
  on.exit(unlink(fullpath, recursive = TRUE))
  withr::local_dir(universe)
  pkgs <- unique(c('.registry', list.files(), gert::git_submodule_list()$path))
  skiplist <- submodules_up_to_date()
  dirty <- setdiff(pkgs, skiplist)
  # If only the registry is dirty, we check if it actually needs an update
  if(identical('.registry', dirty)){
    git_cmd("submodule", "update", "--init", "--remote", "--recommend-shallow", "-f", '.registry')
    update_gitmodules()
    if(is.na(match('.gitmodules', gert::git_status()$file))){
      dirty <- setdiff(dirty, '.registry') # No update needed
    }
  }
  return(dirty)
}

list_universes <- function(){
  res <- gh::gh('/users/r-universe/repos', per_page = 100, .limit = 1e5)
  names <- tolower(vapply(res, function(x){x$name}, character(1)))
  updated <- as.POSIXct(chartr('TZ', '  ', vapply(res, function(x){x$pushed_at}, character(1))))
  names[order(updated, decreasing = TRUE)]
}

trigger_workflow <- function(universe, workflow = 'sync.yml', inputs = NULL){
  url <- sprintf('/repos/r-universe/%s/actions/workflows/%s/dispatches', universe, workflow)
  gh::gh(url, .method = 'POST', ref = 'master', inputs = inputs)
}
