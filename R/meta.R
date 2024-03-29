#' Sync repositories
#'
#' Find all universes that need an update and trigger sync workflow.
#'
#' @export
trigger_syncs <- function(){
  skiplist <- c("actions", "workflows")
  universes <- setdiff(unique(list_universes()), skiplist)
  git_clone('https://github.com/r-universe-org/cran-to-git', '/tmp/cran-to-git')
  git_clone('https://github.com/r-universe/workflows', '/tmp/workflows')
  results <- lapply(universes, check_and_trigger)
  names(results) <- universes
  out <- Filter(length, results)
  print(out)
  errors <- out[vapply(out, inherits, logical(1),  'try-error')]
  if(length(errors)) {
    for(i in seq_along(errors)){
      print_message("ERROR updating %s: %s", names(errors[i]), errors[i])
    }
    stop("Failure syncing some universes")
  }
  print_message("All universes synced OK!")
}

check_and_trigger <- function(universe){
  try({
    dirty <- needs_update(universe)
    if(length(dirty)){
      print_message("[%s]: updates needed for %s", universe, paste(dirty, collapse = ', '))
      trigger_workflow(universe)
    } else {
      print_message("[%s]: universe is up-to-date", universe)
    }
    return(dirty)
  })
}

needs_update <- function(universe){
  if(universe == 'cran') {
    if(format(Sys.time(), '%H') == '00') return("Everything")
    return(cran_recently_updated())
  }
  if(universe == 'bioc') {
    return(metabioc_recently_updated())
  }
  retry(git_clone(paste0('https://github.com/r-universe/', universe)))
  fullpath <- normalizePath(universe)
  on.exit(unlink(fullpath, recursive = TRUE))
  withr::local_dir(universe)
  hours_passed <- as.integer(difftime(Sys.time(), gert::git_log(max=1)$time, units = 'hours'))
  if(hours_passed > 0 && hours_passed %% (30*24) == 0){
    print_message("Triggering random montly refresh for: %s", universe)
    return('.registry')
  }
  pkgs <- unique(c('.registry', list.files(), gert::git_submodule_list()$path))
  check_new_release_tags()
  skiplist <- submodules_up_to_date()
  dirty <- setdiff(pkgs, skiplist)
  if(nchar(Sys.getenv('UPDATE_ALL_WORKFLOWS')) && file.exists('/tmp/workflows')){
    file.copy(list.files('/tmp/workflows', full.names = TRUE, pattern = 'yml$'), '.github/workflows/', overwrite = TRUE)
    if(any(grepl('^.github/workflows', gert::git_status()$file))){
      dirty <- c(dirty, 'workflows')
      print_message("Workflow files need updating for: %s", universe)
    } else {
      print_message("Workflow files are up-to-date for: %s", universe)
    }
  }
  current_registry <- url_to_repo(gert::git_submodule_info(".registry")$url)
  is_cran_registry <- basename(current_registry) == "cran-to-git"
  if(is_cran_registry){
    if(is_valid_registry(sprintf('%s/%s.r-universe.dev', universe, universe))){
      print_message("Might need to switch to custom packages.json")
      return('.registry')
    }
  } else if(is_deleted_registry(current_registry)){
    print_message("Maybe need to disable custom packages.json")
    return('.registry')
  }
  # If only the registry is dirty, we check if it actually needs an update
  if(identical('.registry', dirty)){
    if(is_cran_registry && file.exists('/tmp/cran-to-git')){
      unlink(".registry", recursive = TRUE)
      file.symlink('/tmp/cran-to-git', '.registry')
    } else {
      retry(git_submodule_shallow('.registry'))
    }
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
  created <- as.POSIXct(chartr('TZ', '  ', vapply(res, function(x){x$created_at}, character(1))))
  names[order(updated, decreasing = TRUE)]
}

trigger_workflow <- function(universe, workflow = 'sync.yml', inputs = NULL){
  url <- sprintf('/repos/r-universe/%s/actions/workflows/%s/dispatches', universe, workflow)
  gh::gh(url, .method = 'POST', ref = 'master', inputs = inputs)
}

# TODO: this may not work as intended because metacran lags behind CRAN
cran_recently_updated <- function(hours = 1){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  curl::curl_download('https://cloud.r-project.org/web/packages/packages.rds', destfile = tmp)
  cran <- as.data.frame(readRDS(tmp), stringsAsFactors = FALSE)
  cran$age <- difftime(Sys.time(),  as.POSIXct(cran[['Date/Publication']]), units = 'hours')
  cran$Package[cran$age < hours]
}

metabioc_recently_updated <- function(hours = 1){
  latest <- gh::gh("/orgs/bioc/repos", sort='pushed', per_page = 1)[[1]]
  pushed <- as.POSIXct(sub("T", " ", latest$pushed_at), tz = 'UTC')
  if(difftime(Sys.time(),  pushed, units = 'hours') < hours){
    return('Everything')
  }
}
