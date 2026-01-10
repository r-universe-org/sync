#' Sync repositories
#'
#' Find all universes that need an update and trigger sync workflow.
#'
#' @export
trigger_syncs <- function(){
  check_github_status()
  check_cloudflare_status()
  skiplist <- c("actions", "workflows")
  universes <- setdiff(unique(list_universes()), skiplist)
  git_clone('https://github.com/r-universe-org/cran-to-git', '/tmp/cran-to-git')
  git_clone('https://github.com/r-universe-org/workflows', '/tmp/workflows')
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
  # Do not do full scan huge repos
  if(universe == 'cran' || universe == 'bioc') {
    return(github_last_update(universe, 3))
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
    write_metadata_json()
    update_config_json()
    registry_changes <- intersect(c('.gitmodules', '.config.json', '.metadata.json'), gert::git_status()$file)
    if(length(registry_changes)){
      print_message("Registry sync in '%s' triggered for %s", universe, registry_changes)
    } else {
      dirty <- setdiff(dirty, '.registry') # No effective registry changes
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

cran_recent_updates <- function(days = 1){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  curl::curl_download('https://cloud.r-project.org/web/packages/packages.rds', destfile = tmp)
  cran <- as.data.frame(readRDS(tmp), stringsAsFactors = FALSE)
  cran$age <- difftime(Sys.time(),  as.POSIXct(cran[['Date/Publication']]), units = 'days')
  cran$Package[cran$age < days]
}

bioc_skiplist <- function(release = FALSE){
  yml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  version <- ifelse(release, yml$release_version, yml$devel_version)
  bioc_url <- sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', version)
  bioc <- jsonlite::read_json(bioc_url)
  stopifnot(length(bioc) > 2100)
  submodules <- gert::git_submodule_list()
  bioc <- Filter(function(info){
    if(!length(info$git_last_commit)) {
      message("Missing git_last_commit for ", info$Package)
      return(TRUE)
    }
    current <- submodules[submodules$name == info$Package,]$head
    if(!length(current)) {
      message("Unknown package in metadata ", info$Package)
      return(FALSE)
    }
    !isTRUE(grepl(info$git_last_commit, current, fixed = TRUE))
  }, bioc)
  message(sprintf("Found %d packages out of sync with: %s", length(bioc), bioc_url))
  names(bioc)
}

bioc_recent_updates <- function(days = 14, release = FALSE){
  yml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  version <- ifelse(release, yml$release_version, yml$devel_version)
  bioc <- jsonlite::read_json(sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', version))
  stopifnot(length(bioc) > 2100)
  dates <- as.Date(vapply(bioc, function(x) as.character(x$git_last_commit_date)[1], character(1)))
  names(which(Sys.Date()-dates < days))
}

github_recent_updates <- function(org = 'cran', max = 100){
  repos <- gh::gh("/orgs/{org}/repos", sort='pushed', per_page = max, org = org)
  vapply(repos, function(x) x$name, character(1))
}

make_filter_list <- function(org){
  if(format(Sys.time(), "%a-%H") == 'Sun-00'){
    return(NULL)
  }
  if(org == 'cran'){
    return(unique(c(cran_recent_updates(7), github_recent_updates('cran'))))
  }
  if(org == 'bioc'){
    # TODO: at time of a new bioc release bioc_recent_updates() returns everything
    # However sometimes metacran mirror is stalled for few days.
    return(unique(c(bioc_skiplist(release = FALSE), github_recent_updates('bioc'))))
  }

  if(org == 'bioc-release'){
    return(unique(c(bioc_skiplist(release = TRUE), github_recent_updates('bioc'))))
  }
}

github_last_update <- function(org, hours = 1){
  latest <- gh::gh("/orgs/{org}/repos", org = org, sort = 'pushed', per_page = 1)[[1]]
  pushed <- as.POSIXct(sub("T", " ", latest$pushed_at), tz = 'UTC')
  if(difftime(Sys.time(),  pushed, units = 'hours') < hours){
    return(latest$name)
  }
}
