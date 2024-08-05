#' @export
#' @rdname sync
update_local <- function(path = '.'){
  update_registry(path = path)
  update_submodules(path = path)
}

#' @export
#' @rdname sync
update_remote <- function(url){
  #libgit2 is super slow with submodules
  #path <- gert::git_clone(url)
  git_cmd('clone', url)
  withr::local_dir(basename(url))
  update_workflows('cran')
  update_local()
}

#' @export
#' @rdname sync
update_submodules <- function(path = '.', skip = '.registry'){
  withr::local_dir(path)
  repo <- gert::git_open(path)
  submodules <- gert::git_submodule_list(repo = repo)
  submodules$upstream <- remote_heads_many(submodules$url, submodules$branch)
  for(i in seq_len(nrow(submodules))){
    info <- as.list(submodules[i,])
    if(info$path %in% skip) next
    if(is.na(info$upstream) || !nchar(info$upstream)){
      print_message("Failed to get upstream commit for '%s' (repo deleted?)", info$path)
    } else if(info$upstream == info$head){
      #print_message("Submodule '%s' is up-to-date", info$path)
    } else  {
      print_message("Updating submodule '%s' to %s", info$path, info$upstream)
      tryCatch(update_and_push(info), error = function(e){
        cat(conditionMessage(e), '\n', file = stderr())
        git_cmd('submodule', 'update', '-f', '--init', info$path)
      })
    }
  }
}

#' @export
#' @rdname sync
submodules_up_to_date <- function(skip_broken = TRUE, path = '.'){
  withr::local_dir(path)
  repo <- gert::git_open(path)
  submodules <- gert::git_submodule_list(repo = repo)
  submodules$upstream <- remote_heads_many(submodules$url, submodules$branch)
  isok <- which(submodules$upstream == submodules$head)
  fine <- submodules$path[isok]
  broken <- submodules[is.na(submodules$upstream),]
  for(i in seq_len(nrow(broken))){
    module <- as.list(broken[i,])
    ishash <- grepl('^[0-9a-f]{6,100}$', tolower(module$branch))
    if(ishash){
      if(grepl(tolower(module$branch), tolower(module$head), fixed = TRUE)){
        print_message("Found up-to-date raw hash: for %s@%s", module$url, module$branch)
        fine <- c(fine, module$path)
      } else {
        # TODO: should we normalize/validate the commit hash here?
        print_message("Assuming raw hash: for %s@%s", module$url, module$branch)
      }
    } else {
      print_message("Failed to get upstream status for %s@%s", module$url, module$branch)
      if(skip_broken){
        fine <- c(fine, module$path) # no point in updating missing branch/pkg?
      }
    }
  }
  return(fine)
}

update_and_push <- function(info){
  pkg_dir <- info$path
  gert::git_submodule_set_to(pkg_dir, info$upstream, checkout = FALSE, repo = dirname(pkg_dir))
  git_cmd('submodule', 'update', '--init', '--recommend-shallow', pkg_dir)
  subdir <- get_module_config(pkg_dir, 'subdir')
  r_pkg_dir <- ifelse(length(subdir), file.path(pkg_dir, subdir), pkg_dir)
  desc <- get_description_data(r_pkg_dir)
  if(!identical(desc$package, pkg_dir)){
    git_cmd('submodule', 'update', '-f', '--init', pkg_dir)
    stop(sprintf("Package '%s' from registry does not match package name in description file: '%s'", pkg_dir, desc$package))
  }
  subrepo <- gert::git_open(pkg_dir)
  stopifnot(basename(gert::git_info(repo = subrepo)$path) == pkg_dir)
  update_remotes_json(desc)
  git_cmd('add', '.remotes.json')
  #todo: update_gitmodules() here
  pkg_commit <- gert::git_log(repo = subrepo, max = 1)
  person <- utils::as.person(desc$maintainer)[1]
  person$email <- normalize_email(person$email)
  sig <- format(person, include = c("given", "family", "email"))
  validate_signature(sig) # validates email syntax from description
  sig <- paste(sig, unclass(pkg_commit$time)) # add timestamp
  gert::git_commit(message = paste(desc$package, desc$version), author = sig)
  gert::git_push(verbose = TRUE)
  sys::exec_wait("git", c("submodule", "deinit", pkg_dir), std_out = FALSE)
  unlink(file.path('.git/modules', pkg_dir), recursive = TRUE, force = TRUE)
}

# Spec: https://www.git-scm.com/docs/http-protocol#_discovering_references
raw_remote_references <- function(repo){
  url <- sprintf('%s/info/refs?service=git-upload-pack', repo)
  h <- curl::new_handle(useragent = 'git/2.35.1.windows.2', failonerror = TRUE)
  req <- curl::curl_fetch_memory(url, handle = h)
  parse_raw_gitpack(req$content)
}

parse_raw_gitpack <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  txt <- readLines(con, warn = FALSE)
  stopifnot(grepl('^[0-9a-f]{4}#', txt[1]))
  stopifnot(grepl('service=', txt[1]))
  if(length(txt) == 2 && txt[2] == '00000000'){
    return(NULL) #empty repo
  }
  stopifnot(utils::tail(txt, 1) == '0000')
  refs <- utils::head(txt, -1)
  if(grepl("git-upload-pack0000", txt[1])){
    # bitbucket.org seems to omit LF after 1st line
    refs[1] <- sub('.*git-upload-pack', "", refs[1])
  } else {
    refs <- utils::tail(refs, -1)
  }
  refs[1] <- sub("^0000", "", refs[1])
  substring(refs, 5)
}

remote_heads_many <- function(repos, refs = NULL, verbose = TRUE){
  pool <- curl::multi_set(multiplex = FALSE) # use default pool
  len <- length(repos)
  out <- rep(NA_character_, len)
  completed <- 0
  lapply(seq_len(len), function(i){
    k <- i
    url <- sprintf('%s/info/refs?service=git-upload-pack', repos[i])
    ref <- ifelse(length(refs) && !is.na(refs[i]), refs[i], "HEAD")
    h <- curl::new_handle(useragent = 'git/2.35.1.windows.2', failonerror = TRUE)
    curl::curl_fetch_multi(url, handle = h, done = function(res){
      txt <- tryCatch(parse_raw_gitpack(res$content), error = function(...){})
      if(!length(txt)){
        message("Failed to get HEAD ref: ", repos[i])
        return()
      }
      pattern <- ifelse(ref=='HEAD', 'HEAD$', sprintf("\\/%s$", ref))
      match <- grep(pattern, txt, value = TRUE)
      out[k] <<- ifelse(length(match), sub(" .*$", "", match), NA_character_)

      # In case of annotated tags, we actually need the dereferenced ^{} value
      if(!identical(ref, 'HEAD')){
        match <- grep(sprintf('refs/tags/%s^{}', ref), txt, fixed = TRUE, value = TRUE)
        if(length(match)){
          out[k] <<- sub(" .*$", "", match)
        }
      }

      if(verbose) {
        completed <<- completed + 1
        if((len-completed) %% 100 == 0)
          cat(sprintf("\rScanning for changes... %d/%d", as.integer(completed), as.integer(len)), file = stderr())
      }
    }, fail = message, pool = pool)
  })
  curl::multi_run(pool = pool)
  cat("\n", file = stderr())
  out
}
