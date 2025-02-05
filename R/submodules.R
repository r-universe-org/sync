#' @export
#' @rdname sync
submodules_up_to_date <- function(skip_broken = TRUE, filter_packages = NULL, path = '.'){
  withr::local_dir(path)
  repo <- gert::git_open(path)
  submodules <- gert::git_submodule_list(repo = repo)
  skiplist <- NULL
  if(length(filter_packages)){
    do_check <- submodules$path %in% filter_packages
    skiplist <- submodules$path[!do_check]
    submodules <- submodules[do_check,]
    print_message("Skipping check for %d packages without recent activity", length(skiplist))
  }
  submodules$upstream <- remote_heads_in_batches(submodules$url, submodules$branch)
  isok <- which(submodules$upstream == submodules$head)
  fine <- c(submodules$path[isok], skiplist)
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
  pool <- curl::multi_set(multiplex = TRUE, host_con = 1L, max_streams = 2) # Use default pool
  len <- length(repos)
  out <- rep(NA_character_, len)
  completed <- 0
  lapply(seq_len(len), function(i){
    k <- i
    url <- sprintf('%s/info/refs?service=git-upload-pack', repos[i])
    ref <- ifelse(length(refs) && !is.na(refs[i]), refs[i], "HEAD")
    h <- make_handle(url)
    curl::multi_add(handle = h, done = function(res){
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
    }, fail = function(err){
      message(sprintf("Failure for %s: %s", url, err))
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  cat("\n", file = stderr())
  out
}

# GitHub does not like too many requests at once so we wait a bit
remote_heads_in_batches <- function(repos, refs){
  ngroups <- ceiling(length(repos)/500)
  batch <- sample(seq_len(ngroups), length(repos), replace = TRUE)
  output <- rep(NA, length(repos))
  for(group in seq_len(ngroups)) {
    sx <- batch == group
    if(group > 1) {
      message("Done! Waiting for a bit for the next batch...")
      Sys.sleep(300)
    }
    message(sprintf("Starting batch %d of %d...", group, ngroups))
    output[sx] <- remote_heads_many(repos[sx], refs[sx])
  }
  return(output)
}

make_handle <- function(desc_url, failonerror = TRUE){
  handle <- curl::new_handle(url = desc_url, failonerror = failonerror, useragent = 'git/2.35.1.windows.2')
  if(grepl('github.com', desc_url, fixed = TRUE)){
    token <- Sys.getenv('GITHUB_TOKEN')
    if(nchar(token)){
      curl::handle_setheaders(handle, Authorization = paste('Bearer', token))
    }
  }
  handle
}

