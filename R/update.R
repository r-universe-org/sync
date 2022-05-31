#' @export
#' @rdname sync
update_submodules <- function(path = '.', skip = '.registry'){
  submodules <- gert::git_submodule_list(repo = path)
  submodules$upstream <- remote_heads_many(submodules$url)
  for(i in seq_len(nrow(submodules))){
    info <- as.list(submodules[i,])
    if(info$path %in% skip) next
    if(info$upstream == info$head){
      print_message("Submodule '%s' is up-to-date", info$path)
    } else if(is.na(info$upstream) || !nchar(info$upstream)){
      print("Failed to get upstream commit for '%s' (repo deleted?)", info$path)
    } else {
      print_message("Updating submodule '%s' to %s", info$path, info$upstream)
      gert::git_submodule_set_to(info$path, info$upstream, checkout = FALSE, repo = path)
    }
  }
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
  stopifnot(utils::tail(txt, 1) == '0000')
  refs <- tail(head(txt, -1), -1)
  refs[1] <- sub("^0000", "", refs[1])
  substring(refs, 5)
}

remote_heads_many <- function(repos, verbose = FALSE){
  pool <- curl::new_pool()
  len <- length(repos)
  out <- character(len)
  completed <- 0
  lapply(seq_len(len), function(i){
    k <- i
    url <- sprintf('%s/info/refs?service=git-upload-pack', repos[i])
    h <- curl::new_handle(useragent = 'git/2.35.1.windows.2', failonerror = TRUE)
    curl::curl_fetch_multi(url, handle = h, done = function(res){
      txt <- parse_raw_gitpack(res$content)
      head <- grep("HEAD$", txt, value = TRUE)
      out[k] <<- ifelse(length(head), sub(" HEAD", "", head, fixed = TRUE), NA_character_)
      if(verbose) {
        completed <<- completed + 1
        cat(sprintf("\r %d/%d", as.integer(completed), as.integer(len)))
      }
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  out
}
