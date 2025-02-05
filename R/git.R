git_cmd <- function(..., std_err = TRUE, timeout = 600){
  # Timeout is mostly in case of unexpected password prompts
  args <- c(...)
  cat("git", args, '\n', file = stderr())
  sys::exec_wait('git', args, std_err = std_err, timeout = timeout)
}

git_cmd_assert <- function(..., timeout = 600){
  args <- c(...)
  cat("git", args, '\n', file = stderr())
  res <- sys::exec_internal('git', args = args, timeout = timeout, error = FALSE)
  errtxt <- sys::as_text(res$stderr)
  lapply(errtxt, cat, file = stderr(), "\n")
  if(!identical(res$status, 0L)){
    fatal_error <- grep('fatal', errtxt, value = TRUE)
    if(length(fatal_error)){
      errtxt <- sub("fatal:", "", fatal_error[1])
    } else {
    }
    stop(sprintf('git %s: %s', args[1], paste(errtxt, collapse = "\n")))
  }
  return(res)
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
    if(!git_cmd("submodule", "update", "--init", "--remote", "-f", dest)){
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
      return(eval.parent(cl))
    }, error = function(err){
      if(i < times){
        message(sprintf("Error '%s' in %s: Retrying...", err$message, deparse(cl)))
        Sys.sleep(wait)
      } else {
        err$message <- sprintf("%s. (tried %d times, giving up)", err$message, times)
        err$call <- cl
        stop(err)
      }
    })
  }
}
