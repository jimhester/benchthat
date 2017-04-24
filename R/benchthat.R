#' @export
bench_that <- function(code, name = "test", commit = get_commit("HEAD"), filename = timing_file()) {
  time <- tryCatch(system.time(force(code)), error = function(e) {
    message("code exited with error:\n", e$message,
      "\n")
    }, interrupt = function(e) {
      message("interrupt received.")
    })
  write_timing(filename, name, commit, time)
}

#' @importFrom glue collapse glue
write_timing <- function(filename, name, commit, time) {
  unless(file.exists(filename), {
    dir <- dirname(filename)
    unless(dir.exists(dir), {
      dir.create(dir, recursive = TRUE)
    })
    cat(file = filename, paste0(collapse(c("commit", "commit_date", "benchmark", "user", "system", "total", "child_user", "child_system"), sep = "\t"), "\n"))
  })
  cat(file = filename, paste0(commit@sha, "\t", commit_date(commit), "\t", name, "\t", collapse(time, sep = "\t"), "\n"), append = TRUE)
}

unless <- function(x, y) {
  if (!isTRUE(x)) {
    return(y)
  }
  invisible(FALSE)
}

#' @export
bench_prof <- function(code, name = "test", filename = prof_file(), interval = 0.01) {
  gc()
  Rprof(
    filename,
    interval = interval,
    line.profiling = TRUE,
    gc.profiling = TRUE,
    memory.profiling = TRUE)

  tryCatch(force(code), error = function(e) {
    message("code exited with error:\n", e$message,
      "\n")
    }, interrupt = function(e) {
      message("interrupt received.")
    })
  Rprof(NULL)
}

get_commit <- function(sha = "HEAD", path = ".") {
  r <- git2r::repository(project_path(path))
  if (sha == "HEAD") {
    ref <- git2r::head(r)
    if (is(ref, "git_commit")) {
      return(ref)
    }
    git2r::lookup(r, git2r::branch_target(ref))
  } else {
    git2r::lookup(r, sha)
  }
}

commit_date <- function(commit) {
  as(commit@author@when, "character")
}

project_path <- function(path) {
  rprojroot::find_root(rprojroot::is_git_root, path)
}

timing_file <- function(path = ".") {
  root <- project_path(path)
  file.path(root, "docs", "benchmarks", "timing.tsv")
}

process_timings <- function(filename = timing_file()) {
  library(tidyverse)

  x <- read_tsv(filename, col_types = "ccnnnn")

  x %>%
    group_by(commit, benchmark) %>%
    summarise(user = mean(user), system = mean(system), total = mean(total), child_user = mean(child_user), child_system = mean(child_system), n = n())
  f <- list(x$benchmark, x$commit)

  numeric_means <- function(xx) {
    is_numeric <- vlapply(xx, is.numeric)
    xx[is_numeric] <- colMeans(xx[is_numeric])
    xx
  }
  browser()

  unsplit(lapply(split(x, f), numeric_means), f)
}

vlapply <- function(x, f, ...) vapply(x, f, logical(1), ...)
