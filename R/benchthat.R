#' @export
bench_that <- function(code, name = "test", commit = current_commit(), filename = timing_file()) {
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
    cat(file = filename, paste0(collapse(c("commit", "benchmark", "user", "system", "total", "child_user", "child_system"), sep = "\t"), "\n"))
  })
  cat(file = filename, paste0(glue(commit, name, benchmark, collapse(time, sep = "\t"), sep = "\t"), "\n"), append = TRUE)
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

current_commit <- function() {
  tryCatch({
    r <- git2r::repository(".")
    h <- git2r::lookup(r, git2r::branch_target(head(r)))
    h@sha
  }, error = function(e) "initial")
}

timing_file <- function(path = ".") {
  root <- rprojroot::find_package_root(path)
  file.path(root, "docs", "benchmarks", "timing.tsv")
}
