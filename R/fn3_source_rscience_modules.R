#' Source Science Modules
#' @export
fn3_source_rscience_modules <- function(paths, pattern = "^module_.*\\.R$") {
  # Get full file paths
  files <- list.files(path = paths, pattern = pattern, full.names = TRUE, recursive = TRUE)

  if (length(files) == 0) {
    warning("No modules found in: ", paths)
  }

  # Source each file
  for (f in files) {
    source(f, local = FALSE)
  }
}
