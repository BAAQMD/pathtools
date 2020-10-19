#' here_path
#'
#' Returns a path relative to `here::here()`, and creates it if it does not yet exist.
#'
#'     Also applies `glue::glue()` to the path (after `here::here()`).
#'
#' @param ... passed to [here::here()]
#' @param ext file extension
#'
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom xfun with_ext
#'
#' @return
#' @export
here_path <- function (..., ext = NULL) {
  path <- as.character(glue::glue(here::here(...)))
  if (is.character(ext)) {
    path <- xfun::with_ext(path, ext)
  }
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  return(path)
}
