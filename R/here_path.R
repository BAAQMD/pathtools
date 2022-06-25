#' glue_path
#'
#' Applies `glue::glue()` to the path(s), and creates any nonexistent directories.
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
glue_path <- function (..., ext = NULL, verbose = getOption("verbose", default = FALSE)) {
  msg <- function (...) if(isTRUE(verbose)) message("[here_path] ", ...)
  parts <- map_chr(list(...), glue::glue)
  paths <- rlang::exec(file.path, !!!parts)
  paths <- as.character(paths)
  if (is.character(ext)) {
    paths <- xfun::with_ext(paths, ext)
  }
  for (dn in dirname(paths)) {
    if (isFALSE(dir.exists(dn))) {
      msg("creating directory: ", dn)
      dir.create(dn, recursive = TRUE)
    }
  }
  return(paths)
}

#' @export
here_path <- function (...) {
  here::here(glue_path(...))
}
