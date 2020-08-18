#' Register the processing of respirometer data in the LEEF package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom LEEF add_pre_processor add_extractor
#' @export
#'
register <- function(compression) {
  if (is.null(system.file(package = "LEEF"))) {
    stop("This function requres the package to be installed!")
  }

  switch(
    compression,
    "none" = LEEF::add_archiver( run_archive.none() ),
    "tar" = LEEF::add_archiver( run_archive.tar() ),
    "tar.gz" = LEEF::add_archiver( run_archive.tar.gz() ),
  )
  ##
  invisible(TRUE)
}

