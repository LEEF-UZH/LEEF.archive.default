#' Register the processing of respirometer data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom LEEF.Data add_pre_processor add_extractor
#' @export
#'
register <- function(compression) {
  switch(
    compression,
    "none" = LEEF.Data::add_archiver( run_archive.none() ),
    "tar" = LEEF.Data::add_archiver( run_archive.tar() ),
    "tar.gz" = LEEF.Data::add_archiver( run_archive.tar.gz() ),
  )
  ##
  invisible(TRUE)
}

