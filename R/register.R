#' Register the processing of respirometer data in the LEEF package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom LEEF add_archiver
#' @export
#'
register <- function(compression) {
  if (is.null(system.file(package = "LEEF"))) {
    stop("This function requres the package to be installed!")
  }

  switch(
    compression,
    "none"   = LEEF::add_archiver( run_archive_none ),
    "tar"    = LEEF::add_archiver( run_archive_tar ),
    "tar.gz" = LEEF::add_archiver( run_archive_tar.gz ),
    stop("not a valid compression!\n", "Allowed values are 'none', 'tar', und 'tar.gz'.")
  )

  ##
  invisible(TRUE)
}

