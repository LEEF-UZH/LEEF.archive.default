#' Register the processing of o2meter data in the LEEF package
#'
#' @param compression the compression algorithm to be used. This can be:
#'   - *none*: no compression, multiple files (copy of directory to be archived) - the fastest
#'   - *tar*: no compression, single file - intermediate speed
#'   - *tar.gz.max*: gz compression level 9 - very slow
#' @return invisibly \code{TRUE} when completed successful
#'
#' @md
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
    "tar_subdir" = LEEF::add_archiver( run_archive_tar_subdir ),
    "tar.gz_subdir" = LEEF::add_archiver( run_archive_tar.gz_subdir ),
    stop("not a valid compression!\n", "Allowed values are 'none', 'tar', und 'tar.gz.max'.")
  )

  ##
  invisible(TRUE)
}

