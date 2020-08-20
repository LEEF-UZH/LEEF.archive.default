#' Archive all files and folders in \code{input} to \code{output}
#'
#' These functions archive the \code{input} folder, create checksums, and, depending on the function, compress the archive.
#'
#' @param input directory which to archive
#' @param output directory in which the archive will be created
#'
#' @return \code{run_archive_none()}: invisibly returns the name of the archive directory
#'
#' @rdname run_archive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_archive_none(
#'   input = "./input",
#'   output = "./output"
#' )
#' }
run_archive_none <- function(
  input,
  output
){
  timestamp <- format( Sys.time() , "%Y-%m-%d--%H-%M-%S" )
  ##

  archivename <- options()$LEEF$archive$name
  if (is.null(archivename)) {
    archivename <- "none"
  }
  archivename <- paste(
    archivename,
    timestamp,
    sep = "."
  )
  archivedir <- file.path( output, archivename )
  ##
  hashdir <- tempfile()
  hash_directory(
    input = input,
    output = hashdir
  )
  ##

  dir.create( path = archivedir, showWarnings = FALSE, recursive = TRUE )
  ##
  file.copy(
    from = file.path( input, "." ),
    to = archivedir,
    recursive = TRUE,
    copy.date = TRUE
  )
  ##
  file.copy(
    from = file.path( hashdir, "." ),
    to = archivedir,
    recursive = TRUE,
    copy.date = TRUE
  )
  unlink(hashdir, recursive = TRUE)
  ##
  invisible(normalizePath(archivedir))
}
