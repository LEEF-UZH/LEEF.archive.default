#' Archive all files and folders in \code{input} to \code{output} without compression
#'
#'
#' @param input directory which to archive
#' @param output directory in which the arcival directory will be created
#'
#' @return invisibly returns the name of the archivedir
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
