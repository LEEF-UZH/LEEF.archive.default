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
#' run_archive.none(
#'   input = "./input",
#'   output = "./output"
#' )
#' }
run_archive <- function(
  input,
  output
){
  hashdir <- tmpfile()
  hash_directory(
    input = archivedir,
    output = hashdir
  )
  ##
  timestamp <- format( Sys.time() , "%Y-%m-%d--%H-%M-%S" )
  archivename <- paste(
    options()$LEEF$archive$name,
    timestamp,
    sep = "."
  )
  archivedir <- file.path( output, archivename )
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
  invisible(archivedir)
}
