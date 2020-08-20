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
#' @importFrom yaml yaml.load_file
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

  smdf <- file.path(input, "sample_metadata.yml")
  smd <- yaml::yaml.load_file( smdf )

  timestamp <- smd$timestamp

  ##

  archivename <- options()$LEEF$name
  if (is.null(archivename)) {
    archivename <- "none"
  }
  archivename <- paste(
    archivename,
    timestamp,
    sep = "."
  )
  archivedir <- file.path( output, archivename )

  unlink( archivedir, recursive = TRUE, force = TRUE )
  dir.create( archivedir, showWarnings = FALSE, recursive = TRUE)

  ##
  hashdir <- tempfile()
  hash_directory(
    input = input,
    output = hashdir
  )
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
