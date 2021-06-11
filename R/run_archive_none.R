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

	if (file.exists(file.path(input, "sample_metadata.yml"))) {
	  timestamp <- yaml::yaml.load_file( file.path(input, "sample_metadata.yml") )$timestamp
	} else {
	  timestamp <- yaml::yaml.load_file( file.path(input, "..", "00.general.parameter", "sample_metadata.yml") )$timestamp
	}

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
  hashdir <- file.path(output, "tmp")

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
  dir.create(file.path(archivedir, "00.general.parameter"))
  file.copy(
    from = file.path(input, "..", "00.general.parameter", "."),
    to = file.path(archivedir, "00.general.parameter", ""),
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
