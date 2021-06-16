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

  archivename <- options()$LEEF$name
  archivedir <- "."
  if (is.null(archivename)) {
    archivename <- "none"
  }
  if (file.exists(file.path(input, "sample_metadata.yml"))) {
	  timestamp <- yaml::yaml.load_file( file.path(input, "sample_metadata.yml") )$timestamp
	  archivedir <- basename(input)
  } else if (file.exists(file.path(input, "..", "00.general.parameter", "sample_metadata.yml"))) {
	  timestamp <- yaml::yaml.load_file( file.path(input, "..", "00.general.parameter", "sample_metadata.yml") )$timestamp
	  archivedir <- basename(input)
  } else {
	  timestamp <- yaml::yaml.load_file( file.path(input, "..", "..", "00.general.parameter", "sample_metadata.yml") )$timestamp
	  archivedir <- paste0(basename(dirname(input)), ".", basename(input))
	}

  ##



  archivename <- paste(
    archivename,
    archivedir,
    timestamp,
    sep = "."
  )
  archivedir <- file.path( output, archivename )

  unlink( archivedir, recursive = TRUE, force = TRUE )
  dir.create( archivedir, showWarnings = FALSE, recursive = TRUE)

  ##

  file.copy(
    from = file.path( input, "." ),
    to = archivedir,
    recursive = TRUE,
    copy.date = TRUE
  )

  ##

  dir.create(file.path(archivedir, "00.general.parameter"))
  if (file.exists(file.path(input, "..", "00.general.parameter", "sample_metadata.yml"))) {
    file.copy(
      from = file.path(input, "..", "00.general.parameter", "."),
      to = file.path(archivedir, "00.general.parameter", ""),
      recursive = TRUE,
      copy.date = TRUE
    )
  } else {
    file.copy(
      from = file.path(input, "..", "..", "00.general.parameter", "."),
      to = file.path(archivedir, "00.general.parameter", ""),
      recursive = TRUE,
      copy.date = TRUE
    )
  }



  ##

  hash_directory(
    input = archivedir,
    output = archivedir
  )

  ##
  invisible(normalizePath(archivedir))
}
