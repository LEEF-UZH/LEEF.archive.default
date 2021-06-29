#' Compress and checksum the \code{DATA_options("new_data_path")}
#'
#' @return \code{run_archive_tar()}: invisibly returns the name of the archive file
#' @importFrom openssl sha256
#' @importFrom utils tar
#'
#' @rdname run_archive
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_archive_none_subdir(
#'   input = "./input",
#'   output = "./output"
#' )
#' }
run_archive_none_subdir <- function(
  input,
  output
){
  oldwd <- getwd()
  on.exit(
    setwd(oldwd)
  )
  ##
  tmpdir <- file.path(output, "tmp_none_subdir")
  dir.create( tmpdir, recursive = TRUE, showWarnings = FALSE)
  ##

  measurements <- list.dirs(  input, recursive = FALSE, full.names = TRUE )

  # archive_none -------------------------------------

  lapply(
    measurements,
    function(mes) {
      run_archive_none(mes, tmpdir)
    }
  )

  # Copy to output ----------------------------------------------------------

  dir.create( path = output, showWarnings = FALSE, recursive = TRUE )
  ##
  file.copy(
    from = file.path(tmpdir, "."),
    to = output,
    recursive = TRUE,
    copy.date = TRUE
  )

  unlink( tmpdir, recursive = TRUE )

  # Return ------------------------------------------------------------------

  invisible( output )
}
