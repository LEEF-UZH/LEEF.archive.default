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
#' run_archive.tar(
#'   input = "./input",
#'   output = "./output"
#' )
#' }
run_archive_tar <- function(
  input,
  output
){
  oldwd <- getwd()
  on.exit(
    setwd(oldwd)
  )
  ##
  input <- run_archive_none( input = input, output = tempfile() )
  ##
  archivename <- paste(
    basename(input),
    "tar",
    sep = "."
  )
  tmpdir <- tempfile()
  dir.create( tmpdir)
  tarfile <- file.path( tmpdir, archivename)

  ##

# Tar input directory -----------------------------------------------------

  oldwd <- setwd(dirname(input))
  utils::tar(
    tarfile = tarfile,
    files = "./"
  )
  setwd(oldwd)

# Hash tar file -----------------------------------------------------------

  f <- file( tarfile, open = "rb" )
  hash <- as.character( openssl::sha256( f ) )
  close(f)
  rm(f)
  hashln <- paste(hash, archivename, sep = "  ")
  hashfile <- paste0(tarfile, ".sha256")
  file.create(hashfile)
  f <- file( hashfile )
  writeLines(
    text = hashln,
    con = f
  )
  close(f)
  rm(f)

# Copy to output ----------------------------------------------------------

  dir.create( path = output, showWarnings = FALSE, recursive = TRUE )
  ##
  file.copy(
    from = c(tarfile, hashfile),
    to = output,
    copy.date = TRUE
  )

  unlink(dirname(tarfile), recursive = TRUE)

# Return ------------------------------------------------------------------

  invisible( file.path( output, archivename ) )
}
