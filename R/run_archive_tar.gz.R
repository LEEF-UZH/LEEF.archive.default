#' Compress and checksum the \code{DATA_options("new_data_path")}
#'
#' Compress all files in \code{DATA_options("new_data_path")} into the directory \code{"NEW_DATA_PATH/../archive"}
#'
#' @param compression which compression should be used for the archive. Thge following values are supported at the moment:
#' \describe{
#'    \item{\code{"none"}}{copy the to be imported data folder into archive}
#'    \item{\code{"tar"}}{tar the to be imported data folder}
#'    \item{\code{"tar.gz"}}{tar and gz compress the to be imported data folder}
#' }
#' Default is the value as defined in the config file.
#'
#' @param get_tts if \code{TRUE}, a Trusted Time Stamp will be obtained from OriginStamp
#'
#' @return invisibly returns the name of the archivefile
#' @importFrom openssl sha256
#' @importFrom utils tar
#' @importFrom yaml write_yaml
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_archive(
#'   compression = "tar",
#'   get_tts = TRUE
#' )
#' }
run_archive_tar.gz <- function(
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
    "tar.gz",
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
    files = "./",
    compression = "gz",
    compression_level = 9
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
