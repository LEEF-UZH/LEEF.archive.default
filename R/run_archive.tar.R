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
run_archive <- function(
  input,
  output
){
  oldwd <- getwd()
  on.exit(
    setwd(oldwd)
  )
  ##
  input <- run_archive.none( input = input, output = tmpfile() )
  ##
  archivename <- paste(
    input,
    "tar",
    sep = "."
  )
  archivefile <- file.path( tmpfile(), archivename)
  ##

# Tar input directory -----------------------------------------------------

  oldwd <- setwd(input)
  utils::tar(
    tarfile = archivefile,
    files = "./"
  )
  setwd(oldwd)

# Hash tar file -----------------------------------------------------------

  f <- file( archivefile, open = "rb" )
  hash <- as.character( openssl::sha256( f ) )
  close(f)
  rm(f)
  hashln <- paste(hash, archivename, sep = "  ")
  hashfile <- paste0(archivefile, ".sha256")
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
    from = c(archivefile, hashfile),
    to = output,
    copy.date = TRUE
  )

  unlink(dirname(archivefile), recursive = TRUE)

# Return ------------------------------------------------------------------

  invisible( file.path( output, archivename ) )
}
