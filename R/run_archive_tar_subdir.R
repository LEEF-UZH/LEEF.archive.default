#' Compress and checksum the \code{DATA_options("new_data_path")}
#'
#' @return \code{run_archive_tar()}: invisibly returns the name of the archive file
#' @importFrom openssl sha256
#' @importFrom utils tar
#' @importFrom parallel mclapply
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
run_archive_tar_subdir <- function(
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
  tmpdir <- tempfile()
  dir.create( tmpdir)
  ##

  measurements <- list.dirs(  input, recursive = FALSE, full.names = FALSE )
  files <-        list.files( input, recursive = FALSE, full.names = FALSE )
  files <- files[ !(files %in% measurements) ]

# Copy files in input --------------------------------------------------

  file.copy(
    from = file.path(input, files),
    to = tmpdir
  )


# Remove hash files -------------------------------------------------------

  hashfiles <- list.files( tmpdir, pattern = "*.sha256", recursive = FALSE, full.names = TRUE )
  unlink( hashfiles )

# Tar input measurements in input -------------------------------------

  parallel::mclapply(
    measurements,
    function(mes) {
      archivename <- paste(
        basename(input),
        mes,
        "tar",
        sep = "."
      )
      tarfile <- file.path( tmpdir, archivename)
      oldwd <- setwd(input)
      utils::tar(
        tarfile = tarfile,
        files = file.path(".", mes)
      )
      setwd(oldwd)
    }
  )

# Hash files -----------------------------------------------------------

  files <- list.files(
    tmpdir,
    recursive = FALSE,
    full.names = FALSE
  )

  parallel::mclapply(
    files,
    function(fn) {
      f <- file( file.path(tmpdir, fn), open = "rb" )
      hash <- as.character( openssl::sha256( f ) )
      close(f)
      rm(f)
      hashln <- paste(hash, fn, sep = "  ")
      hashfile <- paste0( file.path(tmpdir, fn), ".sha256")
      file.create( hashfile )
      f <- file( hashfile )
      writeLines(
        text = hashln,
        con = f
      )
      close(f)
      rm(f)
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
