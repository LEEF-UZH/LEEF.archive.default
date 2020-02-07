#' Create hash file of input directory and save to file in output directory
#'
#' This function is calculating the \bold{file hash} for each subdirectory and
#' file and storing it in a \code{files.sha256} file and finally calculates the
#' \bold{directory hash} for this file and saves it in the file
#' \code{dir.sh256}. It is calculating one checksum file for the whole dataset
#' as well which is stored in the ToBeImported directory.
#' @param input dircetory of which the hashes should be calculated
#' @param output directory in which the hash files should be written
#'
#' @return invisibly \code{TRUE}
#'
#' @importFrom openssl sha256
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hash_directory(
#'   input = "./input",
#'   output = "./output"
#' )
#' }
hash_directory <- function(
  input,
  output
){

  # Helper function ---------------------------------------------------------

  hash_in_dir <- function(
    dir,
    recursiveHash = FALSE
  ) {
    new_files <- list.files(
      path = dir,
      recursive = recursiveHash
    )
    if (length(new_files) == 0) {
      return()
    }
    new_files <- sort(new_files)

    filehash <- file.path( dir, "file.sha256" )
    dirhash  <- file.path( dir, "dir.sha256" )

    # Hash create files.sha256 ----------------------------------------------

    hash <- lapply(
      new_files,
      function(fn) {
        fnc <- file.path( dir, fn )
        f <- file( fnc, open = "rb" )
        hash <- as.character( openssl::sha256( f ) )
        close(f)
        rm(f)
        hash <- paste(hash, fn, sep = "  ")
      }
    )
    hash <- simplify2array(hash)
    f <- file( file.path( output, filehash ) )
    writeLines(
      text = hash,
      con = f
    )
    close(f)
    rm(f)

    # Create dir.sha256 ---------------------------------------------------

    f <- file(  file.path( dir, "file.sha256"), open = "rb" )
    hash <- as.character( openssl::sha256( f ) )
    close(f)
    rm(f)
    hash <- paste(hash, "file.sha256", sep = "  ")
    f <- file( file.path( output, dirhash ) )
    writeLines(
      text = hash,
      con = f
    )
    close(f)
    rm(f)
  }

# Iterate through subdirectories ------------------------------------------

  dir.create( output, showWarnings = FALSE, recursive = TRUE )
  ##
  dirs <- list.dirs( input, recursive = TRUE )
  hash_in_dir(
    dir = dirs[1],
    recursiveHash = TRUE
  )
  for (d in dirs[-1]) {
    hash_in_dir(
      dir = d,
      recursiveHash = FALSE
    )
  }

# finalize ----------------------------------------------------------------

  invisible(TRUE)

}
