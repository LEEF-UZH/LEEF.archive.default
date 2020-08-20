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
  # dir.create( output, showWarnings = FALSE, recursive = TRUE )
  ##
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  ##
  .hash_dir_tree(root = input, output = tmpdir)
  ##
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = file.path(tmpdir, "."),
    to = output,
    recursive = TRUE,
    copy.date = TRUE
  )
  unlink(tmpdir)
  ##
  invisible(TRUE)

}



#' Create hash of all files in the input directory and stores result in a file
#' named \code{file.sha25} in the \code{output} directory
#'
#' @param input directory to hash
#' @param output where hash file should be stored
#' @param recursive if \code{TRUE}, directory will be recursively scanned for files to hash
#'
#' @return invisibly the file name and path of the hash file \code{file.sha.256}
#'
.hash_dir <- function(
  input,
  output,
  recursive = FALSE
) {
  filehash <- file.path( output, "file.sha256" )

  files <- setdiff(
    list.files(
      path = input,
      full.names = recursive,
      include.dirs = FALSE,
      recursive = recursive
      ),
    list.dirs(
      path = input,
      full.names = recursive,
      recursive = recursive
    )
  )

  if (length(files) == 0) {
    return()
  }
  files <- sort(files)

  # Hash create files.sha256 ----------------------------------------------

  hash <- lapply(
    files,
    function(fn) {
      f <- file(
        ifelse(
          recursive,
          fn,
          file.path(input, fn)
        ),
        open = "rb"
      )
      hash <- as.character( openssl::sha256( f ) )
      close(f)
      rm(f)
      hash <- paste(hash, fn, sep = "  ")
    }
  )
  hash <- simplify2array(hash)
  dir.create( output, recursive = TRUE, showWarnings = FALSE )
  file.create(filehash)
  f <- file( filehash )
  writeLines(
    text = hash,
    con = f
  )
  close(f)
  rm(f)

  invisible( filehash )
}


#' hash a directory tree
#'
#' @param root root of the directory tree to be hashed
#' @param output directory containing the hash files in the tree structure
#'
#' @return invisibly the file name and path of the hash file \code{tree.sha.256}
#'
.hash_dir_tree <- function(
  root,
  output
) {
  dirs <- list.dirs(
    path = root,
    full.names = TRUE,
    recursive = TRUE
  )
  treehash <- file.path( output, "tree.sha256" )

  filehashes <- sapply(
    dirs,
    function(dir) {
      .hash_dir(
        input = dir,
        output = file.path(output, gsub(root, "", dir))
      )
    }
  )

  filehashes <- unlist(filehashes)

  hash <- lapply(
    filehashes,
    function(fn) {
      f <- file( fn, open = "rb" )
      hash <- as.character( openssl::sha256( f ) )
      close(f)
      rm(f)
      hash <- paste(hash, gsub(output, ".", fn), sep = "  ")
    }
  )
  hash <- simplify2array(hash)
  dir.create( output, recursive = TRUE, showWarnings = FALSE )
  file.create(treehash)
  f <- file( treehash )
  writeLines(
    text = hash,
    con = f
  )
  close(f)
  rm(f)


  invisible(treehash)

}

