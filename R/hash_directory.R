#' Create hash file of input directory and save to file in output directory
#'
#' This function is calculating the \bold{file hash} for each subdirectory and
#' file and storing it in a \code{files.sha256} file and finally calculates the
#' \bold{directory hash} for this file and saves it in the file
#' \code{dir.sh256}. It is calculating one checksum file for the whole dataset
#' as well which is stored in the ToBeImported directory.
#' @param input dircetory of which the hashes should be calculated
#' @param hashfile File name of the hash file. Defaults to \code{file.sha256} in the input directory
#'
#' @return invisibly \code{TRUE}
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hash_directory(
#'   input = "./input",
#'   hashfile = file.path(input, "file.hash")
#' )
#' }
hash_directory <- function(
  input,
  hashfile = file.path(input, "file.hash")
){
  dir.create(basename(hashfile), recursive = TRUE, showWarnings = FALSE)
  tmp_hash <-  .hash_recursively(root = input)
  file.copy(
    from = tmp_hash,
    to = hashfile,
    overwrite = TRUE
  )
  unlink(tmp_hash)
  ##
  invisible(TRUE)
}


#' calculate sha256 hashes recursively for all files in directory
#'
#' @param root root of the directory tree to be hashed
#' @param output directory containing the hash files in the tree structure
#'
#' @return file name and path of the hash file \code{file.sha.256}
#'
#' @importFrom openssl sha256
#'
.hash_recursively <- function(
  root,
  hashfile = tempfile(pattern = "file.sha256.")
) {
  files <- list.files(
    path = root,
    recursive = TRUE,
    full.names = FALSE
  )
  if (length(files) > 0) {
    hashes <- sapply(
      files,
      function(fn) {
        f <- file(
          file.path(root, fn),
          open = "rb"
        )
        hash <- as.character( openssl::sha256( f ) )
        close(f)
        rm(f)
        hash <- paste(hash, fn, sep = "  ")
        return(hash)
      }
    )
    writeLines(hashes, hashfile)
  } else {
    hashfile <- NULL
  }
  return(hashfile)
}


#' #' get timestamp for file when asked for
#' #'
#' #' @param file file name
#' #'
#' #' @return the content of the response
#' .get_tts <- function(file) {
#'
#'   if( !isTRUE(getOption("LEEF")$tts$create) ){
#'     invisible(NULL)
#'   }
#'
#'   result <- ROriginStamp::create_timestamp(
#'     hash = ROriginStamp::hash_file( file ),
#'     comment = paste(getOption("LEEF")$name, ),
#'     notifications = data.frame(
#'       currency = 0,
#'       notification_type = getOption("LEEF")$tts$notofication$notification_type,
#'       target = getOption("LEEF")$tts$notofication$target
#'     )
#'   )
#'
#'   return(result$content)
#'
#' }
