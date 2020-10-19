

#' @title Download test PCX files for the package.
#'
#' @return invisible list of files, usually ignored. This function is called for the side effect of downloading the data.
#'
#' @export
download_test_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("pcx");

  md5_info = read_md5sum_file(system.file('extdata', 'md5sum_libav_pcx', package = 'pcx', mustWork = TRUE));
  base_url= 'https://samples.libav.org/image-samples/pcx/';
  urls = paste(base_url, md5_info$filename, sep='');

  cfiles = pkgfilecache::ensure_files_available(pkg_info, md5_info$filename, urls, md5sums=md5_info$md5sum);
  cfiles$file_status = NULL; # not exposed to end user
  return(invisible(cfiles));
}


#' @title Get file names available in package cache.
#'
#' @description Get file names of optional data files which are available in the local package cache. You can access these files with get_optional_data_file().
#'
#' @return vector of strings. The file names available, relative to the package cache.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info list_available
list_opt_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("pcx");
  return(pkgfilecache::list_available(pkg_info));
}


#' @title Access a single file from the package cache by its file name.
#'
#' @param filename, string. The filename of the file in the package cache.
#'
#' @param mustWork, logical. Whether an error should be created if the file does not exist. If mustWork=FALSE and the file does not exist, the empty string is returned.
#'
#' @return string. The full path to the file in the package cache or the empty string if there is no such file available. Use this in your application code to open the file.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info get_filepath
get_opt_data_filepath <- function(filename, mustWork=TRUE) {
  pkg_info = pkgfilecache::get_pkg_info("pcx");
  return(pkgfilecache::get_filepath(pkg_info, filename, mustWork=mustWork));
}


#' @title Delete all data in the package cache.
#'
#' @return integer. The return value of the unlink() call: 0 for success, 1 for failure. See the unlink() documentation for details.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info erase_file_cache
delete_all_opt_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("pcx");
  return(pkgfilecache::erase_file_cache(pkg_info));
}


# md5f = 'inst/extdata/md5sum_libav_pcx'; md5i = read_md5sum_file(md5f);
#' @title Read md5sum file written by md5sum program.
#'
#' @param filepath character string, the path the the md5sum file
#'
#' @return data.frame with 3 columns: md5sum, is_binary, and filename.
#'
#' @note File names entries must not include whitespace.
#' @importFrom utils read.table
#' @export
read_md5sum_file <- function(filepath) {
  df = read.table(filepath, sep="", stringsAsFactors = FALSE, col.names = c('md5sum', 'filename'));
  rows_with_asterisk = startsWith(df$filename, '*'); # there may be a '*' to mark the input mode as binary in front of the file name.
  df$is_binary = rows_with_asterisk;
  df$filename[rows_with_asterisk] = substring(df$filename[rows_with_asterisk], 2, nchar(df$filename[rows_with_asterisk]));
  return(df);
}
