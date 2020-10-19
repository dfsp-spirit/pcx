# Helper functions for the unit tests, these can be used in any test.

#' @title Determine whether a test is running on CRAN under macos
#'
#' @description We are currently getting failed unit tests on CRAN under macos, while the package works under MacOS on both <https://builder.r-hub.io/> and on our MacOS machines. This is because the package file cache does not work on CRAN, as the HOME is mounted read-only on the CRAN test systems. So we have to skip the tests that require optional data under MacOS on CRAN.
#'
#' @return logical, whether a test is running on CRAN under MacOS
tests_running_on_cran_under_macos <- function() {
  return(tolower(Sys.info()[["sysname"]]) == 'darwin' && !identical(Sys.getenv("NOT_CRAN"), "true"));
}

download_test_data <- function() {
  if(requireNamespace('pkgfilecache', quietly = TRUE)) {
    pkg_info = pkgfilecache::get_pkg_info("pcx");

    md5_info = read_md5sum_file(system.file('extdata', 'md5sum_libav_pcx', package = 'pcx', mustWork = TRUE));
    base_url= 'https://samples.libav.org/image-samples/pcx/';
    urls = paste(base_url, md5_info$filename, sep='');

    cfiles = pkgfilecache::ensure_files_available(pkg_info, md5_info$filename, urls, md5sums=md5_info$md5sum);
    cfiles$file_status = NULL; # not exposed to end user
    return(invisible(cfiles));
  }
}

# md5f = 'inst/extdata/md5sum_libav_pcx'; md5i = read_md5sum_file(md5f);
#' @title Read md5sum file written by md5sum program.
#'
#' @param filepath character string, the path the the md5sum file
#'
#' @return data.frame with 3 columns: md5sum, is_binary, and filename.
#'
#' @note File names entries must not include whitespace.
read_md5sum_file <- function(filepath) {
  df = read.table(filepath, sep="", stringsAsFactors = FALSE, col.names = c('md5sum', 'filename'));
  rows_with_asterisk = startsWith(df$filename, '*'); # there may be a '*' to mark the input mode as binary in front of the file name.
  df$is_binary = rows_with_asterisk;
  df$filename[rows_with_asterisk] = substring(df$filename[rows_with_asterisk], 2, nchar(df$filename[rows_with_asterisk]));
  return(df);
}
