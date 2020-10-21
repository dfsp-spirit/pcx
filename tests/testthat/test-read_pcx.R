
#' @title Determine whether a test is running on CRAN under macos
#'
#' @description We are currently getting failed unit tests on CRAN under macos, while the package works under MacOS on both <https://builder.r-hub.io/> and on our MacOS machines. This is because the package file cache does not work on CRAN, as the HOME is mounted read-only on the CRAN test systems. So we have to skip the tests that require optional data under MacOS on CRAN.
#'
#' @return logical, whether a test is running on CRAN under MacOS
#' @keywords internal
tests_running_on_cran_under_macos <- function() {
  return(tolower(Sys.info()[["sysname"]]) == 'darwin' && !identical(Sys.getenv("NOT_CRAN"), "true"));
}



testthat::test_that("We can read the Quake 2 Quad Damage skin if available.", {
  pcxf = '~/data/q2_pak/models/items/quaddama/skin.pcx';
  if( ! file.exists(pcxf)) {
    testthat::skip("Quake 2 Quad Damage skin available");
  }
  pcx = read.pcx(pcxf);

  testthat::expect_true(is.matrix(pcx$palette));
  testthat::expect_equal(length(pcx$palette) / 4L, 256L);
  testthat::expect_equal(dim(pcx$colors), c(148, 168));

  testthat::expect_equal(pcx$header$bitpix, 8L);
  testthat::expect_equal(pcx$header$num_channels, 1L);
})


testthat::test_that("We can read a PCX file with a 256 color palette.", {

  # file from https://samples.libav.org/image-samples/pcx/
  pcxf = system.file('extdata', 'BLOOD02.PCX', package = 'pcx', mustWork = TRUE);
  pcx = read.pcx(pcxf);

  # Check that we get a function:
  testthat::expect_true(is.matrix(pcx$palette));
})


testthat::test_that("We can read a PCX file that is not indexed.", {

  # file from https://samples.libav.org/image-samples/pcx/
  pcxf = system.file('extdata', 'lena.pcx', package = 'pcx', mustWork = TRUE);
  pcx = read.pcx(pcxf);

  testthat::expect_true(is.array(pcx$colors));
  testthat::expect_equal(dim(pcx$colors), c(512, 512, 3));

  testthat::expect_true(is.null(pcx$palette));
})


testthat::test_that("We can convert a PCX file to JPEG format.", {

  if( ! requireNamespace("jpeg", quietly = TRUE)) {
    testthat::skip("The 'jpeg' package needs to be installed for this test.");
  }

  # file from https://samples.libav.org/image-samples/pcx/
  pcxf = system.file('extdata', 'lena.pcx', package = 'pcx', mustWork = TRUE);
  pcx = read.pcx(pcxf);

  testthat::expect_true(is.array(pcx$colors));

  # Write as JPEG:
  lena_jpg = tempfile(fileext = '.jpg');
  jpeg::writeJPEG(pcx$colors/255., target = lena_jpg);
})


testthat::test_that("We can read all libav sample PCX files.", {
  testthat::skip_if(tests_running_on_cran_under_macos()); # Cannot download data on CRAN under MacOS.
  pcx::download_test_data();

  md5_info = read_md5sum_file(system.file('extdata', 'md5sum_libav_pcx', package = 'pcx', mustWork = TRUE));
  idx = 1L;
  for(filename in md5_info$filename) {
    testfile = pcx::get_opt_data_filepath(filename);
    cat(sprintf("Reading file '%s' (%d of %d).\n", filename, idx, nrow(md5_info)));
    pcx = read.pcx(testfile);
    testthat::expect_false(is.null(pcx$header));
    idx = idx + 1L;
  }
})
