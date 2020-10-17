
testthat::test_that("We can read a PCX file with a 256 color palette.", {

  pcxf = system.file('extdata', 'BLOOD02.PCX', package = 'pcx', mustWork = TRUE);
  pcx = read.pcx(pcxf);

  # Check that we get a function:
  testthat::expect_true(is.matrix(pcx$palette));
})
