


testthat::test_that("We can read the Quake 2 Quad Damage skin if available.", {
  pcxf = '~/data/q2_pak/models/items/quaddama/skin.pcx';
  if( ! file.exists(pcxf)) {
    testthat::skip("Quake 2 Quad Damage skin available");
  }
  pcx = read.pcx(pcxf);
  testthat::expect_true(is.matrix(pcx$palette));
})


testthat::test_that("We can read a PCX file with a 256 color palette.", {

  # file from https://samples.libav.org/image-samples/pcx/
  pcxf = system.file('extdata', 'BLOOD02.PCX', package = 'pcx', mustWork = TRUE);
  pcx = read.pcx(pcxf);

  # Check that we get a function:
  testthat::expect_true(is.matrix(pcx$palette));
})
