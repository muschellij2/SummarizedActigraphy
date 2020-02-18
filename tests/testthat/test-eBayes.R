testthat::context("Whole Pipeline")

tarfile = system.file("extdata", "can.tar.gz", package = "limmi")
tarfile
exdir = tempfile()
dir.create(exdir)
files = untar(tarfile = tarfile, list = TRUE, exdir = exdir)
files = files[!grepl("^\\.", basename(files))]
unz = untar(tarfile = tarfile, files = files, exdir = exdir)
files = files[ grepl("hdr", basename(files))]
files
files = file.path(exdir, files)

testthat::test_that("eBayes works", {
  eb = limmi::nifti_eBayes(files)
  vals = head(na.omit(eb$lm_fit$coefficients[,1]), 10)

  testthat::expect_equal(
    vals,
    c(0.121366907221576, 0.127145683858544, 0.158761932048947, 0.1563262026757,
      0.0135644840387007, 0.0429873446313044, 0.0688248955023786, 0.0809315139971053,
      0.0801648885632555, 0.121430590748787)
  )
})
