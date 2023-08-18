testthat::test_that("using read_actigraphy on CSV files", {
  file = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz",
                     package = "SummarizedActigraphy")
  out = SummarizedActigraphy::read_actigraphy(file)
  testthat::expect_true(out$freq == 100L)
  file = system.file("extdata", "example1sec.csv", package = "AGread")
  if (file.exists(file)) {
    testthat::expect_warning({
      out = SummarizedActigraphy::read_actigraphy(file, only_xyz = FALSE)
    })
    testthat::expect_true(out$freq == 1L)
  }
})
