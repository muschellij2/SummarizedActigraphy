testthat::test_that("multiplication works", {
  file = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz",
                     package = "SummarizedActigraphy")
  out = SummarizedActigraphy::read_actigraphy(file)
  testthat::expect_true(out$freq == 1L)
  file = system.file("extdata", "example1sec.csv", package = "AGread")
  if (file.exists(file)) {
    out = SummarizedActigraphy::read_actigraphy(file)
    testthat::expect_true(out$freq == 1L)
  }
})
