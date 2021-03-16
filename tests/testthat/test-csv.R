testthat::context("reading csv")

testthat::test_that("Reading in CSV and header", {
  file = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz",
                     package = "SummarizedActigraphy")
  out = read_acc_csv(file)
  testthat::expect_named(out,
                         c("data", "freq", "filename",
                           "header", "original_header"))
  testthat::expect_equal(
    colMeans(out$data[, c("X", "Y", "Z")]),
    c(X = -0.819743617463617, Y = -0.0207721787941788, Z = 0.0215000914760914)
  )
  hdr = SummarizedActigraphy:::parse_acc_header(file)
  testthat::expect_equal(hdr$sample_rate, 100L)

  file = system.file("extdata", "example1sec.csv", package = "AGread")
  if (file.exists(file)) {
    out = read_acc_csv(file, only_xyz = FALSE)

    testthat::expect_named(out,
                           c("data", "freq", "filename",
                             "header", "original_header"))
    testthat::expect_equal(
      colMeans(out$data[, paste0("Axis", 1:3)]),
      c(Axis1 = 60.9611111111111, Axis2 = 45.7666666666667, Axis3 = 68.55)
    )
    SummarizedActigraphy:::quick_check(out$data)
    acc = out
    class(acc) = "AccData"
    SummarizedActigraphy:::quick_check(acc)
    testthat::expect_equal(get_sample_rate(out), 1L)
  }
})
