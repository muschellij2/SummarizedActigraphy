testthat::context("Reading in Data")

file = system.file("extdata",
                   "TAS1H30182785_2019-09-17.gt3x",
                   package = "SummarizedActigraphy")
files = rep(file, 3)
df = data.frame(file = files,
                age = stats::rpois(length(files), 50),
                stringsAsFactors = FALSE)


testthat::test_that("eBayes works", {
  se = actigraphy_df_to_SummarizedExperiment(df, "file")
  testthat::skip_if_not_installed("limma")
  eb = limma::lmFit(SummarizedExperiment::assay(se))
  vals = head(na.omit(eb$coefficients[,1]), 10)
  check_vals = head(SummarizedExperiment::assay(se), 10)[,1]
  testthat::expect_equal(
    vals,
    check_vals
  )
})
