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
  eb = limmi::nifti_eBayes(files)
  vals = head(na.omit(eb$lm_fit$coefficients[,1]), 10)

  testthat::expect_equal(
    vals,
    c(0.121366907221576, 0.127145683858544, 0.158761932048947, 0.1563262026757,
      0.0135644840387007, 0.0429873446313044, 0.0688248955023786, 0.0809315139971053,
      0.0801648885632555, 0.121430590748787)
  )
})
