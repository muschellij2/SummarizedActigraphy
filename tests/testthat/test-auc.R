testthat::context("Check MIMS vs AUC")
file = system.file("extdata",
                   "TAS1H30182785_2019-09-17.gt3x",
                   package = "SummarizedActigraphy")


testthat::test_that("Calculating AUC gives exact as MIMS", {
  res = read_actigraphy(file, verbose = FALSE)
  res = fix_zeros(res)
  res = ensure_header_timestamp(res)
  if (requireNamespace("MIMSunit", quietly = TRUE)) {
    fast_mims = calculate_fast_mims(res)
    mims = calculate_mims(res)
    # making similar to mims
    mims$MIMS_UNIT[mims$MIMS_UNIT < 0] = NA
    res = mims_default_processing(res)
    auc = calculate_auc(res, allow_truncation = TRUE)
    auc = auc[, c("HEADER_TIME_STAMP", "AUC")]

    testthat::expect_equal(mims$MIMS_UNIT, auc$AUC, tolerance = 1e-6)
    testthat::expect_equal(fast_mims$MIMS_UNIT, auc$AUC, tolerance = 1e-6)

  }
})
