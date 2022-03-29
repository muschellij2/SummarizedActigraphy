testthat::context("Calculating Summaries with data.table")

csv_file = tempfile(fileext = ".csv.gz")
dl = download.file(url = "https://ndownloader.figshare.com/files/24459683",
                   destfile = csv_file, mode = "wb")


testthat::test_that("Calculating AI Works with data.table", {
  testthat::skip_if_not_installed("data.table")
  csv = read_acc_csv(csv_file)
  csv = csv$data
  xyz = c("X", "Y", "Z")
  testthat::expect_true(all(unlist(c(csv[60631,xyz])) == 0))

  csv = fix_zeros(csv)

  df = calculate_ai(csv)
  dt = calculate_ai(data.table::as.data.table(csv))
  dt = as.data.frame(dt)
  df = as.data.frame(df)
  testthat::expect_equal(df, dt)
  cm = colMeans(df[c("AI")])

  testthat::expect_equal(
    cm,
    c(AI = 1.93594188261824),
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )

  cm = colMeans(dt[c("AI")])
  testthat::expect_equal(
    cm,
    c(AI = 1.93594188261824),
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )
})



testthat::test_that("Calculating Summaries Works with data.table", {
  testthat::skip_if_not_installed("data.table")
  csv = read_acc_csv(csv_file)
  csv = csv$data
  xyz = c("X", "Y", "Z")
  testthat::expect_true(all(unlist(c(csv[60631,xyz])) == 0))

  csv = fix_zeros(csv)

  df = calculate_mad(csv)
  dt = calculate_mad(data.table::as.data.table(csv))
  dt = as.data.frame(dt)
  df = as.data.frame(df)
  testthat::expect_equal(df, dt)
  cn = setdiff(colnames(df), "HEADER_TIME_STAMP")
  cm = colMeans(df[, cn])

  true_vec =     c(SD = 0.0650732323834855, SD_t = 0.0607735166358629, AI_DEFINED = 0.0896506702949309,
                   MAD = 0.0325750622274331, MEDAD = 0.0181929081988467, mean_r = 1.02525455686984,
                   ENMO_t = 0.0306416201033462)
  testthat::expect_equal(
    cm,
    true_vec,
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )

  cm = colMeans(dt[, cn])
  testthat::expect_equal(
    cm,
    true_vec,
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )
})
