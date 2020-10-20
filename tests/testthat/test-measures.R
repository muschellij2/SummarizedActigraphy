testthat::context("Calculating Summaries")

url = paste0("https://github.com/THLfi/read.gt3x/files/",
             "3522749/GT3X%2B.01.day.gt3x.zip")
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile)
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file


testthat::test_that("Calculating Summaries Works", {
  res = read_actigraphy(path)

  output = calculate_measures(res, calculate_mims = FALSE)
  cm = colMeans(output[c("AI", "SD", "MAD")])
  testthat::expect_equal(
    cm,
    c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217
    ), tolerance = 1e-5
  )
})


testthat::context("Calculating MIMS")

file = system.file("extdata",
                   "TAS1H30182785_2019-09-17.gt3x",
                   package = "SummarizedActigraphy")


testthat::test_that("Calculating Summaries Works", {
  res = read_actigraphy(file)

  if (requireNamespace("MIMSunit", quietly = TRUE)) {
    output = calculate_measures(res, calculate_mims = TRUE)
    cm = colMeans(output[c("AI", "SD", "MAD", "MIMS_UNIT")])
    testthat::expect_equal(
      cm,
      c(AI = 1.93017183478711, SD = 0.104740319672159, MAD = 0.0606835327590105,
        MIMS_UNIT = 5.32015536241241)
      , tolerance = 1e-5
    )
    n_idle = calculate_n_idle(res, unit = "5 min")
    testthat::expect_equal(
      n_idle$n_idle,
      c(4300L, 28900L, 30000L, 28600L, 30000L,
        30000L, 27400L, 27800L,
        500L))
  } else {
    output = calculate_measures(res, calculate_mims = FALSE)
    cm = colMeans(output[c("AI", "SD", "MAD")])
    testthat::expect_equal(
      cm,
      c(AI = 1.93017183478711, SD = 0.104740319672159, MAD = 0.0606835327590105)
      , tolerance = 1e-5
    )
    n_idle = calculate_n_idle(res, unit = "5 min")
    testthat::expect_equal(
      n_idle$n_idle,
      c(4300L, 28900L, 30000L, 28600L, 30000L,
        30000L, 27400L, 27800L,
        500L))
  }
})

