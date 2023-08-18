testthat::context("Calculating Summaries")

url = paste0("https://github.com/THLfi/read.gt3x/files/",
             "3522749/GT3X%2B.01.day.gt3x.zip")
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile, mode = "wb")
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file

csv_file = tempfile(fileext = ".csv.gz")
dl = download.file(url = "https://ndownloader.figshare.com/files/24459683",
                    destfile = csv_file, mode = "wb")


testthat::test_that("Calculating Summaries Works", {
  csv = read_acc_csv(csv_file)
  csv = csv$data
  res = read_actigraphy(path, verbose = FALSE)
  lubridate::tz(res$data$time) = "UTC"

  xyz = c("X", "Y", "Z")
  testthat::expect_true(all(unlist(c(csv[60631,xyz])) == 0))
  testthat::expect_true(all(unlist(c(res$data[60631,xyz])) == 0))

  csv_output = calculate_measures(csv, calculate_mims = FALSE)
  output = calculate_measures(res, calculate_mims = FALSE)
  testthat::expect_equal(csv_output, output)
  cm = colMeans(output[c("AI", "SD", "MAD")])

  testthat::expect_equal(
    cm,
    c(AI = 1.93594188261824, SD = 0.0650732323834855, MAD = 0.0325750622274331),
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )



  ##################################
  # BY second
  ##################################
  csv_fixed = fix_zeros(csv, by_second = FALSE)
  testthat::expect_false(all(unlist(c(csv_fixed[60631,xyz])) == 0))
  rm(csv_fixed)

  # zeros are still there - stays on one second
  csv = fix_zeros(csv, by_second = TRUE)
  testthat::expect_true(all(unlist(c(csv[60631,xyz])) == 0))

  csv_output = calculate_measures(csv, fix_zeros = FALSE, calculate_mims = FALSE)
  output = calculate_measures(res, by_second = TRUE, calculate_mims = FALSE)
  testthat::expect_equal(csv_output, output)
  cm = colMeans(output[c("AI", "SD", "MAD")])

  testthat::expect_equal(
    cm,
    c(AI = 1.93596393318777, SD = 0.065882608891451, MAD = 0.0334080714317491),
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )


})

testthat::test_that("Calculating Summaries BY_SECOND", {
  csv = read_acc_csv(csv_file)
  csv = csv$data
  res = read_actigraphy(path, verbose = FALSE)
  lubridate::tz(res$data$time) = "UTC"


  csv_output = calculate_measures(csv, calculate_mims = FALSE)
  output = calculate_measures(res, calculate_mims = FALSE)
  testthat::expect_equal(csv_output, output)
  cm = colMeans(output[c("AI", "SD", "MAD")])

  testthat::expect_equal(
    cm,
    c(AI = 1.93594188261824, SD = 0.0650732323834855, MAD = 0.0325750622274331),
    # c(AI = 1.55390057389385, SD = 0.0501211543332476, MAD = 0.0294511550834217),
    tolerance = 1e-5
  )
})


testthat::context("Calculating MIMS")

file = system.file("extdata",
                   "TAS1H30182785_2019-09-17.gt3x",
                   package = "SummarizedActigraphy")


testthat::test_that("Calculating Summaries Works", {
  res = read_actigraphy(file, verbose = FALSE)

  if (requireNamespace("MIMSunit", quietly = TRUE)) {
    output = calculate_measures(res, calculate_mims = TRUE, fix_zeros = TRUE)

    print(dput(output$MIMS_UNIT))
    testthat::expect_equal(
      output$MIMS_UNIT,
      c(63.242524442506, 54.5102327000229, 31.1747913899169, 25.2219789460452,
        8.57512380428226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13.8770214728778, 10.5567081544926,
        0, 0, 0, 0, -0.01), tolerance = 1e-5
    )
    testthat::expect_equal(
      round(output$MIMS_UNIT, 3),
      c(63.243, 54.51, 31.175, 25.222, 8.575, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 13.877, 10.557, 0, 0, 0, 0, -0.01),
      tolerance = 0.001
    )
    cm = colMeans(output[c("AI", "SD", "MAD", "MIMS_UNIT")])
    testthat::expect_equal(
      cm,
      c(AI = 1.93017183478711, SD = 0.104740319672159, MAD = 0.0606835327590105,
        MIMS_UNIT = 5.05239953439375)
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

