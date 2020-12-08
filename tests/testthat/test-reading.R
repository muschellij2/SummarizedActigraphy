testthat::context("Reading in Data")

url = paste0("https://github.com/THLfi/read.gt3x/files/",
             "3522749/GT3X%2B.01.day.gt3x.zip")
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile, mode = "wb")
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file

testthat::test_that("GT3x", {

  res = read_actigraphy(path)
  testthat::expect_equal(mean(res$data$X), -0.228406351135833)

  dob = res$header$Value[res$header$Field == "DateOfBirth"]
  if (length(dob) > 0) {
    SummarizedActigraphy:::ticks2datetime(dob)
  }

  file = system.file("extdata",
                     "TAS1H30182785_2019-09-17.gt3x",
                     package = "SummarizedActigraphy")
  res = read_actigraphy(file)
  testthat::expect_equal(mean(res$data$X), -0.0742151351351352)

})

testthat::test_that("bad file", {

  file = "blah.exe"
  testthat::expect_error(read_actigraphy(file))
})


testthat::test_that("bin formats", {

  file = system.file("extdata",
                     "MECSLEEP17_left_wrist_012854_2013-12-09_11-37-24.bin.xz",
                     package = "SummarizedActigraphy")
  res = read_actigraphy(file)
  testthat::expect_equal(mean(res$data$X), -0.147653632966532)


  file = system.file("testfiles", "genea_testfile.bin", package = "GGIR")
  if (file.exists(file)) {
    res = read_actigraphy(file)
    # mg not g (or vector magnitude?)
    testthat::expect_equal(mean(res$data$X)/1000, -0.15303776683087)
  }

  file = system.file("testfiles", "GENEActiv_testfile.bin", package = "GGIR")
  if (file.exists(file)) {
    res = read_actigraphy(file)
    testthat::expect_equal(mean(res$data$X), -0.194275899087493)
  }
})

testthat::test_that("CWA formats", {

  file = system.file("testfiles", "ax3_testfile.cwa", package = "GGIR")
  if (file.exists(file)) {
    res = read_actigraphy(file)
    testthat::expect_equal(mean(res$data$X), 0.775495573675064)
  }
})

file = system.file("extdata",
                   "TAS1H30182785_2019-09-17.gt3x",
                   package = "SummarizedActigraphy")
files = rep(file, 3)
df = data.frame(file = files,
                age = stats::rpois(length(files), 50),
                stringsAsFactors = FALSE)


testthat::test_that("eBayes works - same file", {
  df$file = factor(df$file)
  se = actigraphy_df_to_SummarizedExperiment(df, "file", measure = "AI_mean")
  testthat::skip_if_not_installed("limma")
  eb = limma::lmFit(SummarizedExperiment::assay(se))
  vals = head(na.omit(eb$coefficients[,1]), 10)
  check_vals = head(SummarizedExperiment::assay(se), 10)[,1]
  testthat::expect_equal(
    vals,
    check_vals
  )
})


testthat::context("Reading in CWA")


file  = system.file("testfiles/ax3_testfile.cwa", package = "GGIR")[1]
files = rep(file, 3)
df = data.frame(file = files,
                age = stats::rpois(length(files), 50),
                stringsAsFactors = FALSE)


testthat::test_that("eBayes works - but with cwa", {
  se = actigraphy_df_to_SummarizedExperiment(df, "file")
  testthat::skip_if_not_installed("limma")
  fit = limma::lmFit(SummarizedExperiment::assay(se))
  vals = head(na.omit(fit$coefficients[,1]), 10)
  check_vals = head(SummarizedExperiment::assay(se), 10)[,1]
  testthat::expect_equal(
    vals,
    check_vals
  )
})



