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
    ), tolerance = 1e-6
  )
})
