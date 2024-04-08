url = paste0("https://github.com/THLfi/read.gt3x/files/",
             "3522749/GT3X%2B.01.day.gt3x.zip")
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile, mode = "wb")
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
gt3x_file

binfile = system.file("extdata",
                      "MECSLEEP17_left_wrist_012854_2013-12-09_11-37-24.bin.xz",
                      package = "SummarizedActigraphy")

g2_file = system.file("extdata",
                      "TAS1H30182785_2019-09-17.gt3x",
                      package = "SummarizedActigraphy")

df = data.frame(file = c(binfile, gt3x_file, g2_file),
                stringsAsFactors = FALSE)
df$age = stats::rpois(nrow(df), 50)

testthat::test_that("Can make an SE object and lmFit", {
  out = lapply(df$file, read_actigraphy)
  # flag_data = FALSE so no warnings about dynamic range
  sums = lapply(out, summarise_actigraphy, flag_data = FALSE)
  se = actigraphy_df_to_SummarizedExperiment(df, "file")
  testthat::skip_if_not_installed("limma")
  fit = limma::lmFit(SummarizedExperiment::assay(se))
  indices = which(rowSums(!is.na(SummarizedExperiment::assay(se))) > 1)
  vals = fit$coefficients[indices,1]
  check_vals = rowMeans(SummarizedExperiment::assay(se)[indices,], na.rm = TRUE)
  testthat::expect_equal(
    vals,
    check_vals
  )
  proportion = 0.01
  stdev.coef.lim = c(0.1,4)
  trend = FALSE
  robust = FALSE
  winsor.tail.p = c(0.05,0.1)
  eb = limma::eBayes(fit,
                     proportion = proportion,
                     stdev.coef.lim = stdev.coef.lim,
                     trend = trend, robust = robust,
                     winsor.tail.p = winsor.tail.p)
})
