#' Convert Actigraphy data.frame to a SummarizedExperiment
#'
#' @param x An optional `data.frame` describing the samples/participants.
#' @param path_column column in `x` that is the paths to the file
#' names of the data.  Can also be a list column of `x` where this column
#' are the images
#' @param measure measure to grab to create an assay
#' @param assay_name Name of the `assay` in the
#' \code{\link{SummarizedExperiment}}
#' @param verbose print diagnostic messages
#' @param rowData row data to pass to \code{\link{SummarizedExperiment}}
#' @param ... additional arguments to pass to \code{\link{SummarizedExperiment}}
#'
#' @return A \code{SummarizedExperiment} output
#' @export
#'
#' @examples
#' file = system.file("extdata",
#' "TAS1H30182785 (2019-09-17).gt3x",
#' package = "SummarizedActigraphy")
#' files = rep(file, 10)
#' df = data.frame(file = files,
#' age = stats::rpois(length(files), 50),
#' stringsAsFactors = FALSE)
#' se = actigraphy_df_to_SummarizedExperiment(df, "file")
actigraphy_df_to_SummarizedExperiment = function(
  x,
  path_column = "file",
  measure = c("ai_mean", "mad_mean", "enmo_mean",
               "ai_median", "mad_median",
               "enmo_median"),
  assay_name = measure,
  rowData = NULL,
  ...,
  verbose = TRUE) {

  files = x[[path_column]]
  if (is.factor(files)) {
    files = as.character(files)
  }
  mat = lapply(files, summarize_actigraphy, ..., verbose = verbose)
  mat = lapply(mat, function(x) {
    x[, measure, drop = FALSE]
  })
  mat = lapply(measure, function(x) {
    mat = sapply(mat, function(r) {
      r[[x]]
    })
    mat
  })

  if (!is.null(assay_name)) {
    names(mat) = assay_name
  }
  SummarizedExperiment::SummarizedExperiment(
    assays = mat,
    rowData = rowData,
    colData = x,
    ...)
}

