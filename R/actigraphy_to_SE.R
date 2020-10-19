#' Convert Actigraphy data.frame to a SummarizedExperiment
#'
#' @param x An optional `data.frame` describing the samples/participants.
#' @param path_column column in `x` that is the paths to the file
#' names of the data.  Can also be a list column of `x` where this column
#' are the images
#' @param measure measure to grab to create an assay.  If \code{NULL},
#' then every measure that is not the column \code{time}
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
#' "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' files = rep(file, 3)
#' df = data.frame(file = files,
#' age = stats::rpois(length(files), 50),
#' stringsAsFactors = FALSE)
#' se = actigraphy_df_to_SummarizedExperiment(df, "file")
#' \donttest{
#' df$file = factor(df$file)
#' se = actigraphy_df_to_SummarizedExperiment(df, "file", measure = "AI_mean")
#' }
#'
actigraphy_df_to_SummarizedExperiment = function(
  x,
  path_column = "file",
  measure = NULL,
  assay_name = measure,
  rowData = NULL,
  ...,
  verbose = TRUE) {

  files = x[[path_column]]
  if (is.factor(files)) {
    files = as.character(files)
  }
  mat = lapply(files, summarize_actigraphy, ..., verbose = verbose)
  if (!is.null(measure)) {
    mat = lapply(mat, function(x) {
      x[, measure, drop = FALSE]
    })
  } else {
    cn = unique(c(sapply(mat, colnames)))
    measure = setdiff(cn, "time")
  }
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

