#' Convert Actigraphy data.frame to a SummarizedExperiment
#'
#' @param x An optional `data.frame` describing the samples/participants.
#' @param path_column column in `x` that is the paths to the file
#' names of the data.  Can also be a list column of `x` where this column
#' are the images
#' @param assay_name Name of the `assay` in the
#' \code{\link{SummarizedExperiment}}
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
#' age = stats::rpois(length(files), 50))
#' se = acc_df_to_SummarizedExperiment(df, "file")
acc_df_to_SummarizedExperiment = function(
  x, path_column = "file",
  assay_name = path_column,
  template_images = NULL,
  ...) {

  imgs = x[[path_column]]
  if (is.factor(imgs)) {
    imgs = as.character(imgs)
  }
  mat = nifti_images_to_matrix(imgs)
  if (!is.null(template_images)) {
    n_timgs = names(template_images)
    if (is.null(n_timgs) & is.character(template_images)) {
      n_timgs = template_images
    } else {
      n_timgs = NULL
    }
    rowData = nifti_images_to_matrix(template_images)
    colnames(rowData) = n_timgs
  } else {
    rowData = NULL
  }
  mat = list(mat)
  if (!is.null(assay_name)) {
    names(mat) = assay_name
  }
  SummarizedExperiment::SummarizedExperiment(
    assays = mat,
    rowData = rowData,
    colData = x,
    ...)
}

