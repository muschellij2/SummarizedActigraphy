#' Run `lmFit` on NIfTI Images
#'
#' @param imgs List of images or character of filenames
#' @param mask Image to subset the data
#' @param verbose Print diagnostic messages
#' @param ... additional arguments to pass to \code{\link{lmFit}}
#'
#' @return The result from \code{\link{lmFit}}
#' @export
#'
nifti_lmFit = function(
  imgs, mask = NULL, verbose = TRUE,
  ...) {
  if (is.factor(imgs)) {
    imgs = as.character(imgs)
  }
  img = RNifti::asNifti(imgs[[1]])
  if (!is.null(mask)) {
    mask =  RNifti::asNifti(mask) > 0
  } else {
    mask = array(TRUE, dim = dim(img))
  }

  mat = nifti_images_to_matrix(imgs, mask, verbose = verbose)
  fit = limma::lmFit(mat, ...)
  fit
}

