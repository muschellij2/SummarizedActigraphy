#' @title Transform set of images to matrix
#' @description Creates a matrix, where the voxels are on the rows
#' and images are on the columns
#'
#' @param imgs Vector of files or list of images (niftiImage, array, or nifti)
#' @param mask Binary image to subset the voxels
#' @param verbose print diagnostic messages
#'
#' @return Matrix of V by n, where V is the product of the dimensions of one
#' image or the number of voxels in the mask, and n is the number of images
#' @export
nifti_images_to_matrix = function(imgs, mask = NULL, verbose = TRUE) {
  if (is.factor(imgs)) {
    imgs = as.character(imgs)
  }
  img = RNifti::asNifti(imgs[[1]])
  if (!is.null(mask)) {
    mask = RNifti::asNifti(mask)
    mask = mask > 0
  } else {
    mask = array(TRUE, dim = dim(img))
  }
  dimg = dim(img)
  pdim = RNifti::pixdim(img)
  rm(img)
  n_voxels_dim = prod(dimg)
  n_voxels = sum(mask, na.rm = TRUE)
  ind = which(mask, arr.ind = TRUE)
  rm(mask)
  n = length(imgs)
  mat = matrix(NA_real_, nrow = n_voxels, ncol = n)
  if (verbose) {
    pb = utils::txtProgressBar(max = n, style = 3)
  }
  for (i in seq(n)) {
    if (verbose) {
      utils::setTxtProgressBar(pb, value = i)
    }
    x = RNifti::asNifti(imgs[[i]])
    stopifnot(prod(dim(x)) == n_voxels_dim)
    mat[,i] = c(x[ind])
  }
  if (verbose) {
    close(pb)
  }
  if (is.character(imgs)) {
    colnames(mat) = imgs
  }
  attr(mat, "image_dim") = dimg
  attr(mat, "image_pixdim") = pdim
  mat
}
