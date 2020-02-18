
#' Run `eBayes` on NIfTI Images
#'
#' @inheritParams nifti_lmFit
#' @param proportion numeric value between 0 and 1, assumed proportion of
#' voxels which are differentially expressed, passed to \code{\link{eBayes}}
#' @param stdev.coef.lim numeric vector of length 2, assumed
#' lower and upper limits for the standard deviation of
#' log2-fold-changes for differentially expressed voxels,
#' passed to \code{\link{eBayes}}
#' @param trend ogical, should an intensity-trend be allowed for the
#' prior variance? Default is that the prior variance is constant,
#' passed to \code{\link{eBayes}}
#' @param robust logical, should the estimation of df.prior and
#' var.prior be robustified against outlier sample variances,
#' passed to \code{\link{eBayes}}
#' @param winsor.tail.p numeric vector of length 1 or 2,
#' giving left and right tail proportions of x to Winsorize.
#' Used only when \code{robust=TRUE}, passed to \code{\link{eBayes}}
#' @param adjust.method method to adjust p-value,
#'  passed to \code{\link{topTable}}
#' @param coef column number or column name specifying which
#' coefficient or contrast of the linear model is of interest,
#' passed to \code{\link{topTable}}
#' @return
#' @export
#'
#' @return A list of output images from `eBayes` and the linear fit
#' @examples
#' tarfile = system.file("extdata", "can.tar.gz", package = "limmi")
#' tarfile
#' exdir = tempfile()
#' dir.create(exdir)
#' files = untar(tarfile = tarfile, list = TRUE, exdir = exdir)
#' files = files[!grepl("^[.]", basename(files))]
#' unz = untar(tarfile = tarfile, files = files, exdir = exdir)
#' files = files[ grepl("hdr", basename(files))]
#' files
#' files = file.path(exdir, files)
#' res = nifti_eBayes(files, verbose = FALSE)
#' \dontrun{
#' design_mat = cbind(x1 = rep(c(0, 1), each = 6),
#' x2 = rep(c(0, 1, 0, 1), each = 3))
#' res2 = nifti_eBayes(files, design = design_mat, coef = 2)
#' }
nifti_eBayes = function(
  imgs, mask = NULL, verbose = TRUE,
  ...,
  proportion = 0.01, stdev.coef.lim = c(0.1,4),
  trend = FALSE, robust = FALSE, winsor.tail.p = c(0.05,0.1),
  coef = NULL,
  adjust.method = "BH") {
  if (is.factor(imgs)) {
    imgs = as.character(imgs)
  }
  img = RNifti::asNifti(imgs[[1]])
  if (!is.null(mask)) {
    mask =  RNifti::asNifti(mask) > 0
  } else {
    mask = array(TRUE, dim = dim(img))
  }

  if (verbose) {
    msg = paste0("Combining Images into a Matrix")
    message(msg)
  }
  mat = nifti_images_to_matrix(imgs, mask, verbose = verbose)
  # n = nrow(mat)
  if (verbose) {
    msg = paste0("Making lmFit")
    message(msg)
  }
  fit = limma::lmFit(mat, ...)

  if (verbose) {
    msg = paste0("Running eBayes")
    message(msg)
  }
  mask = RNifti::asNifti(mask, reference = img)
  L = eBayes_to_images(
    fit,
    mask = mask,
    verbose = verbose,
    proportion = proportion,
    stdev.coef.lim = stdev.coef.lim,
    trend = trend,
    robust = robust,
    winsor.tail.p = winsor.tail.p,
    coef = coef,
    adjust.method = adjust.method
  )

  L$lm_fit = fit
  L$mask = mask
  L
}

#' @rdname nifti_eBayes
#' @param fit an MArrayLM fitted model object produced by `lmFit` or
#' `contrasts.fit`.
#' @export
eBayes_to_images = function(
  fit,
  mask, verbose = TRUE,
  proportion = 0.01, stdev.coef.lim = c(0.1,4),
  trend = FALSE, robust = FALSE, winsor.tail.p = c(0.05,0.1),
  coef = NULL,
  adjust.method = "BH") {
  cols_to_grab = c("coefficients", "stdev.unscaled",
                   "t", "p.value", "lods", "adjusted_p_value")
  if (verbose) {
    msg = paste0("Running p-value adjustment")
    message(msg)
  }
  n = nrow(fit$coefficients)

  eb.fit = limma::eBayes(
    fit,
    proportion = proportion,
    stdev.coef.lim = stdev.coef.lim,
    trend = trend, robust = robust,
    winsor.tail.p = winsor.tail.p)

  nc = ncol(eb.fit$coefficients)
  eb.fit$adjusted_p_value = sapply(
    seq(nc),
    function(x) {
      limma::topTable(
        eb.fit,
        sort.by = "none",
        coef = x,
        adjust.method = adjust.method,
        number = n)$adj.P.Val
    })
  colnames(eb.fit$adjusted_p_value) = colnames(fit$coefficients)

  if (!is.null(coef)) {
    eb.fit$coef_adjusted_p_value = matrix(
      limma::topTable(
        eb.fit,
        sort.by = "none",
        coef = coef,
        adjust.method = adjust.method,
        number = n)$adj.P.Val, ncol = 1)
    cols_to_grab = c(cols_to_grab, "coef_adjusted_p_value")
  }

  if (verbose) {
    msg = paste0("Transforming to images")
    message(msg)
  }
  img = mask
  out_images = lapply(cols_to_grab, function(x) {
    sub_eb = eb.fit[[x]]
    if (verbose > 1) {
      message(x)
    }
    res = apply(sub_eb, 2, function(r) {
      list(remake_nifti_image(r, img = img, mask = mask))
    })
    res = lapply(res, function(ll) ll[[1]])
    if (length(res) == 1) {
      res = res[[1]]
    }
    res
  })
  names(out_images) = cols_to_grab
  L = list(
    images = out_images,
    empirical_bayes = eb.fit,
    adjust.method = adjust.method
  )
  return(L)
}
