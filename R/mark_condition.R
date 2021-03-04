#' Mark a Condition of a Specified Minimum Length
#'
#' @param x A logical vector
#' @param min_length minimum length, contiguous \code{TRUE} values required
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' x = c(FALSE, TRUE, TRUE, FALSE, FALSE, rep(TRUE, 10), FALSE, rep(TRUE, 20))
#' mark_condition(x)
#' mark_condition(x, 2)
#' mark_condition(x, 5)
#' mark_condition(x, 15)
mark_condition = function(x, min_length = 1) {
  stopifnot(is.logical(x))
  bad = rle(x)
  # 3 contiguous
  min_observed_length = min(bad$lengths[bad$values])
  if (min_observed_length >= min_length ||
      all(!bad$values)) {
    return(x)
  }
  mark_bad = bad$lengths >= min_length & bad$values
  bad$values = rep(FALSE, length(bad$values))
  bad$values[mark_bad] = TRUE
  inverse.rle(bad)
}
