#' Get Transformations
#'
#' @param df data set of data, usually time and X/Y/Z.  Usually from
#' \code{\link{read_actigraphy}}
#' @return \code{\link{set_transformations}} returns the data, with the
#' `transformations` attribute updated and \code{\link{set_transformations}}
#' returns the attribute `transformations`
#' @export
get_transformations = function(df) {
  transforms = attr(df, "transformations")
  is_acc = is.AccData(df)
  if (is_acc) {
    transforms = c(transforms, attr(df$data, "transformations"))
    transforms = unique(transforms)
  }
  transforms
}

#' @export
#' @rdname get_transformations
#' @param add Add the transformations to those already there in `df`
#' @param transformations character string of transformations
set_transformations = function(df, transformations, add = TRUE) {
  if (add) {
    transforms = get_transformations(df)
  } else {
    transforms = NULL
  }
  # this is right - we are appending new stuff to beginning
  transformations = c(transformations, transforms)
  attr(df, "transformations") = transformations
  df
}
