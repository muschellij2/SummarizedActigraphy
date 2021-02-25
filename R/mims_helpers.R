remake_acc = function(df, hdr) {
  df <- list(
    data = df,
    freq = attr(df, "sample_rate"),
    header = hdr,
    missingness = attr(df, "missingness"))
  class(df) = "AccData"
}

#' Default MIMS worker functions
#'
#' @param df data set of data, usually time and X/Y/Z.  Usually from
#' \code{\link{read_actigraphy}}
#' @param dynamic_range dynamic range of the data.  Will be passed to
#' \code{\link{get_dynamic_range}}
#'
#' @return A data set of data
#' @export
mims_default_extrapolation = function(df, dynamic_range = NULL) {
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for mims_default_extrapolation")
  }
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
  }
  dynamic_range = SummarizedActigraphy::get_dynamic_range(df, dynamic_range = dynamic_range)
  # required for MIMS functionality
  df = ensure_header_timestamp(df)
  sample_rate = attr(df, "sample_rate")

  noise_level = 0.03
  k = 0.05
  spar = 0.6
  # interpolation
  df <- MIMSunit::extrapolate(df, dynamic_range, noise_level,
                              k, spar)
  colnames(df) = gsub("EXTRAPOLATED_", "", colnames(df))
  attr(df, "sample_rate") = 100
  if (is_acc) {
    df = remake_acc(df, hdr)
  }
  attr(df, "transformation") = c(attr(df, "transformation"),
                                 "extrapolated")
  df
}

#' @rdname mims_default_extrapolation
#' @export
mims_default_interpolation = function(df) {
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for mims_default_interpolation")
  }
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
  }
  df = ensure_header_timestamp(df)
  df = MIMSunit::interpolate_signal(df, sr = 100, method = "linear")
  colnames(df) = gsub("INTERPOLATED_", "", colnames(df))
  attr(df, "sample_rate") = 100
  if (is_acc) {
    df = remake_acc(df, hdr)
  }
  attr(df, "transformation") = c(attr(df, "transformation"),
                                 "interpolated")
  df
}


# data already has been interpolated
#' @rdname mims_default_extrapolation
#' @export
mims_default_filtering = function(df) {
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for mims_default_filtering")
  }
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
  }
  df = ensure_header_timestamp(df)
  sample_rate = attr(df, "sample_rate")
  if (is.null(sample_rate)) {
    stop("df needs attribute 'sample_rate' for filtering")
  }
  if (sample_rate != 100) {
    warning("Sample rate != 100, not sure if this works the same")
  }
  df <- MIMSunit::iir(
    df, sr = sample_rate,
    # cutoff_freq = eval(formals(custom_mims_unit)$cutoffs),
    cutoff_freq = c(0.2, 5.0),
    order = 4,
    type = "pass",
    filter_type = "butter")
  colnames(df) = gsub("IIR_", "", colnames(df))
  attr(df, "sample_rate") = sample_rate
  if (is_acc) {
    df = remake_acc(df, hdr)
  }
  attr(df, "transformation") = c(attr(df, "transformation"),
                                 "filtered")
  df
}
