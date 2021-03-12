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
  transformations = get_transformations(df)
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
  }
  dynamic_range = SummarizedActigraphy::get_dynamic_range(
    df,
    dynamic_range = dynamic_range)
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
  transformations = c("extrapolated", transformations)
  transformations = set_transformations(df, transformations = transformations,
                                        add = FALSE)
  if (is_acc) {
    df = remake_acc(df, hdr)
  }
  df
}

#' @rdname mims_default_extrapolation
#' @export
mims_default_interpolation = function(df) {
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for mims_default_interpolation")
  }
  transformations = get_transformations(df)

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
  transformations = c("interpolated", transformations)
  transformations = set_transformations(df, transformations = transformations,
                                        add = FALSE)
  df
}


# data already has been interpolated
#' @rdname mims_default_extrapolation
#' @export
mims_default_filtering = function(df) {
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for mims_default_filtering")
  }
  transformations = get_transformations(df)
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
  transformations = c("filtered", transformations)
  transformations = set_transformations(df, transformations = transformations,
                                        add = FALSE)
  df
}

#' Default MIMS Pre-processing
#'
#' @param df Data set of raw accelerometry values, usually time and X/Y/Z.
#' Usually from \code{\link{read_actigraphy}}
#' @param use_extrapolation If `TRUE` the function will apply extrapolation
#' algorithm to the input signal, otherwise it will skip
#' extrapolation but only linearly interpolate the signal to 100Hz.
#' @param use_filtering If `TRUE` the function will apply bandpass
#' filtering to the input signal, otherwise it will skip the filtering.
#' @param verbose print diagnostic messages
#' @param dynamic_range the dynamic ranges of the input signal.  Passed to
#' \code{\link{mims_default_extrapolation}}.  Only needed if
#' \code{use_extrapolation = TRUE}
#' @param round_after_processing Should the result be rounded to 3
#' decimal values after processing, to make similar to standard accelerometry?
#'
#' @return A process data set
#' @export
mims_default_processing = function(
  df, use_extrapolation = TRUE, use_filtering = TRUE,
  verbose = TRUE, dynamic_range = NULL,
  round_after_processing = FALSE) {
  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))
  dynamic_range = get_dynamic_range(df, dynamic_range)
  if (use_extrapolation) {
    if (verbose) {
      message("Running extrapolation")
    }
    df <- mims_default_extrapolation(df, dynamic_range)
  } else {
    if (verbose) {
      message("Running interpolation")
    }
    df <- mims_default_interpolation(df)
  }
  if (use_filtering) {
    if (verbose) {
      message("Running filtering")
    }
    df = mims_default_filtering(df)
  }
  if (round_after_processing) {
    df = df %>%
      dplyr::mutate(
        X = round(X, 3),
        Y = round(Y, 3),
        Z = round(Z, 3))
  }
  df
}
