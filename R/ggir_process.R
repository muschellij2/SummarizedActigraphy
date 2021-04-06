#' Run GGIR Processing
#'
#' @param df An object with columns `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param unit length of time to calculate measures over.  a character string
#' specifying a time unit or a multiple of a unit to be rounded to.
#' Valid base units are `second`, `minute`, `hour`, `day`, `week`, `month`,
#' `bimonth`, `quarter`, `season`, `halfyear`, and `year`.
#' Arbitrary unique English abbreviations as in the \code{\link{period}}
#' constructor are allowed.
#' @param verbose print diagnostic messages
#' @param calibrate Run calibration, passed to \code{do.calibrate} in \code{GGIR}
#' @param ... additional arguments to pass to \code{\link{g.shell.GGIR}}
#'
#' @return A list with all the output elements
#' @export
#'
#' @examples
#' \dontrun{
#' files = read.gt3x::gt3x_datapath(2)
#' path = files[1]
#' res = read_actigraphy(path)
#' res = fix_zeros(res)
#' out = ggir_process(res, desiredtz = "UTC")
#' sum_data = out$part2$IMP$metashort
#' m = calculate_mad(res)
#' m[-(1:10), c("HEADER_TIME_STAMP", "ENMO_t")]
#' head(sum_data, 20)
#' sum_data$HEADER_TIME_STAMP = lubridate::as_datetime(sum_data$timestamp)
#' sum_data = dplyr::left_join(sum_data, m)
#' cor(sum_data$ENMO, sum_data$ENMO_t)
#' }
ggir_process = function(df, unit = "1 min", calibrate = TRUE, verbose = TRUE, ...) {
  tdir = tempfile()
  dir.create(tdir, recursive = TRUE)

  outputdir = tempfile()
  dir.create(outputdir, recursive = TRUE)
  output_directory = file.path(outputdir, paste0("output_", basename(tdir)))
  # res = fix_zeros(res)

  file = tempfile(fileext = ".csv.gz", tmpdir = tdir)
  if (verbose) {
    message("Writing out file ", file)
  }
  file = write_acc_csv(df, file = file)
  rm(df)

  if (verbose) {
    message("File is located at ", file)
  }
  epoch_in_seconds =  n_in_interval(unit, 1)
  if (verbose) {
    message("Output directory is located at ", output_directory)
  }
  if (verbose) {
    message("epoch in seconds ", epoch_in_seconds)
  }

  args = list(...)
  # 5 second interval
  # windowsizes = c(5, 900, 3600),
  # 60 - minute
  windowsizes = c(epoch_in_seconds, 900, 3600)
  if ("windowsizes" %in% names(args)) {
    warning("windowsizes is specified, overrides unit")
    windowsizes = args$windowsizes
  }
  if ("do.cal" %in% names(args)) {
    warning("do.cal is specified, overrides unit")
    calibrate = args$do.cal
  }
  default_args = list(
    do.report = TRUE,
    do.cal = calibrate,
    do.anglez = TRUE,
    overwrite = TRUE
  )
  for (iarg in names(default_args)) {
    if (!iarg %in% names(args)) {
      args[[iarg]] = default_args[[iarg]]
    }
  }

  args$datadir = tdir
  args$do.cal = calibrate
  args$outputdir = outputdir


  out = do.call(GGIR::g.shell.GGIR, args = args)


  bn = basename(file)
  rda = paste0(bn, ".RData")
  meta_dir = file.path(output_directory, "meta")
  out_files = list(
    basic = file.path(meta_dir, "basic", paste0("meta_", rda)),
    part2 = file.path(meta_dir, "ms2.out", rda),
    part3 = file.path(meta_dir, "ms3.out", rda),
    part4 = file.path(meta_dir, "ms4.out", rda),
    part5 = file.path(meta_dir, "ms5.out", rda),
    sleep.qc = file.path(meta_dir, "sleep.qc", rda)
  )
  out = vector(mode = "list", length = length(out_files))
  names(out) = names(out_files)
  for (x in names(out_files)) {
    thisfile = out_files[[x]]
    e = new.env()
    if (file.exists(thisfile)) {
      load(thisfile, envir = e)
      e = as.list(e)
    } else {
      e = NULL
    }
    out[[x]] <- e
  }
  out$arguments = args
  if (!is.null(out$part2$IMP$metashort)) {
    names_ms = colnames(out$part2$IMP$metashort)
    if (!is.null(out$part2$IMP$averageday)) {
      if (is.null(colnames(out$part2$IMP$averageday))) {
        names_ms = setdiff(names_ms, "timestamp")
        if (length(names_ms) == ncol(out$part2$IMP$averageday)) {
          colnames(out$part2$IMP$averageday) = names_ms
          out$part2$IMP$averageday = as.data.frame(out$part2$IMP$averageday)
        }
      }
    }
  }
  out$output_directory = output_directory
  out$intermediate_file = file
  if (verbose) {
    message(out$basic$C$QCmessage)
  }
  out
}
