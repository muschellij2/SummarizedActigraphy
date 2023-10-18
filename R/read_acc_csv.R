sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}

check_header_exists = function(file) {
  x = readr::read_lines(
    file, skip = 10,
    n_max = 1
  )
  x = strsplit(x, ",")[[1]]
  x = gsub('"', "", x)
  x = gsub("'", "", x)
  num_x = suppressWarnings({as.numeric(x)})
  # if they aren't numbers, there is a header
  check = mean(is.na(num_x)) >= 0.5
  return(check)
}

epoch_to_sample_rate = function(epoch) {
  epoch = lubridate::hms(epoch)
  epoch = lubridate::as.period(epoch, unit = "seconds")
  epoch = as.numeric(epoch)
  1/epoch
}


#' Read ActiGraph Accelerometer CSV
#'
#' @param file CSV file to read in
#' @param ... additional arguments to pass to \code{\link{read_csv}}
#' @param only_xyz should the data be subset for only X/Y/Z values (and time)?
#'
#' @return A list of the header and the data set
#' @export
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz",
#' package = "SummarizedActigraphy")
#' if (file.exists(file)) {
#'    out = read_acc_csv(file)
#'    SummarizedActigraphy:::parse_acc_header(file)
#' }
#' file = system.file("extdata", "example1sec.csv", package = "AGread")
#' if (file.exists(file)) {
#'    out = read_acc_csv(file, only_xyz = FALSE)
#'    SummarizedActigraphy:::quick_check(out$data)
#'    acc = out
#'    class(acc) = "AccData"
#'    SummarizedActigraphy:::quick_check(acc)
#' }
#'
#' file = system.file("extdata", "721431sec.csv.gz",
#' package = "SummarizedActigraphy")
#' if (file.exists(file)) {
#'    out = read_acc_csv(file, only_xyz = FALSE)
#'    cn = colnames(out$data)
#'    names(cn) = cn
#'    cn[c("X1", "X2", "X3", "X4")] = c("Axis1", "Axis2", "Axis3", "Steps")
#'    colnames(out$data) = cn
#'    acc = out
#'    class(acc) = "AccData"
#' }

read_acc_csv = function(file, ..., only_xyz = TRUE) {
  Timestamp = timestamp = NULL
  rm(list = c("Timestamp", "timestamp"))
  L = extract_acc_header(file)
  hdr = L$header
  srate = L$sample_rate
  lubridate_func = L$lubridate_func
  format = L$format
  start_date = L$start_date

  args = list(...)
  if (is.null(args$col_names)) {
    args$col_names = check_header_exists(file)
  }
  args$file = file
  args$skip = 10
  if (is.null(args$col_types)) {
    args$col_types = readr::cols(
      .default = readr::col_double(),
      Date = readr::col_character(),
      timestamp = readr::col_datetime(),
      Timestamp = readr::col_datetime(),
      Time = readr::col_time(format = "")
    )
  }
  suppressWarnings({
    df = do.call(readr::read_csv, args = args)
  })
  readr::stop_for_problems(df)

  time = HEADER_TIME_STAMP = Date = Time = NULL
  rm(list= c("HEADER_TIME_STAMP", "Date", "Time", "time"))
  if (all(c("Date", "Time") %in% colnames(df))) {
    # df = df %>%
    #   dplyr::mutate(
    #     HEADER_TIME_STAMP = paste(Date, Time),
    #     HEADER_TIME_STAMP = lubridate::dmy_hms(HEADER_TIME_STAMP))
    df = df %>%
      dplyr::mutate(
        HEADER_TIME_STAMP = paste(Date, Time),
        HEADER_TIME_STAMP = do.call(lubridate_func , args = list(HEADER_TIME_STAMP))
      )
  } else if ("timestamp" %in% colnames(df)) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = timestamp)
  } else if ("Timestamp" %in% colnames(df)) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = Timestamp)
  } else {
    if (is.na(srate)) {
      warning(paste0("Sample rate is NA, using epoch: ", L$parsed_header$epoch))
    }
    srate = epoch_to_sample_rate(L$parsed_header$epoch)
    df$HEADER_TIME_STAMP = seq(0, nrow(df) - 1)/srate
    df$HEADER_TIME_STAMP = start_date + df$HEADER_TIME_STAMP
    # reset for code below
    srate = NA
  }
  class(df) = "data.frame"
  stopifnot(!anyNA(df$HEADER_TIME_STAMP))
  colnames(df) = trimws(sub("Accelerometer", "", colnames(df)))

  if (only_xyz && !any(SummarizedActigraphy::xyz %in% colnames(df))) {
    warning("X/Y/Z not in the data, but only_xyz = TRUE, ",
            "setting only_xyz = FALSE")
    only_xyz = FALSE
  }
  if (only_xyz) {
    df = df[, c("HEADER_TIME_STAMP", "X", "Y", "Z")]
  } else {
    cn = colnames(df)
    df = df[, c("HEADER_TIME_STAMP", setdiff(cn, "HEADER_TIME_STAMP"))]
  }
  df = df %>%
    dplyr::rename(time = HEADER_TIME_STAMP)

  df = tibble::as_tibble(df)
  if (is.na(srate)) {
    srate = try({get_sample_rate(df)})
    if (inherits(srate, "try-error")) {
      srate = NA
    }
  }
  L = list(
    data = df,
    freq = srate,
    filename = file,
    header = L$parsed_header,
    original_header = hdr
  )
  class(L) = "AccData"
  L
}

#' @rdname read_acc_csv
#' @export
extract_acc_header = function(file) {
  hdr = readLines(file, n = 10)
  st = sub_thing(hdr, "Start Time")
  sd = sub_thing(hdr, "Start Date")
  format = sub(".*date format (.*) (Filter|at).*", "\\1", hdr[1])
  if (format == "") {
    warning("No format for date in the header, using mdy")
    format = "mdy"
  } else {
    format = tolower(format)
    format = c(sapply(strsplit(format, "/"), substr, 1,1))
    format = paste(format, collapse = "")
  }
  all_formats = c("ydm", "dym", "ymd", "myd", "dmy", "mdy")
  stopifnot(format %in% all_formats)
  lubridate_func = paste0(format, "_hms")
  lubridate_func = utils::getFromNamespace(lubridate_func, "lubridate")
  start_date = do.call(lubridate_func, args = list(paste0(sd, " ", st)))
  srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))
  L = list(
    format = format,
    start_date = start_date,
    sample_rate = srate,
    lubridate_func = lubridate_func)
  parsed_header = try({
    parse_acc_header(hdr)
  }, silent = FALSE)
  parsed_header = try({
    parse_acc_header(hdr)
  }, silent = FALSE)
  if (inherits(parsed_header, "try-error")) {
    warning("Header parsing errored, no parsed header returned")
    parsed_header = NULL
  }
  L$parsed_header = parsed_header
  L$header = hdr

  return(L)
}

parse_acc_header = function(hdr) {
  if (length(hdr) == 1 && file.exists(hdr)) {
    hdr = readLines(hdr, n = 10)
  }
  hdr = gsub("^-*", "", hdr)
  hdr = gsub("-*$", "", hdr)
  hdr = hdr[!hdr %in% ""]
  hdr = trimws(hdr)
  ac_version = gsub(".*ife v(\\d.*) Firm.*", "\\1", hdr[1])
  ac_firmware = gsub(".*irmware v(\\d.*) date.*", "\\1", hdr[1])
  filter = gsub(".*Filter(.*)", "\\1", hdr[1])
  filter = trimws(filter)
  filter = sub("-*$", "", filter)
  filter = trimws(filter)

  format = sub(".*date format (.*) (Filter|at).*", "\\1", hdr[1])
  if (format == "") {
    warning("No format for date in the header, using mdy")
  } else {
    format = tolower(format)
    format = c(sapply(strsplit(format, "/"), substr, 1,1))
    format = paste(format, collapse = "")
    all_formats = c("ydm", "dym", "ymd", "myd", "dmy", "mdy")
    stopifnot(format %in% all_formats)
  }
  srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))

  vars = c(serial = "Serial Number:",
           start_time = "Start Time",
           start_date = "Start Date",
           epoch = "Epoch Period \\(hh:mm:ss\\)",
           download_time = "Download Time",
           download_date = "Download Date",
           battery_voltage = ".*Battery Voltage:",
           memory_address = "Current Memory Address:",
           mode = ".*Mode\\s*="
  )
  out = lapply(vars, function(x) sub_thing(hdr, x))
  out$battery_voltage = sub("Mode.*", "", out$battery_voltage)
  out = lapply(out, trimws)
  out$firmware = ac_firmware
  out$actilife_version = ac_version
  out$filter = filter
  out$date_format = format
  out$sample_rate = srate
  out
}

