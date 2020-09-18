sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}



#' Read ActiGraph Accelerometer CSV
#'
#' @param file CSV file to read in
#' @param ... additional arguments to pass to \code{\link{read_csv}}
#'
#' @return A list of the header and the data set
#' @export
read_acc_csv = function(file, ...) {
  hdr = readLines(file, n = 10)
  st = sub_thing(hdr, "Start Time")
  sd = sub_thing(hdr, "Start Date")
  format = sub(".*date format (.*) at.*", "\\1", hdr[1])
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

  suppressWarnings({
    df = readr::read_csv(
      file, skip = 10,
      col_types = readr::cols(
        .default = readr::col_double(),
        Date = readr::col_character(),
        Time = readr::col_time(format = "")
      ), ...)
  })
  readr::stop_for_problems(df)

  time = HEADER_TIME_STAMP = Date = Time = NULL
  rm(list= c("HEADER_TIME_STAMP", "Date", "Time", "time"))
  if (all(c("Date", "Time") %in% colnames(df))) {
    df = df %>%
      dplyr::mutate(
        HEADER_TIME_STAMP = paste(Date, Time),
        HEADER_TIME_STAMP = lubridate::dmy_hms(HEADER_TIME_STAMP))
  } else {
    df$HEADER_TIME_STAMP = seq(0, nrow(df) - 1)/srate
    df$HEADER_TIME_STAMP = start_date + df$HEADER_TIME_STAMP
  }
  class(df) = "data.frame"
  stopifnot(!anyNA(df$HEADER_TIME_STAMP))
  colnames(df) = trimws(sub("Accelerometer", "", colnames(df)))

  df = df[, c("HEADER_TIME_STAMP", "X", "Y", "Z")]
  df = df %>%
    dplyr::rename(time = HEADER_TIME_STAMP)
  parsed_header = try({
    parse_acc_header(hdr)
  }, silent = FALSE)
  if (inherits(parsed_header, "try-error")) {
    warning("Header parsing errored, no parsed header returned")
    parsed_header = NULL
  }


  L = list(
    header = hdr,
    parsed_header = parsed_header,
    data = df
  )
}


parse_acc_header = function(hdr) {
  if (length(hdr) == 1 && file.exists(hdr)) {
    hdr = readLines(hdr)
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

  format = sub(".*date format (.*) at.*", "\\1", hdr[1])
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

