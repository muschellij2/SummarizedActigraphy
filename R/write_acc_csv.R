#' Write ActiGraph-like Accelerometer CSV
#'
#' @param file CSV file to write out
#' @param x either an \code{AccData} or a \code{data.frame} of time and values.
#' @param sample_rate integer stating number of observations per second in the CSV
#'
#' @return The character path of the output file.
#' @export
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.csv.gz",
#' package = "SummarizedActigraphy")
#' if (file.exists(file)) {
#'    x = read_acc_csv(file)
#'    outfile = write_acc_csv(x)
#'    out2 = read_acc_csv(outfile)
#'    all.equal(x$data, out2$data)
#' }
write_acc_csv = function(x, file = tempfile(fileext = ".csv.gz"), sample_rate = NULL) {
  download_date = start_date = battery_voltage = firmware = NA
  # format_date =
  acc_data = is.AccData(x)
  if (acc_data) {
    header = x$header
  }
  if (is.null(sample_rate)) sample_rate = get_sample_rate(x)

  null_na = function(x) {
    if (is.null(x)) x = NA
    x
  }
  null_header_field = function(header, field) {
    x = header$Value[header$Field == field]
    x = null_na(x)
    stopifnot(length(x) == 1)
    x
  }

  x = ensure_header_timestamp(x, subset = FALSE)
  if (acc_data) {
    firmware = null_header_field(header, "Firmware")
    serial_number = null_header_field(header, "Serial Number")
    start_date = null_header_field(header, "Start Date")
    if (length(start_date) > 0) {
      start_date = lubridate::as_datetime(start_date)
    }
    if (length(start_date) == 0 || is.na(start_date)) {
      start_date = x$HEADER_TIME_STAMP[[1]]
    }
    download_date = null_header_field(header, "Download Date")
    if (length(download_date) > 0) {
      download_date = lubridate::as_datetime(download_date)
    }
    battery_voltage = null_header_field(header, "Battery Voltage")
  } else {
    start_date = lubridate::floor_date(x$HEADER_TIME_STAMP[[1]], unit = "1 second")
    download_date = NA
    battery_voltage = NA
    serial_number = attr(x, "serial_number")
    if (is.null(serial_number)) {
      serial_number = attr(x, "serial_prefix")
    }
  }

  start_time = format(start_date, format = "%H:%M:%S")
  start_date = format(lubridate::as_date(start_date), "%m/%d/%Y")
  download_time = format(download_date, format = "%H:%M:%S")
  download_date = format(lubridate::as_date(download_date), "%m/%d/%Y")

  req_vars = list(
    start_date = start_date,
    start_time = start_time,
    sample_rate = sample_rate
  )
  for (ivar in seq_along(names(req_vars))) {
    var_x = req_vars[[ivar]]
    if (is.na(var_x)) {
      warning(paste0(ivar, " is NA, may cause errors in header"))
    }
  }


  sa_version = as.character(utils::packageVersion("SummarizedActigraphy"))

  csv_hdr = c(
    paste0(
      "------------ ", "Data File Created By SummarizedActigraphy v",
      sa_version, "Firmware v", firmware, " ",
      " date format M/d/yyyy at ", sample_rate,
      " Hz", "  Filter None -----------"),
    paste0("Serial Number: ", serial_number),
    paste0("Start Time ", start_time),
    paste0("Start Date ", start_date),
    paste0("Epoch Period (hh:mm:ss) " , "00:00:00"),
    paste0("Download Time ", download_date),
    paste0("Download Date " , download_time),
    paste0("Current Memory Address: ", "0"),
    paste0("Current Battery Voltage: ", battery_voltage, "     Mode = 12"),
    "--------------------------------------------------"
  )

  x = as.data.frame(x)
  x = x[, SummarizedActigraphy::xyz]
  colnames(x) = paste0("Accelerometer ", colnames(x))

  readr::write_lines(csv_hdr, file = file)
  readr::write_csv(x, file = file, append = TRUE, col_names = TRUE)
  return(file)
}
