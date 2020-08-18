is.AccData = function(x) {
  inherits(x, "AccData")
}



ticks2datetime = function (ticks, tz = "GMT")
{
  ticks <- as.numeric(ticks)
  seconds <- ticks/1e+07
  datetime <- as.POSIXct(seconds, origin = "0001-01-01", tz = tz)
  datetime
}




# read it in
read_acc_csv = function(file, ...) {
  hdr = readr::read_lines(file, n_max = 10)
  df = readr::read_csv(
    file, skip = 10,
    col_types = readr::cols(
      .default = readr::col_double(),
      Date = readr::col_character(),
      Time = readr::col_time(format = "")
    ), ...)

  HEADER_TIME_STAMP = Date = Time = NULL
  rm(list= c("HEADER_TIME_STAMP", "Date", "Time"))
  readr::stop_for_problems(df)
  df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = paste(Date, Time),
      HEADER_TIME_STAMP = lubridate::dmy_hms(HEADER_TIME_STAMP))
  stopifnot(!anyNA(df$HEADER_TIME_STAMP))
  list(
    header = hdr,
    data = df
  )
}
quick_check = function(df) {
  if (is.AccData(df)) {
    df = df$data.out
  }
  HEADER_TIME_STAMP = Axis1 = Axis2 = Axis3 = NULL
  rm(list= c("HEADER_TIME_STAMP", "Axis1", "Axis2", "Axis3"))
  df = df %>%
    dplyr::mutate(VM_check = round(
      sqrt(Axis1^2 + Axis2^2 + Axis3^2), 2))
  stopifnot(max(abs(df$VM_check - df$`Vector Magnitude`)) <
              1e-5)
}
