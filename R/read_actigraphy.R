#' Read Actigraphy File
#'
#' @param file file to read
#' @param ... additional arguments to pass to function
#' @param read_function A custom function to read the data in.
#'
#' @return An accelerometer object from the function used to
#' read the file.
#' @export
#'
#' @importFrom tools file_ext
#' @importFrom R.utils decompressFile
#'
#' @examples
#' url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#' destfile = tempfile(fileext = ".zip")
#' dl = utils::download.file(url, destfile = destfile)
#' gt3x_file = utils::unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#' res = read_actigraphy(path)
#' file = system.file("extdata",
#' "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' file = system.file("extdata",
#' "MECSLEEP17_left_wrist_012854_2013-12-09_11-37-24.bin.xz",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#'
#' file = system.file("binfile", "ax3_testfile.cwa", package = "GGIR")
#' if (file.exists(file)) {
#' res = read_actigraphy(file)
#' }
#'
#' file = system.file("binfile", "genea_testfile.bin", package = "GGIR")
#' if (file.exists(file)) {
#' res = read_actigraphy(file)
#' }
#'
#' file = system.file("binfile", "GENEActiv_testfile.bin", package = "GGIR")
#' if (file.exists(file)) {
#' res = read_actigraphy(file)
#' }
#'
#'
#' file = "blah.exe"
#' testthat::expect_error(read_actigraphy(file))
read_actigraphy = function(file, ..., read_function = NULL) {
  file = test_unzip_file(file)
  ext = tools::file_ext(file)
  ext = tolower(ext)
  if (ext %in% "bin") {
    suppressWarnings({
      res = try({
        .read_actigraphy(file, ..., read_function = read_function)
      }, silent = TRUE)
    })
    if (inherits(res, "try_error")) {
      res = .read_actigraphy(file, ..., read_function = GGIR::g.binread)
    }
  } else {
    res = .read_actigraphy(file, ..., read_function = read_function)
  }
  return(res)
}

test_unzip_file = function(file) {
  if (is.factor(file)) {
    file = as.character(file)
  }
  stopifnot(length(file) == 1 && is.character(file))
  ext = tools::file_ext(file)
  ext = tolower(ext)
  if (ext %in% c("gz", "xz", "bz2")) {
    FUN = switch(ext,
                 gz = gzfile,
                 xz = xzfile,
                 bz2 = bzfile
    )
    file = R.utils::decompressFile(
      file,
      ext = ext,
      FUN = FUN,
      temporary = TRUE,
      overwrite = TRUE,
      remove = FALSE)
  }
  return(file)
}

.read_actigraphy = function(file, ..., read_function = NULL) {
  ext = tools::file_ext(file)
  ext = tolower(ext)

  if (is.null(read_function)) {
    func = switch(
      ext,
      # bin = GGIR::g.binread,
      cwa = GGIR::g.cwaread,
      GGIR::g.readaccfile,
      gt3x = read.gt3x::read.gt3x
    )
  } else {
    func = read_function
  }
  stopifnot(!is.null(func))
  if (ext %in% "gt3x") {
    default_args = list(asDataFrame = TRUE,
                        imputeZeroes = TRUE,
                        verbose = TRUE)
    args = list(file, ...)
    for (iarg in names(default_args)) {
      if (!iarg %in% names(args)) {
        args[[iarg]] = default_args[[iarg]]
      }
    }
    res = do.call(func, args = args)
    hdr = attributes(res)$header
    if (!is.null(hdr)) {
      hdr = lapply(hdr, function(x) {
        if (length(x) == 0) {
          x = NA
        }
        x
      })
      n_values = sapply(res$header$Value, length)
      if (all(n_values == 1)) {
        hdr = lapply(hdr, as.character)
        hdr = tibble::tibble(
          Field = names(hdr),
          Value = unlist(hdr))
        attr(res, "header") = hdr
      }
    }
    res <- list(
      data.out = res,
      freq = attr(res, "sample_rate"),
      filename = basename(file),
      header = hdr,
      missingness = attr(res, "missingness"))
    class(res) = "AccData"
  } else {
    args = list(...)
    if ("desiredtz" %in% names(args)) {
      desiredtz = args$desiredtz
    } else {
      desiredtz = "UTC"
    }
    hdr = GGIR::g.inspectfile(file, desiredtz = desiredtz)

    if (ext %in% "cwa") {
      default_args = list(desiredtz = "UTC")
    } else if (ext %in% "bin" && !is.null(read_function)) {
      default_args = list()
    } else {
      filequality = list(filecorrupt = FALSE, filetooshort = FALSE)
      default_args = list(inspectfileobject = hdr,
                          blocksize = 2,
                          blocknumber = 1,
                          ws = 3,
                          desiredtz = "UTC",
                          filequality = filequality)
    }
    args = list(file,
                ...)
    for (iarg in names(default_args)) {
      if (!iarg %in% names(args)) {
        args[[iarg]] = default_args[[iarg]]
      }
    }
    res = do.call(func, args = args)
    res = res$P
    res$data.out = tibble::as_tibble(res$data.out)
    if (is.data.frame(res$missingness)) {
      res$missingness = tibble::as_tibble(res$missingness)
    }
    res$data.out$timestamp =
      lubridate::as_datetime(res$data.out$timestamp)
    n_values = sapply(res$header$Value, length)
    if (all(n_values == 1)) {
      res$header$Value = unlist(res$header$Value)
      if (is.character(res$header$Value)) {
        res$header$Value = trimws(res$header$Value)
      }
      res$header = tibble::as_tibble(res$header,
                                     rownames = NA)
      res$header = tibble::rownames_to_column(res$header,
                                              var = "Field")
    }
  }
  # attr(res, "function_to_read") = func
  return(res)
}
