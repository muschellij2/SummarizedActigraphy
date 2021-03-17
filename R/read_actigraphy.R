copy_attributes = function(res, old_at) {
  new_at = attributes(res)
  sd = setdiff(names(old_at), names(new_at))
  rm(new_at)
  for (isd in sd) {
    attr(res, isd) = old_at[[isd]]
  }
  return(res)
}

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
#'
#'
#' file = system.file("extdata",
#' "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' testthat::expect_equal(mean(res$data$X), -0.0742151351351352)
#'
#' file = "blah.exe"
#' testthat::expect_error(read_actigraphy(file))
#' \donttest{
#' url = paste0("https://github.com/THLfi/read.gt3x/files/",
#' "3522749/GT3X%2B.01.day.gt3x.zip")
#' destfile = tempfile(fileext = ".zip")
#' dl = utils::download.file(url, destfile = destfile)
#' gt3x_file = utils::unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#' res = read_actigraphy(path)
#' testthat::expect_equal(mean(res$data$X), -0.228406351135833)
#'
#' dob = res$header$Value[res$header$Field == "DateOfBirth"]
#' if (length(dob) > 0) {
#'  SummarizedActigraphy:::ticks2datetime(dob)
#' }
#'
#' }
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
    if (ext == "" && dir.exists(file)) {
      L = list.files(path = file)
      if (any(c("info.txt", "log.bin", "activity.bin") %in% L)) {
        ext = "gt3x"
      }
    }
    func = switch(
      ext,
      # bin = GGIR::g.binread,
      gt3x = read.gt3x::read.gt3x,
      cwa = GGIR::g.cwaread,
      GGIR::g.readaccfile
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
    # reordering columns
    tmp_at = attributes(res)

    cn = colnames(res)
    cn  = unique(c("time", "X", "Y", "Z", cn))
    cn = intersect(cn, colnames(res))
    res = res[, cn]
    res = copy_attributes(res, tmp_at)
    rm(tmp_at)

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
      data = res,
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
      default_args = list(desiredtz = "UTC",
                          start = 0,
                          end = Inf,
                          progressBar = TRUE)
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
    fargs = try({formalArgs(func)})
    if (!inherits(fargs, "try-error")) {
      if (!"verbose" %in% fargs) {
        args$verbose = NULL
      }
    }
    res = do.call(func, args = args)
    hdr = c(hdr, res$header)
    check_data = function(x) "data" %in% names(x)
    if ("P" %in% names(res) && !check_data(res)) {
      res = res$P
    }
    if ("rawxyz" %in% names(res) && !check_data(res)) {
      res$data = res$rawxyz
      colnames(res$data) = c("X", "Y", "Z")
    }
    if (!check_data(res)) {
      if ("data.out" %in% names(res)) {
        names(res)[ names(res) == "data.out" ] = "data"
      }
    }
    res$data = tibble::as_tibble(res$data)
    ndata = names(res$data)
    if ("timestamp" %in% ndata && !"time" %in% ndata) {
      ndata[ ndata == "timestamp" ] = "time"
    }
    names(res$data) = ndata
    if (!"time" %in% names(res$data) && "timestamps1" %in% names(res)) {
      res$data$time = res$timestamps1[,1]
    }
    if ("time" %in% names(res$data)) {
      res$data$time = lubridate::as_datetime(res$data$time)
    }
    if (!"time" %in% names(res$data) && "timestamps2" %in% names(res)) {
      res$data$time = res$timestamps2[,1]
    }
    if (!"time" %in% names(res$data)) {
      warning("time may not be in the data set")
    }
    for (i in c("x", "y", "z")) {
      cn = colnames(res$data)
      if (i %in% cn) {
        cn[cn==i] = toupper(i)
      }
      colnames(res$data) = cn
    }
    hdr = res$header
    if (is.list(hdr) && !"Value" %in% names(hdr)) {
      res$header = list(Value = res$header)
    }
    if (is.matrix(hdr) &&
        ncol(hdr) == 2 && all(colnames(hdr) %in% c("hnames", "hvalues"))) {
      hdr = hdr[, c("hnames", "hvalues")]
      colnames(hdr) = c("Field", "Value")
      hdr = tibble::as_tibble(hdr)
      res$header = hdr
    } else {
      n_values = sapply(res$header$Value, length)
      if (all(n_values == 1)) {
        res$header$Value = unlist(res$header$Value)
        if (is.character(res$header$Value)) {
          res$header$Value = trimws(res$header$Value)
        }
        nn = names(res$header$Value)
        res$header = tibble::as_tibble(res$header,
                                       rownames = NA)
        if (all(rownames(res$header) == 1:nrow(res$header))) {
          res$header$Field = nn
        }
        # res$header = tibble::rownames_to_column(res$header,
        #                                         var = "Field")
      }
    }
    if (!is.data.frame(res$header) ||
        (is.data.frame(res$header) &&
        !all(c("Value", "Field") %in% colnames(res$header)))) {
      warning("Header may not have value/field columns")
    }
    if (all(c("data", "header") %in% names(res))) {
      class(res) = "AccData"
    }
  }
  if (is.data.frame(res$missingness)) {
    res$missingness = tibble::as_tibble(res$missingness)
  }
  # attr(res, "function_to_read") = func
  return(res)
}
