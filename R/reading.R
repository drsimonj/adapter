#' Read a user's single log file
#'
#' Read a user's session or events log file that are expected to be contained in
#' the top-level of the user's log files directory, or a single stream file
#' expected to be in the streams/ directory. These files are assumed to be
#' tab-separated values without variable headers. Uniquely, they are assumed to
#' contain:
#'
#' \describe {
#'  \item{session}{Variable names and values}
#'  \item{events}{A timestamp and an event name. Tab-separated details about the
#'  event can follow in some cases.}
#'  \item{stream}{A timestamp and a value.}
#' }
#'
#' @param user_dir Character string defining the user's log-file directory
#' @param file_name Character string of the file name. Must include extension such as .tsv
#' @param stream_dir Character string defining the directory in which stream log
#'   files exist.
#' @param col_names Vector of column names to be used.
#' @param is_numeric Indicate whether a stream variable is numeric and,
#'   therefore, should be convereted to numeric. Can be a boolean value
#'   (TRUE/FALSE) or a character vector of numeric variable names to seach in.
#' @return \code{\link[tibble]{tibble}}
#' @name read_log
NULL

#' @rdname read_log
#' @export
read_session <- function(user_dir,
                         file_name = "session.tsv",
                         col_names = c("var", "info")) {

  # Append "/" to directory if required
  user_dir <- end_with_slash(user_dir)

  # Read file
  readr::read_tsv(stringr::str_c(user_dir, file_name), col_names = col_names)
}

#' @rdname read_log
#' @export
read_events <- function(user_dir,
                        file_name = "events.tsv",
                        col_names = c("time", "event", "detail")) {

  # Append "/" to directory if required
  user_dir <- end_with_slash(user_dir)

  events <- readr::read_lines(stringr::str_c(user_dir, file_name)) %>%
    stringr::str_split_fixed("\t", 3) %>%
    tibble::as_tibble()
  names(events) <- c("time", "event", "detail")  # Give variables names
  events <- events %>% dplyr::mutate(detail = stringr::str_split(detail, "\t"))  # Split details into a list (where appropriate)
  events
}

#' @rdname read_log
#' @export
read_stream <- function(user_dir,
                        file_name,
                        stream_dir = "streams/",
                        is_numeric = TRUE) {

  # Get variable name
  stream_var <- stringr::str_replace(file_name, "\\..*$", "")

  # Append "/" to directorys if required
  user_dir <- end_with_slash(user_dir)
  stream_dir <- end_with_slash(stream_dir)

  stream <- stringr::str_c(user_dir, stream_dir, file_name) %>%
              readr::read_tsv(col_names = c("time", stream_var), col_types = "dc")  # Note need to import all values as characters

  if (is.character(is_numeric)) {
    is_numeric <- stream_var %in% is_numeric
  }

  if (is.logical(is_numeric)) {
    if (is_numeric) {
      stream[[stream_var]] <- as.numeric(stream[[stream_var]])
    }
  } else {
    stop("is_numeric is not a valid boolean or character string format.")
  }

  stream
}
