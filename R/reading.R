#' Read a user's single log file
#'
#' Read a user's session or events log file that are expected to be contained in
#' the top-level of the user's log files directory, or stream files expected to
#' be in the streams/ directory.
#'
#' The files that can be read are assumed to be tab-separated values without
#' variable headers. Uniquely, they are assumed to contain:
#'
#' \describe{
#'  \item{session}{Variable names and values}
#'  \item{events}{A timestamp and an event name. Tab-separated details about the
#'  event can follow in some cases.}
#'  \item{stream}{A timestamp and a value.}
#' }
#'
#' All functions read in a single file except for read_all_streams, which reads
#' all the stream files in a directory and merges them into a single tibble.
#'
#' @param user_dir Character string defining the user's log-file directory
#' @param file_name Character string of the file name. Must include extension such as .tsv
#' @param stream_dir Character string defining the directory in which stream log
#'   files exist.
#' @param col_names Vector of column names to be used.
#' @param is_numeric Indicate whether a stream variable is numeric and,
#'   therefore, should be convereted to numeric. Can be a boolean value
#'   (TRUE/FALSE) or a character vector of variable names to convert if present.
#' @param is_vec3 Indicate whether a stream variable is a Vector3 value in Unity
#'   and, therefore, should be convereted to a vector of 3 values. Can be a
#'   boolean value (TRUE/FALSE) or a character vector of variable names to
#'   convert if present.
#' @inheritParams base::list.files
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
                        is_numeric = TRUE,
                        is_vec3    = FALSE) {

  # Get variable name
  stream_var <- stringr::str_replace(file_name, "\\..*$", "")

  # Append "/" to directorys if required
  user_dir <- end_with_slash(user_dir)
  stream_dir <- end_with_slash(stream_dir)

  stream <- stringr::str_c(user_dir, stream_dir, file_name) %>%
              readr::read_tsv(col_names = c("time", stream_var), col_types = "dc")  # Note need to import all values as characters

  # Handle vec3 OR numeric, but not both (if boolean values used)
  if(is.logical(is_vec3) && is_vec3) {
    is_numeric <- FALSE
    warning("is_numeric and is_vec3 both set to TRUE; is_vec3 will be used.")
  }

  # Handle numeric variables
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

  # Handle vector 3 variables
  if (is.character(is_vec3)) {
    is_vec3 <- stream_var %in% is_vec3
  }

  if (is.logical(is_vec3)) {
    if (is_vec3) {
      stream[[stream_var]] <- vec3_to_vec(stream[[stream_var]])
    }
  } else {
    stop("is_vec3 is not a valid boolean or character string format.")
  }

  stream
}

#' @rdname read_log
#' @export
read_all_streams <- function(user_dir,
                             pattern = "tsv$",
                             stream_dir = "streams/",
                             is_numeric = c("input_brake", "input_horizontal", "input_vertical"),
                             is_vec3    = c("position", "rotation", "velocity")) {

  # Return NA if the stream directory does not exist
  if(!file.exists(stringr::str_c(user_dir, stream_dir))) {
    warning("Stream directory does not exist")
    NA
  }

  # Append "/" to directorys if required
  user_dir <- end_with_slash(user_dir)
  stream_dir <- end_with_slash(stream_dir)

  # Find all stream files
  stream_files <- list.files(stringr::str_c(user_dir, stream_dir), pattern = "tsv$")

  # Read in all stream files and merge into single tibble
  streams <- purrr::map(stream_files,
                        ~ read_stream(user_dir = user_dir,
                                      file_name = .,
                                      stream_dir = stream_dir,
                                      is_numeric = is_numeric,
                                      # Do vec_3 conversions later so nest_duplicated will work
                                      is_vec3    = FALSE)) %>%
              # Merge all streams into a single data frame
              purrr::map(~ tibble::rownames_to_column(., var = "i")) %>%
              purrr::reduce(dplyr::full_join, by = "i") %>%
              # Compute time as the mean from each stream
              nest_duplicated() %>%
              dplyr::mutate_if(is.list, dplyr::funs(map_dbl(., na_mean))) %>%
              # Select/order/arrange columns
              dplyr::select(time, dplyr::everything(), -i) %>%
              dplyr::arrange(time)

  # Convert Vector 3 variables to lists
  streams <- streams %>% dplyr::mutate_at(c("position", "rotation", "velocity"), vec3_to_vec)

  streams
}
