#' Read user's log files
#'
#' These functions help to read a user's session, events, or stream log files.
#' read_logs() is a wrapper function that uses the default values of all other
#' functions to read all log files into a single list. If this fails, the log
#' files can be read separately using the other functions and by adjusting the
#' variables appropriately.
#'
#' All files that can be read are assumed to be tab-separated values without
#' variable headers. Uniquely, they are assumed to:
#'
#' \describe{
#'  \item{session}{Appear in top-level of user's log file directory; contain
#'  variable names and values}
#'  \item{events}{Appear in top-level of user's log file directory; contain a
#'  timestamp and an event name (tab-separated details about the event can
#'  follow in some cases).}
#'  \item{stream}{Appear in streams/ directory of user's log file directory;
#'  contain a timestamp and a value.}
#' }
#'
#' Most functions are helper functions to read in a single file.
#' read_all_streams, however, reads all the stream files in a directory and
#' merges them into a single tibble. Also, read_logs will make use of all the
#' functions to read all log files into a list.
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
#' @return read_logs with return a list with a "user" S3 class, with three
#'   tibbles (session, events and streams). All others will return a single
#'   \code{\link[tibble]{tibble}}.
#' @name read_logs
#' @export
read_logs <- function(user_dir) {
  session <- read_session(user_dir = user_dir)
  events  <- read_events(user_dir = user_dir)
  streams <- read_all_streams(user_dir = user_dir)

  l <- list(session = session, events = events, streams = streams)
  class(l) <- c("user", class(l))
  l
}

#' @rdname read_logs
#' @inheritParams lubridate::dmy_hms
#' @export
read_session <- function(user_dir,
                         file_name = "session.tsv",
                         col_names = c("var", "info"),
                         tz        = "Australia/Sydney") {

  # Append "/" to directory if required
  user_dir <- end_with_slash(user_dir)

  # Check file exists
  if(!check_file_exists(user_dir, file_name)) {
    return (NA)
  }

  # Read file
  session <- readr::read_tsv(stringr::str_c(user_dir, file_name), col_names = col_names)

  # Convert to wide format so each variable is its own colum
  session <- session %>% tidyr::spread(var, info)

  # Convert date variable to date (if it exists)
  if ("local_start_time" %in% names(session))
    session <- dplyr::mutate_at(session, "local_start_time", lubridate::mdy_hms, tz = tz)

  session
}

#' @rdname read_logs
#' @export
read_events <- function(user_dir,
                        file_name = "events.tsv",
                        col_names = c("time", "event", "detail")) {

  # Append "/" to directory if required
  user_dir <- end_with_slash(user_dir)

  # Check file exists
  if(!check_file_exists(user_dir, file_name)) {
    return (NA)
  }

  events <- readr::read_lines(stringr::str_c(user_dir, file_name)) %>%
    stringr::str_split_fixed("\t", 3) %>%
    tibble::as_tibble()
  names(events) <- c("time", "event", "detail")  # Give variables names
  events <- events %>% dplyr::mutate(detail = stringr::str_split(detail, "\t"),
                                     time   = as.numeric(time))  # Split details into a list (where appropriate)
  events
}

#' @rdname read_logs
#' @export
read_stream <- function(user_dir,
                        file_name,
                        stream_dir = "streams/",
                        is_numeric = TRUE,
                        is_vec3    = FALSE) {

  # Append "/" to directorys if required
  user_dir <- end_with_slash(user_dir)
  stream_dir <- end_with_slash(stream_dir)

  # Check file exists
  if(!check_file_exists(user_dir, stream_dir, file_name)) {
    return (NA)
  }

  # Get variable name
  stream_var <- stringr::str_replace(file_name, "\\..*$", "")

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

#' @rdname read_logs
#' @export
read_all_streams <- function(user_dir,
                             pattern = "tsv$",
                             stream_dir = "streams/",
                             is_numeric = c("input_brake", "input_horizontal", "input_vertical"),
                             is_vec3    = c("position", "rotation", "velocity")) {

  # Append "/" to directorys if required
  user_dir <- end_with_slash(user_dir)
  stream_dir <- end_with_slash(stream_dir)

  # Check streams directory exists
  if(!check_file_exists(user_dir, stream_dir)) {
    return (NA)
  }

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
  is_vec3 <- names(streams)[names(streams) %in% is_vec3]  # include variables only if they exist
  streams <- streams %>% dplyr::mutate_at(is_vec3, vec3_to_vec)

  streams
}


#' Read all users logs from a data directory
#'
#' @param data_dir Directory containing all group data  directories. Must be
#'   organised in particular way.
#' @param clean Should user list be cleaned as well?
#' @param calculate Should derived variables be calculated?
#'
#' @return List of user log-file data lists
#'
#' @export
read_all  <- function(data_dir, clean = TRUE, calculate = TRUE) {
  # Group directories
  group_dirs <- file.path(data_dir, list.files(data_dir), "logs")

  # User directories
  user_dirs <- purrr::map(group_dirs, ~ file.path(., list.files(.))) %>% unlist()

  # User logs
  users <- purrr::map(user_dirs, read_logs)

  # Get user ids
  user_ids <- purrr::map_chr(users, ~ .$session$user_id)

  # Find any duplicated ids
  dup_users <- names(table(user_ids))[table(user_ids) > 1]

  # Remove any users with duplicate ids *FOR NOW
  if (length(dup_users) > 0) {
    warning("Duplicate user ids found. Removing data for the following:", dup_users)
    users <- users[user_ids != dup_users]
    user_ids <- user_ids[user_ids != dup_users]
  }

  # Name user list
  names(users) <- user_ids

  # Add role and group information ------------------------------------------

  # Create a tibble of user info
  user_info <- tibble::tibble(id = user_ids)

  # Split id into relevant info
  user_info <- tidyr::separate(user_info, id, into = c("session_time", "group", "seat"), remove = FALSE)

  # Convert session_time into a date object
  user_info <- user_info %>% dplyr::mutate(session_time = lubridate::ymd_h(session_time, tz = "Australia/Sydney"))

  # Compute role by searching for "2" in the seat
  # becase driver were seated in g2 and d2, drone operators in g1 and d1
  user_info <- user_info %>% dplyr::mutate(role = ifelse(grepl("2", seat), "driver", "drone"))

  # Check that the number of drivers/drones per group makes sense
  ns <- user_info %>%
    dplyr::group_by(session_time, group) %>%
    dplyr::summarise(driver = sum(role == "driver"),
                     drone = sum(role == "drone"))

  if (all(ns$driver == 1)) {
    cat("Check for one driver in each group...\t\tOK\n")
  } else {
    cat("Check for one driver in each group...\t\tERROR\n")
  }

  if (all(ns$drone == 0 | ns$drone == 1)) {
    cat("Check for zero or one drone operators in each group...\t\tOK\n")
  } else {
    cat("Check for zero or one drone operators in each group...\t\tERROR\n")
  }

  # Determine teammates and other session information
  user_teams <- user_info %>%
    dplyr::group_by(session_time, group) %>%
    dplyr::summarise(driver = id[role == "driver"],
                     drone  = ifelse("drone" %in% role, id[role == "drone"], "NA")) %>%
    dplyr::mutate(drone = dplyr::na_if(drone, "NA")) %>%
    tidyr::unite(both, driver, drone, sep = " ", remove = F) %>%
    tidyr::gather(role, id, driver, drone) %>%
    tidyr::drop_na() %>%
    tidyr::separate(both, into = c("team_driver", "team_drone"), sep = " ") %>%
    dplyr::mutate(teammate_id = ifelse(role == "driver", team_drone, team_driver)) %>%
    dplyr::select(-team_driver, -team_drone) %>%
    dplyr::mutate(teammate_id = dplyr::na_if(teammate_id, "NA")) %>%
    dplyr::ungroup()

  # Append information to user and add role class. Also  convert stream data to
  # numeric for drones (as they come out as lists)
  for(i in user_ids) {
    users[[i]]$session <- user_teams %>%
      dplyr::filter(id == i) %>%
      dplyr::bind_cols(users[[i]]$session)

    class(users[[i]]) <- c(users[[i]]$session$role, class(users[[i]]))

    if (is(users[[i]], "drone")) {
      users[[i]]$streams <- users[[i]]$streams %>% purrr::map_df(as.numeric)
    }
  }

  # Add user_list class to final object
  class(users) <- c("user_list", class("user"))

  # If asked, clean logs in user list
  if (clean) {
    users <- clean_laps(users)
    users <- clean_events(users)
  }

  # If asked, calculate new variables
  if (calculate) {
    users <- calc_variables(users)
  }

  users
}
