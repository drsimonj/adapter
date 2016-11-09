#' Clean lap information
#'
#' Add a `lap` number column to the events and streams tibbles and remove all
#' data that precedes the start of the first lap, and comes after the end of the
#' last lap. Only works for driver data.
#'
#' @param user A user list object created by reading functions like
#'   \code{\link{read_logs}}
#' @param n_laps The number of laps that the participant was supposed to do in
#'   the simulation.
#' @export
#'
clean_laps <- function(user, n_laps = 5) {

  # Handle events data
  user$events <- user$events %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(lap = cumsum(event == "newlap")) %>%  # Add lap numbers
    dplyr::filter(dplyr::between(lap, 1, n_laps)) %>%   # Remove data pre/post test laps
    dplyr::select(lap, dplyr::everything())             # Reorder columns


  # Handle streams data
  start_time <- first(user$events$time)
  end_time   <- last(user$events$time)
  lap_times <- user$events %>%
    dplyr::filter(event == "newlap") %>%
    dplyr::select(time, lap)

  user$streams <- user$streams %>%
    dplyr::filter(dplyr::between(time, start_time, end_time)) %>%  # Remove data pre/post test laps
    dplyr::full_join(lap_times) %>%                                # Add lap numbers
    dplyr::arrange(time) %>%
    dplyr::mutate(lap = cumsum(!is.na(lap))) %>%
    tidyr::drop_na(matches("input_")) %>%
    select(lap, time, everything())                                # Reorder columns

  user
}
