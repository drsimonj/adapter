#' @export
clean_laps.drone <- function(user, full_list) {

  # Get teammate driver data
  teammate_id <- user$session$teammate_id
  driver <- full_list[[teammate_id]]

  # Use driver data to extract lap time information
  start_time <- first(driver$events$time)
  end_time   <- last(driver$events$time)

  lap_times  <- driver$events %>%
    dplyr::filter(event == "newlap") %>%
    dplyr::select(time, lap)

  # Handle events
  user$events <- user$events %>%
    dplyr::filter(dplyr::between(time, start_time, end_time)) %>%  # Remove data pre/post test laps
    dplyr::full_join(lap_times) %>%                                # Add lap numbers
    dplyr::arrange(time) %>%
    dplyr::mutate(lap = cumsum(!is.na(lap))) %>%
    tidyr::drop_na(matches("event")) %>%
    select(lap, time, everything())                                # Reorder columns

  # Handle streams data
  user$streams <- user$streams %>%
    dplyr::filter(dplyr::between(time, start_time, end_time)) %>%  # Remove data pre/post test laps
    dplyr::full_join(lap_times) %>%                                # Add lap numbers
    dplyr::arrange(time) %>%
    dplyr::mutate(lap = cumsum(!is.na(lap))) %>%
    tidyr::drop_na(matches("input_")) %>%
    select(lap, time, everything())                                # Reorder columns

  user
}
