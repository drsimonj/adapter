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

#' @export
clean_events.drone <- function(user, full_list) {
  # Get teammate driver data
  driver <- teammate_data(user, full_list)

  # User driver data to get relevant event times
  event_times <- driver$events %>%
    filter(stringr::str_detect(event, "_(start|end)$")) %>%
    tidyr::separate(event, into = c("event", "line"), sep = "_") %>%
    filter(event != "talk") %>%
    select(time, event, line)

  # Get names of events
  events <- unique(event_times$event)

  # Add colum for each event
  for (e in events) {
    # Add to event tibble
    user$events <- event_times %>%
      # Identify event times and join to stream
      rename(.e = event) %>%
      filter(.e == e) %>%
      full_join(user$events) %>%
      # Create boolean variable for when event is happening
      arrange(time) %>%
      mutate(
        line = ifelse(row_number() == 1, "end", line),
        line = zoo::na.locf(line),
        .e = line == "start") %>%
      # Clean, put at end of data frame and rename
      tidyr::drop_na(event) %>%
      select(-line, -.e, .e) %>%
      rename_(.dots = setNames(".e", e))

    # Add to streams tibble
    user$streams <- event_times %>%
      # Identify event times and join to stream
      filter(event == e) %>%
      full_join(user$streams) %>%
      # Create boolean variable for when event is happening
      arrange(time) %>%
      mutate(
        line = ifelse(row_number() == 1, "end", line),
        line = zoo::na.locf(line),
        event = line == "start") %>%
      # Clean up and put at end of data frame
      tidyr::drop_na(matches("input_")) %>%
      select(-line, -event, event) %>%
      rename_(.dots = setNames("event", e))
  }

  user
}
