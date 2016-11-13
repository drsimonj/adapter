#' @export
clean_laps.driver <- function(user, n_laps = 5) {

  # Handle events data
  user$events <- user$events %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(lap = cumsum(event == "newlap")) %>%  # Add lap numbers
    dplyr::filter(dplyr::between(lap, 1, n_laps)) %>%   # Remove data pre/post test laps
    dplyr::select(lap, dplyr::everything())             # Reorder columns

  # Handle streams data
  start_time <- first(user$events$time)
  end_time   <- last(user$events$time)
  lap_times  <- user$events %>%
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


#' @export
clean_events.driver <- function(user) {

  # Get relevant event times
  event_times <- user$events %>%
    filter(stringr::str_detect(event, "_(start|end)$")) %>%
    tidyr::separate(event, into = c("event", "line"), sep = "_") %>%
    filter(event != "talk") %>%   # Remove talking from events list
    #tidyr::unnest(detail) %>%   # These two lines would be useful if there are multiple events with same event name but different details
    #tidyr::unite(event, event, detail)
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
      # Put at end of data frame and rename
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
