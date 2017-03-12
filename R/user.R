#' @export
calc_variables.user <- function(users, anyevent = TRUE, speed = TRUE, distance = TRUE) {

  if (anyevent) users <- calc_anyevent(users)

  if (speed) users <- calc_speed(users)

  if (distance) users <- calc_distance(users)

  users
}

#' @export
calc_anyevent.user <- function(user, event_vars = c("fall", "ice", "fog", "block", "animal")) {
  event_vars <- event_vars[event_vars %in% names(user$events)]

  if (!length(event_vars))
    return(user)

  user$events <- user$events %>%
    select_("time", .dots = event_vars) %>%
    tidyr::gather(key, val, -time) %>%
    group_by(time) %>%
    summarise(any_event = any(val)) %>%
    right_join(user$events)

  user$streams <- user$streams %>%
    select_("time", .dots = event_vars) %>%
    tidyr::gather(key, val, -time) %>%
    group_by(time) %>%
    summarise(any_event = any(val)) %>%
    right_join(user$streams)

  user
}

#' @export
calc_speed.user <- function(user) {
  streams <- user$streams

  if (!"velocity" %in% names(streams))
    return(user)

  user$streams <- streams %>% mutate(speed = purrr::map_dbl(velocity, ~ sqrt(sum(.^2))))

  user
}

#' @export
calc_distance.user <- function(user) {
  streams <- user$streams

  if (!"position" %in% names(streams))
    return(user)

  user$streams <- streams %>%
    # Calculate last position
    mutate(last_position = lag(position)) %>%
    # Subtract last position from position and calculate the travelled distance
    mutate(move = purrr::map2(position, lag(position), ~ ifelse(is.na(.y), 0, .x - .y)),
           distance = purrr::map_dbl(move, ~ sqrt(sum(. ^ 2)))) %>%
    select(-last_position, -move)

  user
}
