#' @export
clean_laps.user_list <- function(user, n_laps = 5) {

  # Keep original class
  oc <- class(user)

  # Handle drivers
  user <- purrr::map_if(user, ~is(., "driver"), clean_laps, n_laps = n_laps)

  # Handle drones
  user <- purrr::map_if(user, ~is(., "drone"), clean_laps, full_list = user)

  # Reapply original class
  class(user) <- oc

  user
}

#' @export
clean_events.user_list <- function(user) {

  # Keep original class
  oc <- class(user)

  # Handle drivers
  user <- purrr::map_if(user, ~is(., "driver"), clean_events)

  # Handle drones
  user <- purrr::map_if(user, ~is(., "drone"), clean_events, full_list = user)

  # Reapply original class
  class(user) <- oc

  user
}

#' @export
calc_variables.user_list <- function(users, speed = TRUE) {
  purrr::map(users, calc_variables, speed = speed)
}
