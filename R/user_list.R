#' @export
clean_laps.user_list <- function(user, n_laps = 5) {

  # Handle drivers
  user <- purrr::map_if(user, ~is(., "driver"), clean_laps, n_laps = n_laps)

  # Handle drones
  user <- purrr::map_if(user, ~is(., "drone"), clean_laps, full_list = user)

  user
}
