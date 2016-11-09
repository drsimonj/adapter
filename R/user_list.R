#' @export
clean_laps.user_list <- function(user, n_laps = 5) {
  purrr::map_if(user, ~is(., "driver"), clean_laps, n_laps = n_laps)
}
