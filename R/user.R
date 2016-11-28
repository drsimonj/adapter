#' @export
calc_speed.user <- function(user) {
  streams <- user$streams

  if (!"velocity" %in% names(streams))
    return(user)

  user$streams <- streams %>% mutate(speed = purrr::map_dbl(velocity, ~ sqrt(sum(.^2))))

  user
}
