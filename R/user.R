#' @export
calc_variables.user <- function(users, speed = TRUE) {
  if (speed)
    users <- calc_speed(users)

  users
}

#' @export
calc_speed.user <- function(user) {
  streams <- user$streams

  if (!"velocity" %in% names(streams))
    return(user)

  user$streams <- streams %>% mutate(speed = purrr::map_dbl(velocity, ~ sqrt(sum(.^2))))

  user
}
