#' @export
calc_variables.user <- function(users, speed = TRUE, distance = TRUE) {
  if (speed)
    users <- calc_speed(users)

  if (distance)
    users <- calc_distance(users)

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
