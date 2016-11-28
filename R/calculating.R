#' Calculate instantaneous speed
#'
#' Take a user object and calculate instantaneous speed from the Unity-produced
#' Vector 3 of the driver's velocity contained in the streams tibble.
#'
#' @export
#' @param user A user list object
#' @return The user object with a speed column in the streams tibble if a
#'   velocity column is present.
calc_speed <- function(user) {
  UseMethod("calc_speed")
}
