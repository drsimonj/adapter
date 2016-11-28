#' Calculate instantaneous speed
#'
#' Take a user object and calculate instantaneous speed from the Unity-produced
#' Vector 3 of the driver's velocity contained in the streams tibble.
#'
#' @export
#' @param
calc_speed <- function(user) {
  UseMethod("calc_speed")
}
