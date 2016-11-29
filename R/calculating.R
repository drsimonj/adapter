#' Calculate variables
#'
#' A wrapper function to apply variable calculation functions to a user list
#'
#' @export
#' @param users A user or list of users created by reading functions like
#'   \code{\link{read_logs}}.
#' @param speed Should speed be calculated using \code{\link{calc_speed}}
#' @param distance Should distance travelled since last time point be calculated using \code{\link{calc_distance}}
calc_variables <- function(users, speed = TRUE, distance = TRUE) {
  UseMethod("calc_variables")
}

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

#' Calculate distance travelled since last time point
#'
#' Take a user object and use the position data to calculate the distance in the
#' sim world they travelled since the last recorded time point.
#'
#' @export
#' @inheritParams calc_speed
#' @return The user object with a distance column in the streams tibble if a
#'   position column is present.
calc_distance <- function(user) {
  UseMethod("calc_distance")
}
