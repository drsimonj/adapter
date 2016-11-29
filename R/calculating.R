#' Calculate variables
#'
#' A wrapper function to apply variable calculation functions to a user list
#'
#' @export
#' @param users A user or list of users created by reading functions like
#'   \code{\link{read_logs}}.
#' @param anyevent Should a boolean column be created to indicates whether any
#'   event is happening via \code{\link{calc_anyevent}}
#' @param speed Should speed be calculated using \code{\link{calc_speed}}
#' @param distance Should distance travelled since last time point be calculated using \code{\link{calc_distance}}
calc_variables <- function(users, anyevent = TRUE, speed = TRUE, distance = TRUE) {
  UseMethod("calc_variables")
}

#' Calculate whether any event is happening
#'
#' Function to calculate whether any event is happening at a given time point.
#' This is a cenvenience instead of constantly writing things like `if (is | fog
#' | ...)`
#'
#' @export
#' @param user A user list object
#' @param event_vars Names of boolean variables indicating whether an event is
#'   happening
#' @return The user object with an any_event column in the events and streams
#'   tibbles.
calc_anyevent <- function(user, event_vars = c("fall", "ice", "fog", "block", "animal")) {
  UseMethod("calc_anyevent")
}

#' Calculate instantaneous speed
#'
#' Take a user object and calculate instantaneous speed from the Unity-produced
#' Vector 3 of the driver's velocity contained in the streams tibble.
#'
#' @export
#' @inheritParams calc_anyevent
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
#' @inheritParams calc_anyevent
#' @return The user object with a distance column in the streams tibble if a
#'   position column is present.
calc_distance <- function(user) {
  UseMethod("calc_distance")
}
