#' Calculate variables
#'
#' A wrapper function to apply variable calculation functions to a user list
#'
#' @export
#' @param users A user or list of users created by reading functions like
#'   \code{\link{read_logs}}.
#' @param speed Should speed be calculated using \code{\link{calc_speed}}
calc_variables <- function(users, speed = TRUE) {
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
