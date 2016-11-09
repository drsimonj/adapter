#' Clean lap information
#'
#' For driver data, add a `lap` number column to the events and streams tibbles
#' and remove all data that precedes the start of the first lap, and comes after
#' the end of the last lap.
#'
#' @param user A list of "user" list objects created by reading functions like
#'   \code{\link{read_logs}}. Also handles single user list objects too.
#' @param n_laps The number of laps that the participant was supposed to do in
#'   the simulation.
#' @param full_list The full user list to be passed on to clean drone data only.
#' @export
#' @return Original user obejct with lap information handled and cleaned.
clean_laps <- function(user, n_laps = 5, full_list) {
  UseMethod("clean_laps")
}
