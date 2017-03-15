#' Create a single session, events, or streams tibble from list of user data
#'
#' Simulation data read in using \code{\link{read_all}}() is available as a user
#' list, with each list element being a list with three tibbles.
#' \code{bind_tbl}() will take the user list and the name of which of the three
#' tibble to work with. It returns a single tibble which is all users tibbles of
#' the specific type bound together. A `uid` column is added.
#'
#' @param user_list List of user data created via a method such as
#'   \code{\link{read_all}}()
#' @param tbl_name Unquoted name of tibble to extract. Must be `session`,
#'   `events`, or `streams`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Given a user-list of data `d`...
#' bind_tbl(d, session)
#' bind_tbl(d, events)
#' bind_tbl(d, streams)
#' }
bind_tbl <- function(user_list, tbl_name) {
  UseMethod("bind_tbl")
}

#' @export
bind_tbl.default <- function(user_list, tbl_name) {
  if (!is(user_list, "user_list"))
    warning("bind_tbl() is intended to be used for objects with the class 'user_list'. ",
            "See ?adapter::read_all for help. ",
            "Will attempt to run anyway...")

  tbl_name <- deparse(substitute(tbl_name))
  tbl_name <- match.arg(tbl_name, c("session", "events", "streams"))

  if (tbl_name == "session")
    return(rename(purrr::map_df(user_list, "session"), uid = user_id))

  purrr::map_df(user_list, ~ mutate(.[[tbl_name]], uid = .$session$user_id))

}


#' Get data for drivers or drones only from a mixed user list
#'
#' A list of user data generated via methods like \code{\link{read_all}}()
#' contains a mix of drivers and drone operators. \code{get_drivers}() and
#' \code{get_drones}() extract only users of a certain type from such a mixed
#' list.
#'
#' @param user_list List of user data created via a method such as
#'   \code{\link{read_all}}()
#' @param type String of the type of user to keep. Must be one of "driver" or
#'   "drone".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Given a user-list of data `d`...
#' get_drivers(d)
#' get_drones(d)
#' }
get_type <- function(user_list, type) {
  UseMethod("get_type")
}

#' @export
get_type.default <- function(user_list, type) {
  if (!is(user_list, "user_list"))
    warning("bind_tbl() is intended to be used for objects with the class 'user_list'. ",
            "See ?adapter::read_all for help. ",
            "Will attempt to run anyway...")

  type <- match.arg(type, c("driver", "drone"))

  tmp <- purrr::keep(user_list, is, type)
  class(tmp) <- c("user_list", class(tmp))
  tmp
}

#' @export
#' @rdname get_type
get_drivers <- function(user_list) {
  get_type(user_list, "driver")
}

#' @export
#' @rdname get_type
get_drones <- function(user_list) {
  get_type(user_list, "drone")
}
