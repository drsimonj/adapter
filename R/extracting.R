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
