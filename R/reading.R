#' Read participant's session file
#'
#' @param user_dir Character string defining the user's log-file directory
#' @param session_file Character string of the session file name
#' @inheritParams readr::read_tsv
#' @param ... Additional arguments to pass to \code{\link[readr]{read_tsv}}
#' @return A \code{\link[tibble]{tibble}}
#' @export
read_session <- function(user_dir,
                         session_file = "session.tsv",
                         col_names = c("var", "info"),
                         ...) {

  # Append "/" to directory if required
  if (!stringr::str_detect(user_dir, "/$"))
    user_dir <- stringr::str_c(user_dir, "/")

  # Read file
  readr::read_tsv(stringr::str_c(user_dir, session_file), col_names = col_names)
}
