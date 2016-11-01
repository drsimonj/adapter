#' Read participant's session file
#'
#' @param participant_dir Character string defining the participant's directory
#'   of log files
#' @param session_file Character string of the session file name
#' @inheritParams readr::read_tsv
#' @param ... Additional arguments to pass to \code{\link[readr]{read_tsv}}
#' @return A \code{\link[tibble]{tibble}}
#' @export
read_session <- function(participant_dir,
                         session_file = "session.tsv",
                         col_names = c("var", "info"),
                         ...) {
  readr::read_tsv(stringr::str_c(participant_dir, session_file), col_names = var_names)
}
