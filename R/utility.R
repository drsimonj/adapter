#' Check/append slash to string
#'
#' Check if a character string has a slash ("/") on the end. If not, append one
#' and return.
#'
#' @param string Character string to check for, and potentially append, a slash
#'   on the end.
#' @return String of path with "/" on end
#' @export
end_with_slash <- function(string) {
  if (stringr::str_detect(string, "/$"))
    return(string)

  stringr::str_c(string, "/")
}
