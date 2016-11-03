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

#' Nest duplicated columns
#'
#' After multiple data frames are joined, there can often a single variable that
#' is replicated and suffixed with .x, .y, .x.x, and so on. This function
#' converts any such variables into a single variable with all original values
#' combined into a list.
#'
#' @param df Data frame
#' @param suffixes Character string to match suffixes. E.g., the default
#'   "\\.[xy]" finds any columns ending with .x or .y
#' @return \code{\link[tibble]{tibble}}
#' @export
nest_duplicated <- function(df, suffixes = "\\.[xy]") {

  # Search string to match any duplicated variables
  search_string <- df %>%
    dplyr::select(dplyr::matches(suffixes)) %>%
    names() %>%
    stringr::str_replace_all(suffixes, "") %>%
    unique() %>%
    stringr::str_c(collapse = "|") %>%
    stringr::str_c("(", ., ")($|", suffixes, ")")

  # Gather duplicated variables and convert names to stems
  df <- df %>%
    tidyr::gather(variable, value, dplyr::matches(search_string)) %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, suffixes, ""))

  # Group by all columns except value to convert duplicated rows into list, then
  # spread by variable (var)
  dots <- names(df)[!stringr::str_detect(names(df), "value")] %>% purrr::map(as.symbol)
  df %>%
    dplyr::group_by_(.dots = dots) %>%
    dplyr::summarise(new = list(value)) %>%
    tidyr::spread(variable, new) %>%
    dplyr::ungroup()
}

#' Compute mean and handle all NA
#'
#' When computing the mean of a vector and removing missing values with na.rm =
#' TRUE, this results in NaN if all values were NA. This function handles such
#' occassions by returning NA instead.
#'
#' @inheritParams base::mean
#' @return Mean of non-missing values or,  if all values were missing, NA.
#' @export
na_mean <- function(x, na.rm = TRUE, ...) {
  if (all(is.na(x)))
    return(NA)

  mean(x, na.rm = na.rm)
}


#' Convert Unity Vector3 to (numeric) vector
#'
#' @param vec3 Character string or vector of character strings in Vector3 format: "(x, y, z)"
#' @param numeric Boolean to
#' @return list of (numeric) vector(s)
#' @export
vec3_to_vec <- function(vec3, is_numeric = TRUE) {
  vec <- vec3 %>%
    stringr::str_replace("\\(", "") %>%
    stringr::str_replace("\\)", "") %>%
    stringr::str_replace(" ", "") %>%
    stringr::str_split(",")
  if (is_numeric)
    vec <- purrr::map(vec, as.numeric)
  vec
}
