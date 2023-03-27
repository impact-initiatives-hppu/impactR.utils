#' Named group split
#'
#' @param df A data frame.
#' @param group Column to split group by.
#'
#' @return  A split and named list of data frames
#'
#' @export
named_group_split <- function(df, group) {
  group_name <- rlang::as_name(rlang::enquo(group))
  if_not_in_stop(df, group_name, "df", "group")

  names <- dplyr::group_keys(dplyr::group_by(df, {{ group }}))

  names <- dplyr::pull(df, {{ group }})

  df <- dplyr::group_split(df, {{ group }})

  df <- purrr::set_names(df, names)

  return(df)
}
