#' Left join a list of dataframes
#'
#' @param list A list of data frames.
#' @param ... Columns to join by.
#'
#' @return A left-joined data frame.
#'
#' @export
left_joints <- function(list, ...) {
  # add checks on the list
  # - check if empty list
  # - check if less than two elements
  # - check if elements are not data frames

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  purrr::imap(list, ~ if_not_in_stop(.x, quoted_cols, .y, arg = "..."))

  joined <- purrr::reduce(
    list,
    dplyr::left_join,
    by = quoted_cols
    )

  return(joined)
}

