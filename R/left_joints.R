#' Left join a few data frames in a list
#'
#' @param list A list of data frames.
#' @param ... Grouping columns.
#'
#' @return A left-joined data frame.
#'
#' @export
left_joints <- function(list, ...) {
  # add checks on the list
  # - check if empty list
  # - check if less than two elemeents
  # - check if elements are not data frames

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  purrr::imap(list, ~ if_not_in_stop(.x, quoted_cols, .y, arg = "..."))

  joined <- list |>
    purrr::reduce(
      dplyr::left_join,
      by = quoted_cols
    )

  return(joined)
}
