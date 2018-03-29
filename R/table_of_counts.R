#' Table showing counts for one or more categorical variables.
#' @param data A data frame
#' @param ... Comma-separated list of categorical variables to include.
#'
#' @return A \code{flextable} with descriptive statistics (N, percent, range)
#'     for the selected variables.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     table_of_counts(mtcars, cyl, wt, hp)
#'
#' @import dplyr
#' @import tidyr
#' @import labelled
#' @import flextable

# TODO:
#   Add option to display counts by group.
#   Check inputs are valid.

table_of_counts <- function(data, ...) {
  vars_quo <- quos(...)
  if (length(vars_quo) == 1) {
    vars_quo <- quo(...)
    data <- select(data, !!vars_quo)
  } else {
    data <- select(data, !!!vars_quo)
  }
  data %>%
    mutate_if(is.labelled, as_factor) %>%
    gather(key, value) %>%
    mutate(value = replace_na(value, "Missing")) %>%
    count(key, value) %>%
    group_by(key) %>%
    mutate(total = sum(n),
           percent = (n / total) * 100,
           min = min(n),           max = max(n),
           value = str_replace(value, "^[0-9]+\\. ", "")) %>%
    ungroup() %>%
    mutate(n_per = str_glue("{n} ({round(percent, 1)}%)"),
           range = str_glue("{min}-{max}")) %>%
    select(key, value, n_per, range) %>%
    arrange(key, value) %>%
    flextable::flextable() %>%
    merge_v(j = "key") %>%
    set_header_labels(key = "Variable",
                      value = "Category",
                      n_per = "N (%)",
                      range = "Range") %>%
    autofit() %>%
    theme_vanilla()
}
