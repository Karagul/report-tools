#' Table showing mean, SD, range, for multiple numeric variables
#' @param data A data frame
#' @param group The grouping variable, e.g. trial arm
#' @param caption A caption for the table (string)
#' @param table Optional parameter to determine whether the function
#'     should return a formatted \code{pander} or \code{flextable} table
#'     (\code{TRUE}) or the raw data frame (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Comma-separated list of variables to calculate means for.
#'
#' @return A data frame with descriptive statistics (mean, SD, range)
#'     the selected variables; a formatted table with this information,
#'     depending on the \code{table} option.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     table_of_means(mtcars,
#'                    cyl,
#'                    table = FALSE,
#'                    caption = NULL,
#'                    wt, hp)
#'
#'
#'     data(starwars)
#'     table_of_means(starwars,
#'                    gender,
#'                    table = TRUE,
#'                    caption = "Means by gender",
#'                    height, mass, birth_year)
#' @import dplyr
#' @import tidyr

# TODO:
#   Check inputs are valid.
#   Find a better way of handling any number of input variables.

table_of_means <- function(data,
                           group,
                           caption = NULL,
                           table = TRUE,
                           ...) {
  # Check inputs are valid
  if (!(is.data.frame(data))) {
    stop("Supplied data is not a valid data.frame")
  } else if (nargs() <= 5) {
    stop("Too few arguments supplied. This function requires at least 5 arguments.")
  }
  # Capture inputs
  vars_quo <- quos(...)
  group_quo <- enquo(group)
  # Changes that are made regardless of number of input variables
  make_changes <- function(x) {
    x %>%
      mutate_if(is.labelled, as_factor) %>%
      mutate_if(is.numeric, round, 1) %>%
      mutate(`Mean (SD) [range]` = stringr::str_glue("{mean} ({sd}) [{min}-{max}]")) %>%
      drop_na()
  }
  # Process depending on the number of input variables
  number_of_vars <- length(vars_quo)
  if (length(vars_quo) == 0) {
    stop("You must supply at least one variable. None found.")
  } else if (number_of_vars == 1) {
    vars_quo <- quo(...)
    raw <- data %>%
      group_by(!!group_quo) %>%
      select(!!vars_quo) %>%
      summarise_all(funs(mean, sd, min, max)) %>%
      make_changes() %>%
      select(!!group_quo, `Mean (SD) [range]`) %>%
      spread(!!group_quo, `Mean (SD) [range]`)
  } else if (number_of_vars > 1) {
    raw <- data %>%
      group_by(!!group_quo) %>%
      select(!!!vars_quo) %>%     # NOTE: Using "!!!" here because multiple input variables.
      summarise_all(funs(mean, sd, min, max),
                    na.rm = TRUE) %>%
      gather(key, value, -!!group_quo) %>%
      tidyr::extract(key, c("Variable", "measure"),
                     "(.*)_(mean|sd|min|max)$") %>%
      spread(measure, value) %>%
      make_changes() %>%
      select(!!group_quo, Variable, `Mean (SD) [range]`) %>%
      spread(!!group_quo, `Mean (SD) [range]`)
  }
  if (table) {
    return(raw %>% pander::pandoc.table())
  } else {
    return(raw)
  }
}


