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
#'     multiple_means(mtcars,
#'                    group = cyl,
#'                    caption = "My table",
#'                    table = FALSE,
#'                    mpg, disp, hp)
#' @import dplyr
#' @import tidyr
multiple_means <- function(data,
                           group,
                           caption = NULL,
                           table = TRUE,
                           ...) {
  vars_quo <- quos(...)
  group_quo <- enquo(group)
  source_data <- data %>%
    group_by(!!group_quo) %>%
    summarise_at(vars(!!!vars_quo),
                 funs(mean, sd, min, max)) %>%
    gather(key, value, -!!group_quo) %>%
    separate(key, c("Variable", "measure")) %>%
    spread(measure, value) %>%
    mutate_if(is.numeric, round, 1) %>%
    mutate(`Mean (SD) [range]` = stringr::str_glue("{mean} ({sd}) [{min}-{max}]")) %>%
    select(!!group_quo, Variable, `Mean (SD) [range]`) %>%
    drop_na() %>%
    spread(!!group_quo, `Mean (SD) [range]`)
  if (table) {
    return(source_data %>% pander::pandoc.table())
  } else {
    return(source_data)
  }
}
