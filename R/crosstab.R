#' Cross-tabulation for two categorical variables.
#' @param data A data frame
#' @param x y  Categorical variables.
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
#' @import janitor

# data %>%
#   mutate_if(is.labelled, as_factor) %>%
#   tabyl(randomised, sex) %>%
#   adorn_totals("row") %>%
#   adorn_percentages("all") %>%
#   adorn_pct_formatting() %>%
#   adorn_ns()

# crosstab <- function(data, x, y) {
#   x <- enquo(x)
#   y <- enquo(y)
#   data %>%
#     select(!!x, !!y) %>%
#     mutate_if(is.labelled, as_factor) %>%
#     count(!!x, !!y) %>%
#     mutate(percent = round(prop.table(n) * 100, 1),
#            cell_content = stringr::str_glue("{n} ({percent}%)")) %>%
#     select(-n, -percent) %>%
#     spread(!!x, n)
# }

# crosstab(data, randomised, sex)
