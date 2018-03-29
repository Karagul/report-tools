#' Table showing median, IQR, and range, for one or more numeric variables
#' @param data A data frame
#' @param group The grouping variable, e.g. trial arm
#' @param ... Comma-separated list of variables to calculate medians for.
#'
#' @return A data frame with descriptive statistics (median, IQR, range)
#'     for the selected variables.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     table_of_medians(mtcars,
#'                      group = cyl,
#'                      wt, hp)
#'
#'     data(starwars)
#'     table_of_means(starwars,
#'                    group = gender,
#'                    height, mass, birth_year)
#' @import dplyr
#' @import tidyr
#' @import labelled
#' @import forcats

# TODO:
#   Rewrite documentation.
#   Check inputs are valid.
#   Find a better way of handling any number of input variables.

table_of_medians <- function(data, group, ...) {
    # Capture inputs
    vars_quo <- quos(...)
    group_quo <- enquo(group)
    # Changes that are made regardless of number of input variables
    make_changes <- function(x) {
        x %>%
            mutate_if(is.labelled, as_factor) %>%
            mutate_if(is.numeric, round, 1) %>%
            mutate(`Median (IQR) [range]` = stringr::str_glue("{median} ({IQR}) [{min}-{max}]")) %>%
            drop_na()
    }
    # Process depending on the number of input variables
    number_of_vars <- length(vars_quo)
    if (length(vars_quo) == 0) {
        stop("You must supply at least one variable. None found.")
    } else if (number_of_vars == 1) {
        # =================== MAKE TABLE FOR SINGLE VARIABLE ================== #
        vars_quo <- quo(...)
        raw <- data %>%
            group_by(!!group_quo) %>%
            select(!!vars_quo) %>%
            summarise_all(funs(median, IQR, min, max)) %>%
            make_changes() %>%
            select(!!group_quo, `Median (IQR) [range]`)
    } else if (number_of_vars > 1) {
        # ====================== MAKE TABLE FOR MULTIPLE VARIABLES =========== #
    get_name <- function(x) {
      if (!is.null(var_label(x))) {
        return(var_label(x))
      } else {
        return(deparse(substitute(x)))
      }
    }

    labels <- data %>%
      select(!!!vars_quo) %>%
      mutate_all(get_name) %>%
      unique() %>%
      gather(var, label)

    raw <- data %>%
      group_by(!!group_quo) %>%
      select(!!!vars_quo) %>%
      summarise_all(funs(median, IQR, min, max),
                    na.rm = TRUE) %>%
      gather(key, value, -!!group_quo) %>%
      tidyr::extract(key, c("var", "measure"),
                     "(.*)_(median|IQR|min|max)$") %>%
      spread(measure, value) %>%
      make_changes() %>%
      select(!!group_quo, var, `Median (IQR) [range]`) %>%
      spread(!!group_quo, `Median (IQR) [range]`) %>%
      full_join(labels) %>%
      select(-var) %>%
      select(Variable = label,  everything())
  }
    return(raw)
}