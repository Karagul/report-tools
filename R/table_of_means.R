#' Table showing mean, SD, range, for one or more numeric variables
#' @param data A data frame
#' @param ... Comma-separated list of variables to calculate means for.
#' @param group An optional grouping variable, e.g. trial arm
#'
#' @return A data frame with descriptive statistics (mean, SD, range)
#'     for the selected variables.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     table_of_means(mtcars,
#'                    wt, hp,
#'                    group = cyl)
#'
#'     data(starwars)
#'     table_of_means(starwars, height, mass, birth_year)
#'     table_of_means(starwars, height, mass, birth_year,
#'                    group = gender)
#'     
#' @import dplyr
#' @import tidyr
#' @import labelled
#' @import forcats

# TODO:
#   Rewrite documentation.
#   Check inputs are valid.
#   Find a better way of handling any number of input variables.

table_of_means <- function(data, ..., group) {
  # Capture arguments ==========================================================
  group_quo <- enquo(group)
  vars_quo <- quos(...)
  # Check: do we need grouping?
  if (missing(group)) {
    add_grouping <- function(x) { x }
  } else {
    add_grouping <- function(x) { x %>% group_by(!!group_quo) }
  }
  # Helper functions ===========================================================
  # Function to format summary statistics
  make_changes <- function(x) {
    x %>%
      mutate_if(is.labelled, as_factor) %>%
      mutate_if(is.numeric, round, 1) %>%
      mutate(`Mean (SD) [range]` = stringr::str_glue("{mean} ({sd}) [{min}-{max}]")) %>%
      drop_na()
  }
  
  # Function to extract variable names
  get_name <- function(x) {
    if (!is.null(var_label(x))) {
      return(var_label(x))
    } else {
      return(deparse(substitute(x)))
    }
  }
  
  # Function to extract variable labels
  get_labels <- function(x, vars) {
    x %>%
      select(!!!vars_quo) %>%
      mutate_all(get_name) %>%
      unique() %>%
      gather(var, label) 
  }
  
  
  variable_labels <- data %>%
    select(!!!vars_quo) %>%
    mutate_all(get_name) %>%
    unique() %>%
    gather(var, label) 
  
  # ========================== MAIN FUNCTION STARTS HERE =======================
  # Calculate summary statistics (regardless of input type) --------------------
  summary_statistics <- data %>%
    add_grouping() %>%
    select(!!!vars_quo) %>%
    summarise_all(funs(mean, sd, min, max),
                  na.rm = TRUE)
  
  # If table contains a single variable... =====================================
  if (any(c("mean", "sd", "min", "max") %in% names(summary_statistics))) {
    # If table is ungrouped... -------------------------------------------------
    if (missing(group)) {
      summary_statistics %>%
        make_changes() %>%
        select(`Mean (SD) [range]`) %>%
        return()
      # Otherwise, if table is grouped... --------------------------------------
    } else {
      summary_statistics %>%
        make_changes() %>%
        select(!!group_quo, `Mean (SD) [range]`) %>%
        return()
    }
    # Otherwise, if we're dealing with multiple variables ======================
  } else {
    # If table is ungrouped... -------------------------------------------------
    if (missing(group)) {
      summary_statistics %>%
        gather(key, value) %>%
        tidyr::extract(key, c("var", "measure"),
                       "(.*)_(mean|sd|min|max)$") %>%
        spread(measure, value) %>%
        make_changes() %>%
        select(var, `Mean (SD) [range]`) %>%
        full_join(variable_labels) %>%
        select(-var) %>%
        select(Variable = label,  everything())
    }
    # If table is grouped... ---------------------------------------------------
    else {
      summary_statistics %>%
        gather(key, value, -(!!group_quo)) %>%
        tidyr::extract(key, c("var", "measure"),
                       "(.*)_(mean|sd|min|max)$") %>%
        spread(measure, value) %>%
        make_changes() %>%
        select(!!group_quo, var, `Mean (SD) [range]`) %>%
        spread(!!group_quo, `Mean (SD) [range]`) %>%
        full_join(variable_labels) %>%
        select(-var) %>%
        select(Variable = label,  everything())
    }
  }
}

# Tests
# -----
# data(mtcars)
# table_of_means(mtcars, hp)
# table_of_means(mtcars, hp, wt)
# table_of_means(mtcars, hp, group = cyl)
# table_of_means(mtcars, hp, wt, group = cyl)
