#' Table showing median, IQR, and range, for one or more numeric variables
#' @param data A data frame
#' @param ... Comma-separated list of variables to calculate medians for.
#' @param group An optional grouping variable, e.g. trial arm
#'
#' @return A data frame with descriptive statistics (median, IQR, range)
#'     for the selected variables.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     table_of_medians(mtcars, wt, hp)
#'     table_of_medians(mtcars, wt, hp, group = cyl)
#'
#'     data(starwars)
#'     table_of_means(starwars,
#'                    height, mass, birth_year)
#'                    
#'     table_of_means(starwars,
#'                    height, mass, birth_year, 
#'                    group = gender)
#' @import dplyr
#' @import tidyr
#' @import labelled
#' @import forcats

# TODO:
#   Rewrite documentation.
#   Check inputs are valid.
#   Find a better way of handling any number of input variables.
#   Find way of using single function for means and medians.

table_of_medians <- function(data, ..., group) {
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
      mutate(`Median (IQR) [range]` = stringr::str_glue("{median} ({IQR}) [{min}-{max}]")) %>%
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
    summarise_all(funs(median, IQR, min, max),
                  na.rm = TRUE)
  
  # If table contains a single variable... =====================================
  if (any(c("median", "IQR", "min", "max") %in% names(summary_statistics))) {
    # If table is ungrouped... -------------------------------------------------
    if (missing(group)) {
      summary_statistics %>%
        make_changes() %>%
        select(`Median (IQR) [range]`) %>%
        return()
      # Otherwise, if table is grouped... --------------------------------------
    } else {
      summary_statistics %>%
        make_changes() %>%
        select(!!group_quo, `Median (IQR) [range]`) %>%
        return()
    }
    # Otherwise, if we're dealing with multiple variables ======================
  } else {
    # If table is ungrouped... -------------------------------------------------
    if (missing(group)) {
      summary_statistics %>%
        gather(key, value) %>%
        tidyr::extract(key, c("var", "measure"),
                       "(.*)_(median|IQR|min|max)$") %>%
        spread(measure, value) %>%
        make_changes() %>%
        select(var, `Median (IQR) [range]`) %>%
        full_join(variable_labels) %>%
        select(-var) %>%
        select(Variable = label,  everything())
    }
    # If table is grouped... ---------------------------------------------------
    else {
      summary_statistics %>%
        gather(key, value, -(!!group_quo)) %>%
        tidyr::extract(key, c("var", "measure"),
                       "(.*)_(median|IQR|min|max)$") %>%
        spread(measure, value) %>%
        make_changes() %>%
        select(!!group_quo, var, `Median (IQR) [range]`) %>%
        spread(!!group_quo, `Median (IQR) [range]`) %>%
        full_join(variable_labels) %>%
        select(-var) %>%
        select(Variable = label,  everything())
    }
  }
}

# Tests
# -----
# data(mtcars)
# table_of_medians(mtcars, hp)
# table_of_medians(mtcars, hp, wt)
# table_of_medians(mtcars, hp, group = cyl)
# table_of_medians(mtcars, hp, wt, group = cyl)

