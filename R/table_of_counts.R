#' Table showing counts for one or more categorical variables.
#' @param data A data frame
#' @param group An optional grouping variable.
#'
#' @return A \code{flextable} with descriptive statistics (N, percent, range)
#'     for all variables in the supplied \code{data.frame}.
#' @export
#'
#' @examples
#'     data(mtcars)
#'     mtcars %>%
#'       select(cyl, vs) %>%
#'       table_of_counts()
#'       
#'     mtcars %>%
#'       select(cyl, vs, am) %>%
#'       table_of_counts(cyl)
#'
#' @import dplyr
#' @import tidyr
#' @import labelled
#' @import flextable

# TODO:
#   Check inputs are valid.

table_of_counts <- function(data, group, maketable = TRUE) {
  group_quo <- enquo(group)
  # Helper functions ===========================================================
  # Function to extract variable names
  get_name <- function(x) {
    if (!is.null(var_label(x))) {
      return(var_label(x))
    } else {
      return(deparse(substitute(x)))
    }
  }
  # Function to extract variable labels
  get_labels <- function(x) {
    x %>%
      mutate_all(get_name) %>%
      unique() %>%
      gather(var, label) 
  }
  # Function to count number of non-grouping variables
  count_var <- function(x) {
    x %>%
      select(-!!group_quo) %>%
      names() %>%
      length()
  }
  # ========================= MAIN FUNCTION STARTS HERE ========================
  # If data are NOT grouped ----------------------------------------------------
  # This presents N (percent) and range for a single variable.
  if (missing(group)) {
    data_for_table <- data %>%
      mutate_if(is.labelled, as_factor) %>%
      gather(key, value) %>%
      mutate(value = tidyr::replace_na(value, "Missing")) %>%
      count(key, value) %>%
      group_by(key) %>%
      mutate(total   = sum(n),
             percent = (n / total) * 100,
             min     = min(n), 
             max     = max(n)) %>%
      ungroup() %>%
      mutate(n_per = str_glue("{n} ({round(percent, 1)})"),
             range = str_glue("{min}-{max}"),
             category = str_replace(value, "^[0-9]+\\. ", ""))  %>%
      select(var = key, category, n_per, range) %>%
      left_join(get_labels(data)) %>%
      select(label, category, n_per, range) 

      if (maketable)  {
          data_for_table %>%
              flextable::regulartable() %>%
              merge_v(j = "label") %>%
              set_header_labels(label    = "Variable",
                                category = "Category",
                                n_per    = "N (%)",
                        range    = "Range") %>%
              merge_v(part = "header") %>%
              autofit() %>%
              theme_booktabs()
      } else {
          return(data_for_table)
      }
  } else {
    # If data ARE grouped ------------------------------------------------------
    data_for_table <- data %>%
      mutate_if(is.labelled, as_factor) %>%
      gather(key, value, -(!!group_quo))  %>%
      mutate(value = replace_na(value, "Missing")) %>%
      group_by(!!group_quo) %>%
      count(key, value) %>%
      group_by(key, value) %>%
      mutate(total    = sum(n),
             percent  = (n / total) * 100,
             min      = min(n),  
             max      = max(n),
             category = str_replace(value, "^[0-9]+\\. ", ""))  %>%
      group_by(!!group_quo) %>%
      mutate(n_per = str_glue("{n} ({sprintf(\"%0.1f\", round(percent, 1))})")) %>%
      rename(var = key) %>%
      left_join(get_labels(data)) %>%
      select(label, category, !!group_quo, n_per, total) %>%
      spread(!!group_quo, n_per) %>%
      select(label, category, names(.)[-grep("total", names(.))], total) 
    
      if (maketable) {
          data_for_table %>%
              flextable::regulartable() %>%
              merge_v(j = "label") %>%
              set_header_labels(label    = "Variable",
                                category = "Category",
                                n_per    = "N (%)",
                                range    = "Range",
                                total = "Total") %>%
              merge_v(part = "header") %>%
              autofit() %>%
              theme_booktabs()
      } else {
          return(data_for_table)
      }
  }
}
