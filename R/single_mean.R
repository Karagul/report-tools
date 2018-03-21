# Table showing mean, SD, range, for a single variable
single_variable_mean <- function(df, group, var, caption = NULL) {
  var_quo <- enquo(var)
  group_quo <- enquo(group)
  df %>%
    group_by(!!group_quo) %>%
    summarise(mean_age = mean(!!var_quo),
              sd_age = sd(!!var_quo),
              min_age = min(!!var_quo),
              max_age = max(
                !!var_quo)) %>%
    mutate_all(round, 1) %>%
    mutate(`Mean (SD) [range]` = str_glue("{mean_age} ({sd_age}) [{min_age}-{max_age}]")) %>%
    select(`Trial arm` = !!group_quo,
           `Mean (SD) [range]`) %>%
    drop_na() %>%
    pandoc.table(caption = caption)
}
