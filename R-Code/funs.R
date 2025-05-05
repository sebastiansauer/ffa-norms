
convert_to_norm_value <- function(score, data_vec, mu = 0, sigma = 1) {
  
  out <- ((score - mean(data_vec)) / sd(data_vec)) * sigma + mu
  return(out)
  
}





compute_all_norms <- function(x, min_score = 0, max_score = max(x), by = .5){
  
  x_ecdf <- ecdf(x)
  
  out <-
    tibble(
      score = seq(from = min_score, to = max_score, by = by),
      perc_rank = x_ecdf(score),
      z = map_dbl(score, ~ convert_to_norm_value(.x, data_vec = x)),
      stanine = map_dbl(score, ~ convert_to_norm_value(.x, data_vec = x, mu = 5, sigma = 2)),
      T = map_dbl(score, ~ convert_to_norm_value(.x, data_vec = x, mu = 50, sigma = 10)),
      perc_normal = map_dbl(z, pnorm)
    ) %>% 
    mutate(stanine = case_when(
      stanine > 9 ~ 9,
      stanine < 1 ~ 1,
      TRUE ~ stanine
    ))
  return(out)
}





plot_fmi_descriptives <- function(data, var) {
  
  d_prepped <- 
   data %>% 
    select(ends_with("_mean"), {{var}}) %>%
    drop_na() %>% 
    group_by({{var}}) %>%
    describe_distribution(iqr = FALSE, range = TRUE, quartiles = TRUE) %>% 
    select(Variable, Mean, SD, .group) %>% 
    mutate(group = str_remove(.group, "^.+=")) %>% 
    select(-.group)
  
  d_prepped %>%  
    #pivot_longer(-c(Variable, group)) %>% 
    ggplot(aes(x = group, color = Variable)) +
    geom_errorbar(aes(ymin = Mean-(2*SD), ymax = Mean+(2*SD)), width = .1) +
    geom_point2(aes(y = Mean), alpha = .7, size = 2) +
    facet_wrap(~ Variable) +
    labs(caption = "Error bars show mean ± 2SD") +
    coord_flip() +
    theme(legend.position = "bottom") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 3))
}






describe_fmi_stats <- function(data, var){
  
  data %>% 
    select(ends_with("_mean"), {{var}}) %>%
    # filter(Geschlecht %in% c("weiblich", "männlich")) %>% 
    #mutate(Geschlecht = as.character(Geschlecht)) %>% 
    drop_na() %>% 
    group_by({{var}}) %>%
    describe_distribution(iqr = FALSE, range = TRUE, quartiles = TRUE) %>% 
    mutate(Mean_01 = Mean/3) %>% 
    relocate(Mean_01, .after = Mean) %>% 
    arrange(Variable) %>% 
    display() 
}



