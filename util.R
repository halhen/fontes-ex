library(tidyverse)

plot_distribution <- function(df, var, highlight) {
  numeric <- df %>% summarize(numeric = is.numeric({{var}})) %>% pull(numeric)
  
  if (numeric) {
    plot <- ggplot(df, aes({{ var }}, fill = ifelse({{ highlight }}, 'high', 'low'), alpha = ifelse({{ highlight }}, 1, 0.6))) +
      geom_density(data = df %>% filter({{ highlight }})) +
      geom_density(data = df %>% filter(!{{ highlight }})) + # plot the semi-transparent gray on top
      scale_alpha_identity()
  }
  else {
    plot <- df %>%
      group_by({{ highlight }}, {{ var }}) %>%
      summarize(n = n()) %>%
      mutate(p = n / sum(n)) %>%
      ungroup() %>%
      complete({{ highlight }}, {{ var }}, fill = list(n = 0, p = 0)) %>%
      ggplot(aes({{ var }}, p, fill = ifelse({{ highlight }}, 'high', 'low'))) +
        geom_col(position = 'dodge', color = 'black') +
        scale_y_continuous(labels = function(y) paste0(100 * y, '%'))
  }
  
  plot +
    labs(y = '') +
    theme(legend.position = 'none') +
    scale_fill_manual(values = c('high' = 'blue', 'low' = 'lightgray'))
}