library(tidyverse)



gini_sample = function(x, 
                       y,
                       sample_steps,
                       n_samples) {

  
gini_v = rep(NA, length(sample_steps)*n_samples)  
size = rep(sample_steps, each=n_samples)

for (i in sample_steps) {
  message(paste("At", i, "size. Going strong...", sep=" "))
  
  for (j in 1:n_samples) {
    gini = x %>%
      sample_n(i) %>%
      select(id) %>%
      left_join(y, by="id") %>%
      group_by(topic) %>% 
      summarise(avg_gamma = mean(gamma)) %>% 
      arrange(desc(avg_gamma)) %>% 
      mutate(rank=row_number()) %>% 
      summarise(gini = ineq(avg_gamma)) %>% 
      pull(gini)

    gini_v[which(is.na(gini_v))[1]] <- gini

  }
}
 gini_df = tibble(gini = gini_v, n = size)
}