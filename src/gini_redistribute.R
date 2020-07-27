library(tidyverse)



gini_redistribute = function(x, 
                       y,
                       vector_of_sizes,
                       n_samples) {
  

  gini_v = rep(NA, length(vector_of_sizes)*n_samples)  
  size = rep(vector_of_sizes, n_samples)
  
  for (i in 1:n_samples) {
    message(paste("At", i, "redistribution. Going strong...", sep=" "))
    
    sample_pool = x
    
    for (j in vector_of_sizes) {
      gini_ids = sample_pool %>%
        sample_n(j) %>%
        select(id)
      gini = gini_ids %>% 
        left_join(y, by="id") %>%
        group_by(topic) %>% 
        summarise(avg_gamma = mean(gamma)) %>% 
        arrange(desc(avg_gamma)) %>% 
        mutate(rank=row_number()) %>% 
        summarise(gini = ineq(avg_gamma)) %>% 
        pull(gini)
      
      gini_v[which(is.na(gini_v))[1]] <- gini
      
      #remove sampled poems from availiable pool
      sample_pool = sample_pool %>% anti_join(gini_ids, by="id")
      
    }
  }
  gini_df = tibble(gini = gini_v, n = size)
}