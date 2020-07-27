library(tidyverse)


## takes list of unique poems and info on meter counts and returns sampled poems by ids
poem_sampler = function(x, y, 
                        sample_size, 
                        n_samples, 
                        meter_count_limit,
                        max_meter_count=Inf,
                        replace=T) {
  
  # first determine which meters to consider for sampling (based on sample_size)
  legit_meters = y %>% 
    filter(n > meter_count_limit, n <= max_meter_count)  %>% 
    select(-n)
  
  sample_pool = x  %>% 
    right_join(legit_meters, by="meter")
  
  samples_df = c()
  #
  for (i in 1:n_samples) {
  sample_i = sample_pool %>% 
    group_by(meter)  %>% 
    sample_n(sample_size)  %>% 
    ungroup()  %>%
    mutate(sample = paste(meter, i, sep="_")) %>% 
    select(id, sample)
  
  if (replace==F) {
    #update remaining sample_pool
    sample_pool = sample_pool %>%
      anti_join(sample_i, by="id")
  }
  
  samples_df = samples_df %>% 
    rbind(sample_i)
  
  }  
  
  return(samples_df)
}