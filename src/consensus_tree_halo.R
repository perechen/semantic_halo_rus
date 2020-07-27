
library(tidyverse)
#calculations
library(proxy)
#dendrograms
library(ape)
library(phylogram)
library(ggdendro)
#visuals

consensus_tree_halo = function(x, # unique poems
                            y, # meter counts
                            z, # gamma table
                            sample_size, 
                            n_samples=2, 
                            min_meter_count,
                            max_meter_count=Inf,
                            distance,
                            clust_method="ward.D2",
                            rep=T) {
  # init variables
  for_consensus =  vector('list', n_samples)  
  
  samples_for_trees = poem_sampler(x, 
                   y, 
                   sample_size = sample_size, 
                   n_samples= n_samples, 
                   meter_count_limit=min_meter_count,
                   max_meter_count=max_meter_count,
                   replace=rep)  %>% 
    left_join(z, by="id")  %>% 
    mutate(sample = str_replace(sample,"^.*_(\\d*)$", "\\1")) %>% 
    select(sample, meter, gamma, topic) %>% 
    group_by(sample)  %>% 
    group_split()
  
  for (i in 1:length(samples_for_trees))
  {
    
    wide = samples_for_trees[[i]]  %>% 
      group_by(topic, meter)  %>% 
      summarise(m_gamma = mean(gamma))  %>% 
      spread(key=topic, value=m_gamma)
    
    names = wide  %>% select(meter)  %>% pull()
    
    wide_matrix = wide[,-1]  %>% as.matrix()
    rownames(wide_matrix) = names
    
    for_consensus[[i]] = dist(wide_matrix, method=distance) %>%
      hclust(method=clust_method) %>%
      as.phylo()
    
    
  }

return(for_consensus)  
  
}