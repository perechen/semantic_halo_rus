
library(tidyverse)
#calculations
library(philentropy)
#dendrograms
library(ape)
library(phylogram)
library(ggdendro)


consensus_tree_within_meter = function(x, # unique poems
                               y, # meter counts
                               z, # gamma table
                               sample_size, # n poems per sample
                               n_samples=2, # n samples per meter
                               n_trees=50, # number of tress
                               min_meter_count, 
                               max_meter_count=Inf,
                               replace=F,
                               distance="JSD",
                               clust_method="ward.D2") {
  # init variables
  for_consensus =  vector('list', n_trees)  
  all_samples = c()
  
  # take n samples per tree
  for (t in 1:n_trees) {
    sample_for_trees = poem_sampler(x, #poem list 
                                    y, #meter list with counts 
                                    sample_size = sample_size, 
                                    n_samples=n_samples, 
                                    meter_count_limit=min_meter_count,
                                    max_meter_count=max_meter_count,
                                    replace=replace)  %>% 
      mutate(tree = t)
    
    all_samples = all_samples %>% bind_rows(sample_for_trees)
  }
  
  # join sampled poems with gamma table & split by groups
  all_samples_gamma = all_samples  %>% 
    left_join(z, by="id")  %>% 
    select(sample, tree, meter, gamma, topic) %>% 
    group_by(tree)  %>% 
    group_split()
  
  # summarise gamma for each tree data & build h. clusters
  for (i in 1:n_trees) {
    
    wide = all_samples_gamma[[i]]  %>% 
      group_by(sample,topic)  %>% 
      summarise(m_gamma = mean(gamma), .groups="keep")  %>% 
      spread(key=topic, value=m_gamma)
    
    names = wide  %>% select(sample)  %>% pull()
    
    wide_matrix = wide[,-1]  %>% as.matrix()
      
    for_consensus[[i]] = wide_matrix  %>% 
      # scale()  %>% 
      JSD(unit="log2") %>% # calc JSD
      `rownames<-`(names) %>% # reset rownames
      as.dist() %>% # to dist object
      hclust(method=clust_method) %>%
      as.phylo()
    
    
  }
  
  return(for_consensus)  # returns a list of lists with tree info
  
}