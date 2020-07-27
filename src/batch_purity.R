


batch_purity = function(x, # unique poems
                        y, # meter counts
                        z, # gamma table
                        sample_size_steps,
                        n_samples=3,
                        n_iterations=10,
                        meter_limit,
                        distance,
                        clust_method="ward.D2") {
  # init variables
  n_classes = y %>%
    filter(n>=meter_limit)  %>%
    pull(meter)  %>%
    length()
  
  purity_v = rep(NA, length(sample_size_steps)*n_iterations)
  sample_depth = rep(sample_size_steps,each=n_iterations)
  
  
    
for (i in sample_size_steps) {
  for (j in 1:n_iterations) {
  wide_gamma = poem_sampler(x, 
                                   y, 
                                   sample_size = i, 
                                   n_samples= n_samples, 
                                   meter_count_limit=meter_limit)  %>% 
    left_join(z, by="id")  %>% 
#    mutate(sample = str_replace(sample,"^.*?_", "")) %>% 
    group_by(topic, sample) %>% 
    summarise(m_gamma = mean(gamma)) %>% 
    spread(key=topic, value=m_gamma)
    
  ## perpare matrix
  names = wide_gamma  %>% pull(sample)
  wide_matrix = wide_gamma[,-1]  %>% as.matrix()
  rownames(wide_matrix) =  names
  
  
  ##calculate distances & build clusterization
  
  tree = wide_matrix  %>% 
    dist(method=distance)  %>% 
    hclust(method=clust_method) 
  
  classes = cutree(tree,k=n_classes)
  expected = str_replace_all(names, "_\\d$", "")
  
  purity = NMF::purity(as.factor(classes), expected)
  
  purity_v[which(is.na(purity_v))[1]] <- purity
  }

}
  final_df = tibble(purity = purity_v,
                    sample_size = sample_depth)
  return(final_df)
}

