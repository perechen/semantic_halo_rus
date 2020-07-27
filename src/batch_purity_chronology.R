


batch_purity_chronology = function(x, # unique poems
                        y, # meter counts
                        z, # gamma table
                        time_steps,
                        sample_size=50,
                        n_samples=3,
                        n_iterations=10,
                        top_meter_limit=7,
                        distance,
                        clust_method="ward.D2") {
  # init variables
  n_classes = top_meter_limit
  
  purity_v = rep(NA, length(time_steps)*n_iterations)
  period_v = rep(time_steps,each=n_iterations)
  
  
    
for (i in time_steps) {
  poems = x %>% filter(period==i)
  meters = y %>% filter(period==i)
  for (j in 1:n_iterations) {
  wide_gamma = poem_sampler(poems,
                            meters,
                            sample_size = sample_size,
                            n_samples= n_samples,
                            meter_count_limit=0)  %>% 
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
                    period = period_v)
  return(final_df)
}

