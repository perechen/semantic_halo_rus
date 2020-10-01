


test_clusters_halo = function(poem_list,
                              meter_list,
                              p_size,
                              samples_per_meter=3,
                              iterations,
                              min_meter_count,
                              max_meter_count=Inf,
                              rep=FALSE,
                              random_probabilities=FALSE,
                              random_labels=FALSE,
                              random_clusters=FALSE,
                              clust_method="complete",
                              dist="JSD") {

p_df = c()
for (p in p_size) {
print(paste("Now at", p, "poems"))
purity_vec = vector(length=length(iterations))
ari_vec = vector(length=length(iterations))
  
  
for (i in 1:iterations) {
  wide_gamma = poem_sampler(x=poem_list,
                            y=meter_list,
                            sample_size=p,
                            n_samples=samples_per_meter,
                            meter_count_limit=min_meter_count,
                            max_meter_count=max_meter_count,
                            replace=rep) %>% 
    left_join(ru_gamma_separated,by="id")  %>% 
    #    filter(meter!="Ан3")  %>% 
    group_by(topic, sample)  %>% 
    
    summarise(m_gamma = mean(gamma), .groups="keep")  %>% 
    spread(key = topic, value=m_gamma)  
  
  
  ## perpare matrix
  names = wide_gamma  %>% pull(sample)
  wide_matrix = wide_gamma[,-1]  %>% as.matrix()
  rownames(wide_matrix) =  names
  
  n_clusters = meter_list %>% filter(n>min_meter_count, n<=max_meter_count) %>% pull(meter) %>% length()
  
  if (random_probabilities==T) {
    for (i in nrow(wide_matrix)) {
      wide_matrix[i,] = sample(wide_matrix[i,])
    }
  }
  
  if (random_labels==T) {
    rownames(wide_matrx) = sample(names)
  }
  
  
  
  ##calculate distances & build clusterization
  
  
  tree = wide_matrix  %>%
      # scale()  %>% 
      JSD(unit="log2") %>% # calc JSD
      `rownames<-`(names) %>% # reset rownames
      as.dist() %>% # to dist object
      hclust(method=clust_method)
  
  classes = cutree(tree,k=n_clusters)
  expected = str_replace_all(names, "_\\d", "")
  
  if (random_clusters == T) {
    c = sample(as.numeric(classes))
    names(c) = names(classes)
    classes = c
  }
  
  purity = NMF::purity(as.factor(classes), expected)
  ari = mclust::adjustedRandIndex(as.numeric(classes), expected)
  
  purity_vec[i] = purity    
  ari_vec[i] = ari
  
  
}

temp_df = tibble(purity = purity_vec, ari = ari_vec, poems_per_sample = p)
p_df = p_df %>% bind_rows(temp_df)

}
return(p_df)

}
