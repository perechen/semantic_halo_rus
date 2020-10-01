library(tidyverse)




test_clusters_families = function(n_sample_families = 50, # how many times sample meter families
                                  n_families=2, # how many meters in one family to consider
                                  n_meters=2, # how many meters per family to take
                                  iterations = 50,# how many times calculate clusters 
                                  min_meter_count = 200, # limit for availiable poems per meter
                                  n_poems_per_sample=100, # how many poems in sample
                                  samples=1, # number of samples per meter
                                  random_probabilities=F,# assign topic probablities for each poems randomly 
                                  random_labels=F,
                                  random_clusters=F,
                                  clust_method="complete",
                                  dist="JSD") { 

purity_mean = vector(length=n_sample_families)
ari_mean = vector(length=n_sample_families)

for (m in 1:n_sample_families) {
  
  print(paste("Now at", m, "batch", sep=" "))
  meter_pairs = meter_count  %>% 
    mutate(m_family=str_replace_all(meter, "^(.*?)\\d.*", "\\1"),
           m_family=str_replace(m_family,"Явольн", "Я"))  %>% 
    group_by(m_family)  %>% 
    filter(n>min_meter_count)  %>% 
    mutate(family_size=max(row_number(m_family))) %>% 
    filter(family_size>=n_families)  %>% 
    sample_n(n_meters)  #%>% 
#    filter(!str_detect(meter, "Дк"))
  
  n_clusters = nrow(meter_pairs)/n_meters

  
  
  
  purity_vec = vector(length=iterations)
  ari_vec = vector(length=iterations)
  
  
  
  for (i in 1:iterations) {
    wide_gamma = poem_sampler(x=unique_poems,
                              y=meter_pairs,
                              sample_size=n_poems_per_sample,
                              n_samples=samples,
                              meter_count_limit=min_meter_count) %>% 
      left_join(ru_gamma_separated,by="id")  %>% 
      #    filter(meter!="Ан3")  %>% 
      group_by(topic, meter)  %>% 
      
      summarise(m_gamma = mean(gamma), .groups="keep")  %>% 
      spread(key = topic, value=m_gamma)  
    
    
    ## perpare matrix
    names = wide_gamma  %>% pull(meter)
    wide_matrix = wide_gamma[,-1]  %>% as.matrix()
    rownames(wide_matrix) =  names
    
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
    expected = str_replace_all(names, "^(.*?)\\d.*", "\\1")  %>% str_replace("Явольн", "Я")
    
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
  
  purity_mean[m] = mean(purity_vec)
  ari_mean[m] = mean(ari_vec)
  
 
}
clust_results = tibble(ari = ari_mean, purity = purity_mean)
return(clust_results)
}