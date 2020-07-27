
clean_meter_from_docnames = function(x) {


docs = x %>%
  separate(index, c("id", "author", "year", "meter"), sep="_") %>%
  mutate(meter = str_remove(meter, "\""))

temp = docs %>%
  mutate(
    # MONOMETERS
    m_form = str_replace_all(meter, c("(^[:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*$" = "\\1", 
                                      
                                      "^([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:],[:lower:]$" = "\\1",
                                      
                                      "([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*?\\+([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*$" = "\\1\\2",
                                      
                                      "^([:upper:][:lower:]{0,2}[:digit:])[:lower:],?[:lower:]?; [:upper:][:lower:]{0,2}[:digit:],?[:digit:]?[:lower:],?[:lower:]?$" = "\\1",
                                      
                            "(^[:upper:][:lower:]{0,2}[:digit:])[:lower:]{0,2}~([:upper:][:lower:]{0,2}[:digit:])[:lower:]{0,2}$" = "\\1\\2")),
      
    clausula = str_replace_all(meter, c("^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*)$" = "\\1",
                                        
                                        "^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:],[:lower:])$" = "\\1",
                                        
                                        "^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*?)\\+[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*$)" = "\\1 \\2",
                                        
                                        "^[:upper:][:lower:]{0,2}[:digit:]([:lower:],?[:lower:]?); [:upper:][:lower:]{0,2}[:digit:],?[:digit:]?[:lower:],?[:lower:]?$" = "\\1",
                                        
                                         "^[:upper:][:lower:]{0,2}[:digit:]([:lower:]{0,2})~[:upper:][:lower:]{0,2}[:digit:]([:lower:]{0,2})$" = "\\1\\2")))  %>% 


### noise instead of music
mutate(m_form = case_when(!str_detect(meter, "^[:upper:]") ~ "NA",
                          
                          ### Free iambs
                          
                     str_detect(meter, "^Я[:digit:][:lower:]?,\\s?Я[:digit:][:lower:]?,\\s?Я[:digit:][:lower:]?") ~ "Явольн",
                          
                          ### Free iambs
                          
                     str_detect(meter, "^Я([:digit:],){2,5}[:digit:]([:lower:],){1,2}[:lower:]$") ~ "Явольн",
                     TRUE ~ m_form),
    
       ### remove whitespaces just in case
       
       m_form = str_remove(m_form, "\\s"))   %>% 
    
      ### remove suspicious heterogenous formulas
    
mutate(m_form = case_when(str_detect(m_form, ",|;|~|#|\\+") ~ "NA",
                          TRUE ~ m_form),
       clausula = case_when(str_detect(m_form, "NA") ~ "NA",
                            str_detect(m_form, "вольн") ~ "вольн",
                            TRUE ~ clausula))




 ### loop to remove doubles from ~ formulas

    for (i in 1:nrow(temp)) {
    if (nchar(temp$m_form[i]) == 4) {
        split = strsplit(temp$m_form[i], "(?<=.{2})", perl=T)[[1]]
        if (split[1] == split[2]) {
            temp$m_form[i] == split[i]

        }
    }
    if (nchar(temp$clausula[i]) == 2) {
        split = strsplit(temp$clausula[i], "(?<=.{1})", perl=T)[[1]]
        if (split[1] == split[2]) {
            temp$clausula[i] == split[i]
            }
        }
    
}

temp1 = temp %>% 
  select(-meter) %>% 
  unite("doc", id:clausula, sep="_")

return(temp1)

}

