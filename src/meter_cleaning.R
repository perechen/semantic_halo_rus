
clean_meter_from_docnames = function(x) {


docs = x %>%
  separate(text, c("id", "author", "first_line", "meter"), sep="_")

temp = docs %>%
  mutate(
    # MONOMETERS
    m_form = str_replace_all(meter, c("(^[:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*$" = "\\1", 
                                      "^([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:],[:lower:]$" = "\\1",
                                      "([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*?\\+([:upper:][:lower:]?[:lower:]?[:digit:])[:lower:]*$" = "\\1\\2",
                                      "^([:upper:][:lower:]{0,2}[:digit:])[:lower:],?[:lower:]?; [:upper:][:lower:]{0,2}[:digit:],?[:digit:]?[:lower:],?[:lower:]?$" = "\\1")),
    clausula = str_replace_all(meter, c("^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*)$" = "\\1",
                                        "^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:],[:lower:])$" = "\\1",
                                        "^[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*?)\\+[:upper:][:lower:]?[:lower:]?[:digit:]([:lower:]*$)" = "\\1 \\2",
                                        "^[:upper:][:lower:]{0,2}[:digit:]([:lower:],?[:lower:]?); [:upper:][:lower:]{0,2}[:digit:],?[:digit:]?[:lower:],?[:lower:]?$" = "\\1"))
  )

for (i in 1:nrow(temp)) {
  if (is.na(temp$meter[i]) == FALSE) { 
    if (str_detect(temp$meter[i], "^Я[:digit:][:lower:]?,\\s?Я[:digit:][:lower:]?,\\s?Я[:digit:][:lower:]?") == TRUE) {
      
      temp$m_form[i] = "Явольн"
      temp$clausula[i] = "вольн"
    }
    else if (str_detect(temp$meter[i], "^Я([:digit:],){2,5}[:digit:]([:lower:],){1,2}[:lower:]$") == TRUE)
    {
      temp$m_form[i] = "Явольн"
      temp$clausula[i] = "вольн"
    }
  }
}

for (i in 1:nrow(temp)) {
  if (is.na(temp$meter[i]) == FALSE) {
    if (str_detect(temp$m_form[i], ",|;|~|#|\\+") == TRUE) {
      temp$m_form[i] = NA
      temp$clausula[i] = NA
    }
  }
  
}

temp1 = temp %>% 
  select(-meter) %>% 
  unite("doc", id:clausula, sep="_")

return(temp1)

}

