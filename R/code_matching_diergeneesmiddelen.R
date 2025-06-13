library(tidyverse)
library(HHSKwkl)

parameters <- data_online("parameters.rds") %>% filter(parnr > 999, parnr < 2000) %>%  
  transmute(parnr = parnr, par = par, parnaamlang = str_to_lower(parnaamlang))

genees <- read_delim("data/metadatadib.csv", delim ="|") %>% 
  select(`WERKZAME STOFFEN`) %>% 
  separate_longer_delim(`WERKZAME STOFFEN`, delim = "#") %>% 
  distinct() %>% 
  rename(genees_orig = 1) %>% 
  mutate(genees_lower = str_to_lower(genees_orig))

parameters %>% 
  fuzzyjoin::fuzzy_left_join(y = genees, by = c("parnaamlang" = "genees_lower"), 
                             match_fun = function(x, y) stringdist::stringdist(x, y) < 2) %>% 
  filter(!is.na(genees_orig)) %>% 
  rename(werkzame_Stof_cbg = genees_orig ) %>% 
  select(-genees_lower) %>% 
  openxlsx::write.xlsx("data/gbm_vertaaltabel_diergeneesmiddelen_cbg.xlsx")



