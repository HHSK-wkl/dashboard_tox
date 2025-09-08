# Script voor het laden van de data ---------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(HHSKwkl)
library(glue)
library(gt)
library(reactable)
library(httr2)

# Load data and create helper functions
parameters <- readRDS("data/parameters.rds")
# try(parameters <- data_online("parameters.rds"))
normen <- readRDS("data/normen.rds")

toelatingen_ctgb_temp <- tempfile(fileext = "xls")
request("https://toelatingen.ctgb.nl/documents/public-authorisations-report.xls") %>% 
  req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36") %>% 
  req_perform(path = toelatingen_ctgb_temp)

toelatingen_diergenees_temp <- tempfile(fileext = "csv")
request("https://www.diergeneesmiddeleninformatiebank.nl/metadatadib.csv") %>% 
  req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36") %>% 
  req_perform(path = toelatingen_diergenees_temp)

toxiciteit <- readxl::read_excel("data/gbm_toxiciteit.xlsx", sheet = "SSDinfo")
f_stof_ctgb <- 
  readxl::read_excel("data/gbm_vertaaltabel_ctgb.xlsx") %>% 
  maak_opzoeker(parnr, werkzame_stof_ctgb) 

f_stof_genees <- 
  readxl::read_excel("data/gbm_vertaaltabel_diergeneesmiddelen_cbg.xlsx") %>% 
  maak_opzoeker(parnr, werkzame_stof_cbg) 

toelatingen_orig <- readxl::read_excel(toelatingen_ctgb_temp, sheet = "Sheet0", guess_max = 5000) %>%
  select(middelnaam, startdatum, expiratiedatum, toepassing, biogewas, aard_werking = `aard werking`, werkzame_stoffen = `werkzamestoffen NL`) %>%
  filter(!is.na(werkzame_stoffen)) %>%
  mutate(status = ifelse(expiratiedatum > Sys.Date(), "Toegelaten", "Verlopen")) %>%
  arrange(desc(expiratiedatum)) %>% 
  mutate(toepassing = fct_relevel(toepassing, "Professioneel", "Professioneel & Niet Professioneel"))

toelatingen_gbm <- 
  toelatingen_orig %>% 
  filter(biogewas == "gewasbescherming") %>%
  select(-biogewas) %>% 
  arrange(toepassing, desc(expiratiedatum))

toelatingen_biociden <- 
  toelatingen_orig %>% 
  filter(biogewas == "biociden") %>%
  select(-biogewas) %>% 
  arrange(toepassing, desc(expiratiedatum)) 

diergenees <- 
  read_delim(toelatingen_diergenees_temp, delim ="|") %>% 
  select(productnaam = PRODUCTNAAM, 
         toelatingsdatum = INSCHRIJVINGSDATUM,
         verloopdatum = `DATUM UITGESCHREVEN/INGETROKKEN`,
         vorm = `FARMACEUTISCHE VORM`,
         afleverstatus = AFLEVERSTATUS,
         werkzame_stoffen = `WERKZAME STOFFEN`,
         doeldieren = DOELDIEREN
         ) %>% 
  mutate(status = ifelse(verloopdatum > Sys.Date(), "Toegelaten", "Verlopen") %>% replace_na("Toegelaten")) %>% 
  arrange(status, desc(verloopdatum), productnaam)
  
  

f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
f_eenheid <- maak_opzoeker(parameters, parnr, eenheid)
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)

f_style_toegelaten <- function(status) {
  if (status == "Toegelaten") {
    color <- HHSKwkl::blauw
  } else if (status == "Verlopen") {
    color <- HHSKwkl::oranje
  } else {
    color <- "#777"
  }
  list(color = color, fontWeight = "bold")
}
