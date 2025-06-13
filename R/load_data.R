# Script voor het laden van de data ---------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(HHSKwkl)
library(glue)
library(gt)

# Load data and create helper functions
parameters <- readRDS("data/parameters.rds")
# try(parameters <- data_online("parameters.rds"))
normen <- readRDS("data/normen.rds")

toxiciteit <- readxl::read_excel("data/gbm_toxiciteit.xlsx", sheet = "SSDinfo")
f_stof_ctgb <- 
  readxl::read_excel("data/gbm_vertaaltabel_ctgb.xlsx") %>% 
  maak_opzoeker(parnr, werkzame_stof_ctgb) 

toelatingen_gbm <- readxl::read_excel("data/public-authorisations-report.xls", sheet = "Sheet0", guess_max = 5000) %>%
  select(middelnaam, startdatum, expiratiedatum, toepassing, biogewas, aard_werking = `aard werking`, werkzame_stoffen = `werkzamestoffen NL`) %>%
  filter(!is.na(werkzame_stoffen),
         toepassing != "Niet Professioneel",
         biogewas == "gewasbescherming") %>%
  mutate(status = ifelse(expiratiedatum > Sys.Date(), "Toegelaten", "Verlopen")) %>%
  arrange(desc(expiratiedatum))

toelatingen_overig <- readxl::read_excel("data/public-authorisations-report.xls", sheet = "Sheet0", guess_max = 5000) %>%
  select(middelnaam, startdatum, expiratiedatum, toepassing, biogewas, aard_werking = `aard werking`, werkzame_stoffen = `werkzamestoffen NL`) %>%
  filter(!is.na(werkzame_stoffen),
         biogewas != "gewasbescherming" | toepassing == "Niet Professioneel") %>%
  mutate(status = ifelse(expiratiedatum > Sys.Date(), "Toegelaten", "Verlopen")) %>%
  arrange(desc(expiratiedatum))

f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
f_eenheid <- maak_opzoeker(parameters, parnr, eenheid)
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)

