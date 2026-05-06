# --- 0. Setup ---------------------------------------------------------------
library(here)
library(readxl)
library(readr)
library(tidyverse)

MARSEILLE_ARR <- sprintf("132%02d", 1:16)   # 13201 … 13216

# --- 1. Load data -------------------------------------------------------------
# Marseille rates
lovac_marseille <- read.csv(here("cache", "marseille_agg.csv")) %>% 
  mutate(
    CODGEO_25 = as.character(CODGEO_25),
    annee = year
  ) %>%
  mutate(
    tx_lv = ifelse(nb_lp == 0, NA, round(nb_lv/ nb_lp, 2)),
    tx_lv_sup2 = ifelse(nb_lp == 0, NA,round(nb_lv_sup2 / nb_lp, 2)),
    tx_lv_inf2 = ifelse(nb_lp == 0, NA,round(nb_lv_inf2 / nb_lp, 2)),
    share_structural = round(nb_lv_sup2 / nb_lv,2)
  )

# Marseille taxes
rei <- read.csv(here("cache", "rei.csv"))


# --- 2. Join data -------------------------------------------------------------
rei_lovac <- rei %>% 
  filter(annee >= 2020) %>% 
  left_join(lovac_marseille, by = "annee") %>% 
  mutate(
    # Revenue foregone: structural vacancies × avg cadastral value × total rate
    base_vacants_estim    = nb_lv_sup2 * base_par_article,
    recettes_potentielles = base_vacants_estim * taux_total / 100,
    
    # Mobilization scenarios (what if X% of structural vacancies re-enter base?)
    gain_10pct = nb_lv_sup2 * 0.10 * base_par_article * taux_total / 100,
    gain_25pct = nb_lv_sup2 * 0.25 * base_par_article * taux_total / 100,
    gain_50pct = nb_lv_sup2 * 0.50 * base_par_article * taux_total / 100,
    
    # Rate increase alternative: how much would a +1pp rate hike yield?
    gain_1pp_rate = base_nette * 0.01 / 100
  )
cat("Joined panel: ", nrow(rei_lovac), "years (",
    min(rei_lovac$annee), "–", max(rei_lovac$annee), ")\n\n")


# --- 3. Export data -----------------------------------------------------------
write_csv(rei_lovac, here("cache", "rei_lovac.csv"))