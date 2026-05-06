# --- 0. Setup ---------------------------------------------------------------
library(here)
library(readxl)
library(readr)
library(tidyverse)

# Reference year for all indexing
REF_YEAR <- 2013

# --- 1. Load data ------------------------------------------------------------
chemin <- here()

# Helper: coerce a column to numeric safely
to_num <- function(x) suppressWarnings(as.numeric(x))


# 2013–2016
rei_a <- map_dfr(2013:2016, function(an) {
  code_marseille <- if (an == 2014) "55" else "055"
  
  df <- read_excel(
    file.path(chemin, "input", paste0("REI_", an, ".xlsx")),
    sheet = paste0("REI_", an)
  ) %>% 
    filter(DEP == "13", COM == code_marseille)
  
  taux_col       <- if ("E12VOTE" %in% names(df)) "E12VOTE" else "E12"
  taux_metro_col <- if ("E32VOTE" %in% names(df)) "E32VOTE" else "E32"
  
  df %>% 
    transmute(
      annee         = an,
      base_nette    = to_num(E11),
      taux_vote     = to_num(.data[[taux_col]]),
      montant_reel  = to_num(E13),
      nb_articles   = to_num(E14),
      base_metro    = to_num(E31),
      taux_metro    = to_num(.data[[taux_metro_col]]),
      base_exo_neuf = to_num(G61)
    )
})
write_csv(rei_a, here("cache", "rei_a.csv"))


# 2017–2018
rei_b <- map_dfr(2017:2018, function(an) {
  df <- read_excel(
    file.path(chemin, "input", paste0("REI_", an, ".xlsx"))
  ) %>% 
    filter(DEP == "13", COM == "055")
  
  taux_col       <- if ("E12VOTE" %in% names(df)) "E12VOTE" else "E12"
  taux_metro_col <- if ("E32VOTE" %in% names(df)) "E32VOTE" else "E32"
  
  df %>% 
    transmute(
      annee         = an,
      base_nette    = to_num(E11),
      taux_vote     = to_num(.data[[taux_col]]),
      montant_reel  = to_num(E13),
      nb_articles   = to_num(E14),
      base_metro    = to_num(E31),
      taux_metro    = to_num(.data[[taux_metro_col]]),
      base_exo_neuf = to_num(G61)
    )
})
write_csv(rei_b, here("cache", "rei_b.csv"))


# 2019–2022
rei_c <- map_dfr(2019:2022, function(an) {
  read_excel(
    file.path(chemin, "input", paste0("REI_", an, ".xlsx"))
  ) %>% 
    filter(DEPARTEMENT == "13", COMMUNE == "055") %>% 
    transmute(
      annee         = an,
      base_nette    = to_num(`FB - COMMUNE / BASE NETTE`),
      taux_vote     = to_num(`FB - COMMUNE / TAUX VOTE`),
      montant_reel  = to_num(`FB - COMMUNE / MONTANT REEL`),
      nb_articles   = to_num(`FB - COMMUNE / NOMBRE D'ARTICLES`),
      base_metro    = to_num(`FB - GFP / BASE NETTE`),
      taux_metro    = to_num(`FB - GFP / TAUX VOTE`),
      base_exo_neuf = to_num(`FB - BASE DES LOCAUX D'HABITATION EXONEREE POUR 2 ANS / COMMUNE`)
    )
})
write_csv(rei_c, here("cache", "rei_c.csv"))


# 2023–2024
rei_d <- map_dfr(2023:2024, function(an) {
  read_delim(
    file.path(chemin, "input", paste0("REI_", an, ".csv")),
    delim = ";", locale = locale(encoding = "latin1"),
    show_col_types = FALSE
  ) %>% 
    filter(DEP == "13", COM == "055") %>% 
    transmute(
      annee         = an,
      base_nette    = to_num(E11),
      taux_vote     = to_num(E12VOTE),
      montant_reel  = to_num(E13),
      nb_articles   = to_num(E14),
      base_metro    = to_num(E31),
      taux_metro    = to_num(E32VOTE),
      base_exo_neuf = to_num(G61)
    )
})
write_csv(rei_d, here("cache", "rei_d.csv"))


# --- 2. Assemble & compute derived variables ----------------------------------
rei <- bind_rows(rei_a, rei_b, rei_c, rei_d) %>% 
  arrange(annee) %>% 
  mutate(
    # Core derived variables
    base_par_article     = base_nette / nb_articles,
    taux_total           = taux_vote + taux_metro,
    recettes_implicites  = base_nette * taux_total / 100,
    croissance_base      = (base_nette / lag(base_nette) - 1) * 100,
    
    # Decomposition: rate effect vs base effect (reference = REF_YEAR)
    taux_ref             = taux_vote[annee == REF_YEAR],
    recettes_si_taux_cst = base_nette * taux_ref / 100,
    effet_base           = recettes_si_taux_cst - recettes_si_taux_cst[annee == REF_YEAR],
    effet_taux           = montant_reel - recettes_si_taux_cst,
    
    # Indices (base 100 = REF_YEAR)
    index_base      = base_nette    / base_nette[annee == REF_YEAR]    * 100,
    index_recettes  = montant_reel  / montant_reel[annee == REF_YEAR]  * 100,
    index_taux      = taux_vote     / taux_vote[annee == REF_YEAR]     * 100,
    
    # Exemptions as share of gross base
    part_exo_neuf   = base_exo_neuf / (base_nette + base_exo_neuf) * 100
  )

# --- 3. Export data -----------------------------------------------------------
write_csv(rei, here("cache", "rei.csv"))