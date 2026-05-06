# Data sources
#  Aggregated open data (commune level):
#   https://www.data.gouv.fr/fr/datasets/logements-vacants-du-parc-prive-par-commune-departement-region-france
#  - LOVAC_exh: all vacant private dwellings, 2020-2025
#  - LOVAC_fil: structurally vacant (2+ years), 2020-2025

# --- 0. Setup ----------------------------------------------------------------
library(here)
library(tidyverse)
library(readxl)
library(readr)

# --- 1. Load data ------------------------------------------------------------
lovac_raw<- read_excel(here("input","lovac-open-data-2020-a-2025-vd.xlsx"), 
                       sheet = "COM")
head(lovac_raw)

# --- 2. Data manage -----------------------------------------------------------
# Wide to long format
lovac_long <- lovac_raw %>%
  pivot_longer(
    cols = starts_with("pp_"),
    names_to = c(".value", "year"),
    names_pattern = "(pp_[^_]+(?:_plus_2ans)?)_(\\d+)$"
  ) %>% 
  mutate(year = as.integer(paste0("20", year)))

# Eliminate secret values
lovac_long[lovac_long == "s"] <- NA

# Variable type
str(lovac_long)

lovac_long <- lovac_long %>% 
  mutate(
    nb_lv = as.integer(pp_vacant),
    nb_lv_sup2 = as.integer(pp_vacant_plus_2ans),
    nb_lp = as.integer(pp_total),
    nb_lv_inf2 = nb_lv - nb_lv_sup2,
    year = as.character(year)
  )

# --- 3. Aggregate big cities --------------------------------------------------

# Arrondissement codes for Marseille, Paris, Lyon
MARSEILLE_ARR <- sprintf("132%02d", 1:16)   # 13201 … 13216
PARIS_ARR     <- sprintf("751%02d", 1:20)   # 75101 … 75120
LYON_ARR      <- sprintf("693%02d", 81:89)  # 69381 … 69389

# Arrondissement prefix: aggregated city label
CITY_AGGREGATION <- tribble(
  ~codgeo_prefix, ~city_code, ~city_name,
  "132",          "MARS",     "Marseille",
  "751",          "PARI",     "Paris",
  "693",          "LYON",     "Lyon"
)

# Peer cities that appear as single communes (no arrondissement split)
PEERS_SINGLE <- tribble(
  ~CODGEO_25,  ~city_name,
  "06088",  "Nice",
  "33063",  "Bordeaux",
  "34172",  "Montpellier",
  "59350",  "Lille"
)

lovac_long <- lovac_long %>%
  mutate(
    codgeo_prefix3 = substr(CODGEO_25, 1, 3),
    city_type = case_when(
      CODGEO_25 %in% MARSEILLE_ARR ~ "marseille_arr",
      CODGEO_25 %in% PARIS_ARR     ~ "paris_arr",
      CODGEO_25 %in% LYON_ARR      ~ "lyon_arr",
      TRUE                       ~ "single_commune"
    )
  )

# Aggregate arrondissement cities by summing stock variables
aggregate_city <- function(data, arr_codes, city_code, city_name) {
  data %>%
    filter(CODGEO_25 %in% arr_codes) %>%
    group_by(year) %>%
    summarise(
      nb_lv = sum(nb_lv, na.rm = F),
      nb_lv_sup2 = sum(nb_lv_sup2, na.rm = F),
      nb_lv_inf2 = sum(nb_lv_inf2, na.rm = F),
      nb_lp = sum(nb_lp, na.rm = F),
      n_arr_used = n(),
      .groups = "drop"
    ) %>%
    mutate(CODGEO_25 = city_code, LIBGEO_25 = city_name)
}

marseille_agg <- aggregate_city(lovac_long, MARSEILLE_ARR, "13055", "Marseille")
paris_agg     <- aggregate_city(lovac_long, PARIS_ARR,     "75056", "Paris")
lyon_agg      <- aggregate_city(lovac_long, LYON_ARR,      "69123", "Lyon")

# --- 4. Build unified peer panel (aggregated cities + single communes) --------
peers_single_data <- lovac_long %>%
  filter(CODGEO_25 %in% PEERS_SINGLE$CODGEO_25) %>%
  left_join(PEERS_SINGLE, by = "CODGEO_25") %>%
  rename(libgeo = city_name) %>%
  mutate(n_arr_used = 1L)
  

lovac_peers <- bind_rows(
  marseille_agg,
  paris_agg,
  lyon_agg,
  peers_single_data %>% select(CODGEO_25, LIBGEO_25, year,
                               nb_lp, nb_lv, nb_lv_sup2, nb_lv_inf2, n_arr_used)
 ) %>%
  mutate(
    tx_lv = ifelse(nb_lp == 0, NA, round(nb_lv/ nb_lp, 2)),
    tx_lv_sup2 = ifelse(nb_lp == 0, NA,round(nb_lv_sup2 / nb_lp, 2)),
    tx_lv_inf2 = ifelse(nb_lp == 0, NA,round(nb_lv_inf2 / nb_lp, 2)),
    share_structural = round(nb_lv_sup2 / nb_lv,2)
  )

cat("Peer panel built:", n_distinct(lovac_peers$CODGEO_25), "cities ×",
    n_distinct(lovac_peers$year), "years\n")

# --- 5. Export data -----------------------------------------------------------
write_csv(lovac_peers,here("cache", "lovac_peers.csv"))
write_csv(lovac_long, here("cache", "lovac_long.csv"))
write_csv(marseille_agg, here("cache", "marseille_agg.csv"))