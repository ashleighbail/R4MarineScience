# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

# -----------------------------
# Phase 1: Ingestion and Decontamination
# -----------------------------

# Find the sheet names in the catch log
catch_sheets <- excel_sheets(here::here("data/workshop2/estuary_catch_log.xlsx"))
catch_sheets

# Read all Excel sheets and combine them into one dataframe
catch_log <- catch_sheets |>
  set_names() |>
  map_dfr(
    ~ read_excel(
      here::here("data/workshop2/estuary_catch_log.xlsx"),
      sheet = .x
    ),
    .id = "site"
  )
catch_log

glimpse(catch_log)

# Clean site and species names
catch_log <- catch_log |>
  mutate(
    site = site |>
      str_to_lower() |>
      str_trim() |>
      str_replace_all(" ", "_"),
    
    species = species |>
      str_to_lower() |>
      str_trim() |>
      str_replace_all(" ", "_")
  )
unique(catch_log$site)
unique(catch_log$species)

# -----------------------------
# Import site metadata
# -----------------------------

metadata <- read_csv(here::here("data/workshop2/estuary_metadata.csv"))
glimpse(metadata)
metadata

# Clean site names in metadata
metadata <- metadata |>
  mutate(
    site_name = site_name |>
      str_to_lower() |>
      str_trim() |>
      str_replace_all(" ", "_")
  )
metadata

# Checking that every catch site has a corresponding metadata site
catch_log |>
  anti_join(metadata, by = c("site" = "site_name"))

# -----------------------------
# Import sonde data
# -----------------------------

sonde <- read_csv(here::here("data/workshop2/estuary_sonde_data.csv"))
glimpse(sonde)

# Convert timestamp from character to date-time
sonde <- sonde |>
  mutate(
    timestamp = dmy_hm(timestamp)
  )
glimpse(sonde)

sonde |>
  filter(turbidity == -999)

sonde |>
  summarise(bad_turbidity = sum(turbidity == -999, na.rm = TRUE))

sonde <- sonde |>
  mutate(
    turbidity = na_if(turbidity, -999)
  )

mean(sonde$turbidity, na.rm = TRUE)
summary(sonde$turbidity)
unique(sonde$site)

# -----------------------------
# Import species dictionary
# -----------------------------

species_dictionary <- read_csv(
  here::here("data/workshop2/species_dictionary.csv")
)
glimpse(species_dictionary)
species_dictionary

catch_log |>
  anti_join(
    species_dictionary,
    by = c("species" = "common_name")
  )

# -----------------------------
# Phase 2: The Relational Architecture
# -----------------------------

sonde <- sonde |>
  mutate(
    date = floor_date(timestamp, unit = "day")
  )
glimpse(sonde)

daily_water <- sonde |>
  group_by(site, date) |>
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(daily_water)
daily_water

catch_translated <- catch_log |>
  left_join(
    species_dictionary,
    by = c("species" = "common_name")
  )
glimpse(catch_translated)
catch_translated
catch_translated <- catch_translated |>
  select(-species)

master_data <- catch_translated |>
  left_join(
    metadata,
    by = c("site" = "site_name")
  ) |>
  left_join(
    daily_water,
    by = c("site", "date")
  )
glimpse(master_data)

master_data |>
  summarise(
    missing_taxonomy = sum(is.na(scientific_name)),
    missing_zone = sum(is.na(zone)),
    missing_temperature = sum(is.na(mean_temperature)),
    missing_salinity = sum(is.na(mean_salinity))
  )

# -----------------------------
# Phase 3: Zero-Catch Framework
# -----------------------------

complete_catch <- master_data |>
  complete(
    site,
    date,
    scientific_name
  )

complete_catch <- complete_catch |>
  mutate(
    count = coalesce(count, 0)
  )
glimpse(complete_catch)

# Create every site × date × species combination
complete_catch <- master_data |>
  select(site, date, scientific_name, count) |>
  complete(
    site,
    date,
    scientific_name
  ) |>
  mutate(
    count = coalesce(count, 0)
  )

complete_catch <- complete_catch |>
  left_join(
    metadata,
    by = c("site" = "site_name")
  )

complete_catch <- complete_catch |>
  left_join(
    daily_water,
    by = c("site", "date")
  )

glimpse(complete_catch)

# -----------------------------
# Phase 4: Statistical Extraction
# -----------------------------

catch_summary <- complete_catch |>
  group_by(scientific_name, zone) |>
  summarise(
    mean_catch = mean(count, na.rm = TRUE),
    se_catch = sd(count, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

salinity_summary <- complete_catch |>
  distinct(site, date, zone, mean_salinity) |>
  group_by(zone) |>
  summarise(
    salinity_mean = mean(mean_salinity, na.rm = TRUE),
    salinity_sd = sd(mean_salinity, na.rm = TRUE),
    salinity_n = sum(!is.na(mean_salinity)),
    salinity_se = salinity_sd / sqrt(salinity_n),
    .groups = "drop"
  )
salinity_summary

summary_table <- catch_summary |>
  left_join(
    salinity_summary,
    by = "zone"
  )

summary_table_clean <- summary_table |>
  select(
    scientific_name,
    zone,
    mean_catch,
    se_catch,
    mean_salinity = salinity_mean,
    se_salinity = salinity_se
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

summary_table_clean

master_dataset <- complete_catch |>
  mutate(
    date = as.Date(date)
  )
write_csv(
  master_dataset,
  "outputs/tables/estuary_master_dataset.csv"
)
