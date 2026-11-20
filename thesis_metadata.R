# ---
# title: "Thesis Metadata"
# author: "M. Gebhard"
# date: "`r Sys.Date()`"
# output: html_document
# ---
# 
# # Load Libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

# read in data ------------------------------------------------------------
#read in Lindsay data
tcrmp_master <- read.csv("imports/TCRMP_coralhealth_condition_master_May2002_Dec2025.csv")
#read in my data
Demo <- read_xlsx("imports/site_map.xlsx", sheet = "Demo")
Metadata <- read_xlsx("imports/site_map.xlsx", sheet = "Metadata")

DHW_data <- read_xlsx("imports/TCRMP_temp_Daily_Temp_DHW.xlsx")

# lindsay help ------------------------------------------------------------
#lindsay example
# GKT_count <- tcrmp_master %>% 
#   filter(surveyyear == 2023 & period == "Annual" & location == "Grammanik Tiger FSA") %>% 
#   group_by(transect) %>% 
#   summarise(n = n_distinct(colony_id)) %>% 
#   mutate(transect = as.numeric(transect))
# demo_count <- Demo %>% 
#   filter(Code == "GKT") %>% 
#   left_join(GKT_count %>% filter(transect == c(1, 2, 3)), by = c("Demo" = "transect"))

# creating dataframe ------------------------------------------------------
#unique ID
  Demo <- Demo %>% 
    mutate(ID = paste0(
      Code, "_",
      Demo))
sites <- c("Grammanik Tiger FSA",
           "College Shoal East",
           "Meri Shoal",
           "Black Point",
           "Seahorse Cottage Shoal",
           "South Capella",
           "South Water",
           "Lang Bank Red Hind FSA",
           "Hind Bank East FSA",
           "Botany Bay",
           "Buck Island STX Deep",
           "Brewers Bay")
#matched site names to locations and codes
  tcrmp_sites <- tcrmp_master %>% 
    select(surveyyear, period, location, transect, colony_id, species) %>% 
    filter(location %in% c("Grammanik Tiger FSA",
          "College Shoal East",
          "Meri Shoal",
          "Black Point",
          "Seahorse Cottage Shoal",
           "South Capella",
           "South Water",
           "Lang Bank Red Hind FSA",
           "Hind Bank East FSA",
           "Botany Bay",
           "Buck Island STX Deep",
           "Brewers Bay")) %>% 
    left_join(Metadata %>% 
                select("Location", "Code"), by = c("location" = "Location")) %>% 
    mutate(ID = paste0(
      Code, "_",
      transect))
  #make distinction for mesophotic (>30m)/shallow 
  
  Metadata_depth <- Metadata %>%
    mutate(ReefType = ifelse(Depth >= 30, "Mesophotic", "Shallow"))

  # Create colony count for 2023 and 2024
  
  demo_colony_count <- tcrmp_sites %>% 
    filter(surveyyear %in% c(2023, 2024)) %>%
    group_by(ID, surveyyear) %>% 
    summarise(
      colony_count = n_distinct(colony_id),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = surveyyear,
      values_from = colony_count,
      names_prefix = "Colony_Count_"
    )
  #average coral colony count across 2 years
  
  coral_average <- demo_colony_count %>%
    group_by(ID_group = substr(ID, 1, 3)) %>%
    group_by(ID_group) %>%
    summarise(
      Average_count = mean(
        c(Colony_Count_2023, Colony_Count_2024),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  Metadata_depth <- Metadata_depth %>%
    left_join(coral_average, by = c("Code" = "ID_group"))
  

    
# data cleanup ------------------------------------------------------------



    
# Sum coral colonies by Code for 2023 and 2024
  
  colony_totals <- tcrmp_sites %>%
    filter(surveyyear %in% c(2023, 2024)) %>%
    mutate(Code = substr(ID, 1, 3)) %>%
    group_by(Code, surveyyear, ID) %>%
    summarise(
      colony_count = n_distinct(colony_id),
      .groups = "drop"
    ) %>%
    group_by(Code, surveyyear) %>%
    summarise(
      Coral_colonies = sum(colony_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = surveyyear,
      values_from = Coral_colonies,
      names_prefix = "Coral_colonies_"
    )
  
  # Join colony totals to Metadata_depth
  
  Metadata_depth <- Metadata_depth %>%
    left_join(colony_totals, by = "Code")
    
 #  Sum coral richness by Code for 2023 and 2024
  richness_totals <- tcrmp_sites %>%
    filter(surveyyear %in% c(2023, 2024)) %>%
    mutate(Code = substr(ID, 1, 3)) %>%
    group_by(Code, surveyyear) %>%
    summarise(
      Coral_richness = n_distinct(species),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = surveyyear,
      values_from = Coral_richness,
      names_prefix = "Coral_richness_"
    )
  #average coral richness across 2 years
  
  # richness_average <- demo_richness %>%
  #   group_by(Location) %>%
  #   summarise(
  #     Average_colonies = mean(
  #       c(Richness_2023, Richness_2024),
  #       na.rm = TRUE
  #     )
  
  ## Create colony richness for 2023 and 2024
  demo_richness <- tcrmp_sites %>%
    filter(surveyyear %in% c(2023, 2024)) %>%
    group_by(ID, surveyyear) %>%
    summarise(
      richness = n_distinct(species),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = surveyyear,
      values_from = richness,
      names_prefix = "Richness_"
    )
  
  # #join new columns to demo_data
    Demo <- Demo %>%
      left_join(demo_colony_count, by = "ID") %>%
      left_join(demo_richness, by = "ID")

  richness_average <- demo_richness %>%
    mutate(ID_group = substr(ID, 1, 3)) %>%
    group_by(ID_group) %>%
    summarise(
      Average_colonies = mean(
        c(Richness_2023, Richness_2024),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
# Metadata_depth <- Metadata_depth %>%
#   select(-Coral_richness.x, -Coral_richness.y, -richness, -Coral_richness) %>%
#   left_join(richness_totals, by = "Code")
    
# Join richness totals to Metadata_depth
  Metadata_depth <- Metadata_depth %>%
    left_join(richness_average, by = c("Code" = "ID_group"))



# Functions for site, month, max DHW -------------------

# Convert DHW column to numeric
DHW_data$DHW <- as.numeric(DHW_data$DHW)

# Convert SampleDate to date format
DHW_data$SampleDate <- as.Date(DHW_data$SampleDate)

# Function to select month based on island
choose_month <- function(island) {
  case_when(
    island %in% c("STT", "STJ") ~ 10,
    island == "STX" ~ 11
  )
}

# Make vector of both bleaching event years
bleaching_years <- c(2023, 2024)

# Filter DHW data for 2023 and 2024
DHW_23_24 <- DHW_data %>%
  mutate(
    Year = lubridate::year(SampleDate),
    Month = lubridate::month(SampleDate)
  ) %>%
  filter(Year %in% bleaching_years)

# Function to calculate maximum DHW for survey period

calculate_max_survey_DHW <- function(site, island) {
  
  survey_month <- choose_month(island)
  
  site_data <- DHW_23_24 %>%
    filter(
      trimws(Location) == trimws(site),
      Month == survey_month,
      !is.na(DHW)
    )
  
  max_values <- site_data %>%
    group_by(Year) %>%
    summarise(
      Max_DHW = max(DHW, na.rm = TRUE),
      .groups = "drop"
    )
  
  max_values$Location <- site
  max_values$Island <- island
  
  return(max_values)
}

calculate_max_survey_DHW("Brewers Bay", "STT")


# Loop through every site for max survey DHW -------------------------------
# Make columns for Max_Survey_DHW_2023 and Max_Survey_DHW_2024 (Survey = October for STT & STJ; Survey = November for STX)

Metadata_depth$Max_Survey_DHW_2023 <- NA
Metadata_depth$Max_Survey_DHW_2024 <- NA

# Create columns for annual maximum DHW

Metadata_depth$Max_Annual_DHW_2023 <- NA_real_
Metadata_depth$Max_Annual_DHW_2024 <- NA_real_

for (i in 1:nrow(Metadata_depth)) {
  site <- Metadata_depth$Location[i]
  island <- Metadata_depth$Island[i]
  results <- calculate_max_survey_DHW(site, island)
  value_2023 <- results$Max_DHW[results$Year == 2023]
  value_2024 <- results$Max_DHW[results$Year == 2024]
  if (length(value_2023) > 0) {
    Metadata_depth$Max_Survey_DHW_2023[i] <- value_2023[1]
  }
  
  if (length(value_2024) > 0) {
    Metadata_depth$Max_Survey_DHW_2024[i] <- value_2024[1]
  }
}


#   Function to calculate maximum annual DHW ---------------------

calculate_annual_DHW <- function(site) {
  
  site_data <- DHW_23_24 %>%
    filter(
      trimws(Location) == trimws(site),
      !is.na(DHW)
    )
  
  max_values <- site_data %>%
    group_by(Year) %>%
    summarise(
      Max_DHW = max(DHW, na.rm = TRUE),
      .groups = "drop"
    )
  
  max_values$Location <- site
  return(max_values)
}

calculate_annual_DHW("Brewers Bay")

# Loop through every site for max annual DHW ------------------------------

Metadata_depth$Max_Annual_DHW_2023 <- NA_real_
Metadata_depth$Max_Annual_DHW_2024 <- NA_real_

for (i in 1:nrow(Metadata_depth)) {
  site <- Metadata_depth$Location[i]
  
  results <- calculate_annual_DHW(site)
  
  value_2023 <- results$Max_DHW[results$Year == 2023]
  value_2024 <- results$Max_DHW[results$Year == 2024]
  
  if (length(value_2023) > 0) {
    Metadata_depth$Max_Annual_DHW_2023[i] <- value_2023[1]
  }
  
  if (length(value_2024) > 0) {
    Metadata_depth$Max_Annual_DHW_2024[i] <- value_2024[1]
  }
}


# export ------------------------------------------------------------------

#export data as csv
write.csv(Demo, "exports/Demo_Metadata.csv")
write.csv(Metadata_depth, "exports/Site_Metadata.csv")

    
    

