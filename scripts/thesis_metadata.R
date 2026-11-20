library(dplyr)
library(tidyverse)
library(readxl)


# read in data ------------------------------------------------------------
#read in Lindsay data
tcrmp_master <- read.csv("~/Downloads/TCRMP_coralhealth_condition_master_May2002_Dec2025_external.csv")
#read in my data
Demo <- read_xlsx("~/Downloads/site_map.xlsx", sheet = "Demo")
Metadata <- read_xlsx("~/Downloads/site_map.xlsx", sheet = "Metadata")

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

#matched site names to locations and codes
  tcrmp_sites <- tcrmp_master %>% 
    select(surveyyear, period, location, transect, colony_id, species) %>% 
    filter(location %in% c("Grammanik Tiger FSA",
          "College Shoal East",
          "Meri Shoal",
          "Black Point",
          "Seahorse Cottage Shoal",
           "Flat Cay",
           "Lang Bank EEMP",
           "Lang Bank Red Hind FSA",
           "Hind Bank East FSA",
           "Botany Bay",
           "Brewers Bay",
           "Jacks Bay",
           "Savana",
           "South Capella")) %>% 
    left_join(Metadata %>% 
                select("Location", "Code"), by = c("location" = "Location")) %>% 
    mutate(ID = paste0(
      Code, "_",
      transect))

#create colony count

  demo_colony_count <- tcrmp_sites %>% 
    group_by(ID) %>% 
    summarise(colony_count = n_distinct(colony_id)) %>% 
    ungroup()

  
  #create richness
  demo_richness <- tcrmp_sites %>% 
    group_by(ID) %>% 
    summarise(richness = n_distinct(species)) %>% 
    ungroup()

#join new columns to demo_data
  Demo <- Demo %>% 
    left_join(demo_colony_count, by = "ID") %>% 
    left_join(demo_richness, by = "ID")
    
    


# data cleanup ------------------------------------------------------------

#make distinction for mesophotic (>30m)/shallow 

Metadata_depth <- Metadata %>%
  mutate(ReefType = ifelse(Depth > 30, "Mesophotic", "Shallow"))

    
#sum coral colonies 
colony_totals <- demo_colony_count %>%
  mutate(Code = substr(ID, 1, 3)) %>%
  group_by(Code) %>%
  summarise(Coral_colonies = sum(colony_count, na.rm = TRUE))
#join to metadata_depth
Metadata_depth <- Metadata_depth %>%
  select(-Coral_colonies) %>%
  left_join(colony_totals, by = "Code")    
    
  #sum richness 
richness_totals <- tcrmp_sites %>%
  mutate(Code = substr(ID, 1, 3)) %>%
  group_by(Code) %>%
  summarise(Coral_richness = n_distinct(species)) %>%
  ungroup()
Metadata_depth <- Metadata_depth %>%
  select(-Coral_richness.x, -Coral_richness.y, -richness, -Coral_richness) %>%
  left_join(richness_totals, by = "Code")
    
Metadata_depth <- Metadata_depth %>%
  left_join(
    richness_totals %>%
      rename(Colony_richness = Coral_richness),
    by = "Code"
  )

# export ------------------------------------------------------------------

#export data as csv
write.csv(Demo, "data/Demo_Metadata.csv")
write.csv(Metadata_depth, "data/Site_Metadata.csv")

    
    

