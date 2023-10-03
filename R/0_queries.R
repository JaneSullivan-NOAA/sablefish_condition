# Queries for sablefish condition factor
# Contact: jane.sullivan@noaa.gov
# Last updated: Sep 2023

# devtools::session_info()
# version  R version 4.2.0 (2022-04-22 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio

# Set up ----

# Most recent survey year 
YEAR <- 2023

# Create a year subdirectory to store annual data used for condition factors
dat_path <- paste0("data/", YEAR)
dir.create(dat_path)
raw_path <- paste0(dat_path, "/raw") # raw data
dir.create(raw_path) 

libs <- c("tidyverse", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# connect to longline database
conn <- odbcDriverConnect(connection="Driver={SQL Server};server=161.55.120.71,1919;database=LONGLINE;trusted_connection=yes;")

# afsc/akfin dbs
db <- read_csv("database.csv") # user-specific usernames and passwords, not tracked on github

# database_afsc <- "afsc"
# username_afsc <- db$username_afsc 
# password_afsc <- db$password_afsc 
# channel_afsc <- odbcConnect(database_afsc, uid = username_afsc, pwd = password_afsc, believeNRows=FALSE)

database_akfin <- "akfin" 
username_akfin <- db$username_akfin
password_akfin <- db$password_akfin
channel_akfin <- odbcConnect(database_akfin, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

# Sablefish survey specimen data -----
query <- "select  *
          from    Age_View"

sable <- sqlQuery(conn, query)
write_csv(sable, paste0(raw_path, "/sable_bio_", min(sable$Year), "_", YEAR, ".csv"))

sable <- sable %>% rename_all(tolower) 

# Create some reference tables
(sable %>% distinct(maturity, maturitydescription) %>% write_csv("data/maturity_codes.csv"))
(sable %>% distinct(sex, sexdescription) %>% write_csv("data/sex_codes.csv"))
(sable %>% distinct(stratum, stratumdescription) %>% write_csv("data/stratum_decriptions.csv"))
(sable %>% distinct(error_flag, errordescription) %>% write_csv("data/error_codes.csv"))

sable %>% 
  # select(-c(maturitydescription, sexdescription, stratumdescription, errordescription)) %>% 
  write_csv(paste0(dat_path, "/sable_bio_", min(sable$year), "_", YEAR, ".csv"))

# Survey stations -----

query <- "select  *
          from Stations_View_AKFIN"


stations <- sqlQuery(conn, query)
write_csv(stations, paste0(raw_path, "/sable_survey_stations.csv"))

stations <- stations %>% rename_all(tolower) 

stations %>% write_csv(paste0(dat_path, "/sable_survey_stations.csv"))

# Observer/fishery specimen data ----

query <- paste0("select   year, nmfs_area, gear, species,
                          length, weight, age, maturity_code, maturity_description,
                          sex, bottom_depth_fathoms, haul_offload_date,
                          sample_system, specimen_type, specimen_type_description,
                          performance
                 from     norpac.debriefed_age
                 where    species in '203'") 

# flat version
query <- paste0("select   year, nmfs_area, gear, species,
                          length, weight, age, maturity_code, maturity_description,
                          sex, bottom_depth_fathoms, haul_offload_date,
                          performance
                 from     norpac.debriefed_age_flat_mv
                 where    species in '203'") 

fsh <- sqlQuery(channel_akfin, query) %>% 
  write_csv(paste0(raw_path, "/sable_fishery_bio_", YEAR, ".csv"))

# NMFS area look up 
query <- "select  distinct  fmp_area as fmp, fmp_subarea, 
                            reporting_area_code as nmfs_area
          from              council.comprehensive_blend_ca"

nmfs_area_lookup <- sqlQuery(channel_akfin, query) %>% rename_all(tolower)
nmfs_area_lookup <- nmfs_area_lookup[complete.cases(nmfs_area_lookup),]
fsh %>% 
  rename_all(tolower) %>%
  mutate(nmfs_area = as.character(nmfs_area)) %>% 
  left_join(nmfs_area_lookup)  %>% 
  write_csv(paste0(dat_path, "/sable_fishery_bio_", YEAR, ".csv"))

