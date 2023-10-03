# Sablefish condition indicator analysis
# Contact: jane.sullivan@noaa.gov
# Last updated: Oct 2023

# devtools::session_info()
# version  R version 4.2.0 (2022-04-22 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio

# Set up ----

# Most recent survey year 
YEAR <- 2023

dat_path <- paste0("data/", YEAR) # directory where source data is contained
out_path <- paste0("results/", YEAR) # directory for results/output
dir.create(out_path)

libs <- c("knitr", "tidyverse", "devtools", "viridis")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# devtools::install_github("sean-rohan-NOAA/akfishcondition", dep = FALSE)
library(akfishcondition)
pkg_version <- packageVersion("akfishcondition")

# Survey data ----

sable <- read_csv(paste0(dat_path, "/sable_bio_1981_", YEAR, ".csv"),
                  guess_max = 1e6)
sable %>% group_by(year) %>% tally() %>% print(n=Inf)

# manually bring in 2022 LLS specimen data from Cara R 2023-10-02
sable22 <- read_csv("data/2022/sable_specimen_2022.csv") %>%
  mutate(year = 2022) %>%
  dplyr::select(year, specimen_number = specimen, age) 

sable <- sable %>%
  filter(year == 2022) %>% 
  select(-age) %>% 
  left_join(sable22, by = join_by(year, specimen_number)) %>% 
  bind_rows(sable %>% filter(year != 2022)) %>% 
  arrange(year, specimen_number)

names(sable)

# Create some reference tables
sable %>% distinct(maturity, maturitydescription) 
sable %>% distinct(sex, sexdescription) 
sable %>% distinct(stratum, stratumdescription) 
sable %>% distinct(error_flag, errordescription) 

# General clean up
dim(sable)
sable <- sable %>% filter(!is.na(length) & !is.na(weight) & !is.na(maturity))
dim(sable)
sable <- sable %>% filter(error_flag == 0) # incomplete or erroneous specimen dat
dim(sable)
sable <- sable %>% filter(station_number <= 499) # get rid of experimental stations
dim(sable)
sable <- sable %>% filter(!council_sablefish_management_area %in% # just keep legs 3-7 = GOA (omit BSAI), per KS guidance 10/14/20
                            c("Aleutians", "Bering Sea"))
dim(sable)

# defined mature = 1, immature = 0
sable <- sable %>% mutate(mature = ifelse(maturity %in% c(1, 2), 0, 1))

# Survey station data ----
stations <- read_csv(paste0(dat_path, "/sable_survey_stations.csv"),
                     guess_max = 1e6)
dim(stations)
stations %>% dplyr::count(habitat_type)

stations <- stations %>% 
  mutate(habitat = case_when(habitat_type == "Deep cross-shelf gully" ~ "deep_gully",
                             habitat_type == "Shallow cross-shelf gully" ~ "shallow_gully",
                             habitat_type == "Upper continental slope" ~ "slope"))

# habitat and other station info
sable <- sable %>% left_join(stations %>% select(station_number, habitat, active))
sable %>% dplyr::count(active) # check that all stations are active

# KS suggested stations should be cut off >=76. Not sure about this. Mine start
# at 62 and I've removed experimental stations. The stations between 62 and 76
# are active stations in slope habitat in the WGOA and CGOA. Can't see a reason
# to omit them.
range(sable$station_number)
stations %>% 
  filter(between(station_number, 62, 76)) %>%
  dplyr::count(active, rpw_flag, nmfs_area_code, council_sablefish_management_area, habitat)

# Stations 62 - 75 are sampled on leg 2 before the vessel goes over to ketchikan
# sable <- sable %>% filter(station_number >= 76)

# Fishery data ----
fsh <- read_csv(paste0(dat_path, "/sable_fishery_bio_", YEAR, ".csv"),
                guess_max = 1e6)

fsh %>% dplyr::count(maturity_code, maturity_description) # VERY little maturity data, ignore it!
dim(fsh)  
fsh <- fsh %>% filter(!is.na(length) & !is.na(weight) & sex != "U")
dim(fsh)

# Use consistent sex codes to survey
fsh <- fsh %>% mutate(sex = ifelse(sex == "F", 2, 1))

# Keep BSAI and GOA
fsh <- fsh %>% filter(fmp %in% c("BSAI", "GOA"))

fsh %>% dplyr::count(year, sex, fmp) %>% 
  filter(sex == 2) %>% 
  arrange(fmp) %>% 
  print(n = Inf)

# Sampling really picked up 1999. Don't cut off current year
fsh <- fsh %>% filter(between(year, 1999, YEAR))

# EDA - ----

# concern there will be too many mature fish in this group
sable %>% # dplyr::filter(year == 2022) %>% 
  dplyr::filter(sex == 2 & length <= 55) %>% 
  group_by(year) %>% 
  dplyr::summarize(n()) %>% 
  print(n = Inf)

sable %>% 
  filter(sex == 2 & age == 4) %>% 
  group_by(year) %>% 
  dplyr::summarize(n()) %>% 
  print(n = Inf)

sable %>% 
  filter(sex == 2 & age == 4) %>% 
  group_by(year, mature) %>% 
  dplyr::summarize(n()) %>% 
  pivot_wider(id_cols = year, names_from = mature, values_from = `n()`) %>% 
  print(n = Inf)

sable %>% 
  filter(age %in% c(2,3,4)) %>% 
  group_by(year, age) %>% 
  dplyr::summarize(n()) %>% 
  pivot_wider(id_cols = year, names_from = age, values_from = `n()`) %>% 
  print(n = Inf)

# Potential indicator groups ----

# 1: GOA Survey Immature age-4 females ----

sable1 <- sable %>% 
  filter(sex == 2 & age == 4 & mature == 0) %>% 
  mutate(condition_stratum = NA,
         species_code = 205101, # create unique specimen name for each grp
         common_name = "GOA LL Survey: Age-4 immature females",
         # added area biomass = 1 (equal weighting)
         area_biomass = 1)
sable1 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

# sable1 %>% 
#   ggplot(aes(x = log(length), y = log(weight), col = factor(year))) +
#   geom_point() +
#   geom_smooth(method = 'lm')

# We can't reach sample size thresholds of 10 if we split by stratum
# sable1 %>% 
#   dplyr::count(year, stratum) %>% 
#   arrange(stratum) %>% 
#   pivot_wider(id_cols = year, names_from = stratum, values_from = n) %>% 
#   arrange(year) %>% 
#   print(n = Inf)

# Create new "shallow" vs "deep" strata cutoff at 300 m - still can't meet sample sizes
# sable1 <- sable1 %>% 
#   mutate(condition_stratum = ifelse(stratum %in% c(1, 2, 3), "shallow", "deep"))

# sable1 %>% 
#   dplyr::count(year, condition_stratum) %>% 
#   pivot_wider(id_cols = year, names_from = condition_stratum, values_from = n) %>% 
#   arrange(year) %>% 
#   print(n = Inf)

# 2: GOA Survey age-4 males ----

sable2 <- sable %>% 
  filter(sex == 1 & age == 4) %>% 
  mutate(condition_stratum = NA,
         species_code = 205102,
         common_name = "GOA LL Survey: Age-4 immature males",
         area_biomass = 1) 
sable2 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

# 3: GOA Survey Immature females <= 65 cm ----
sable3 <- sable %>% 
  filter(sex == 2 & length <= 65 & mature == 0) %>%
  mutate(condition_stratum = NA,
         species_code = 205103,
         common_name = "GOA LL Survey: Immature juvenile females <= 65 cm",
         # added area biomass = 1 (equal weighting)
         area_biomass = 1)

sable3 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

# 4: GOA Survey Females >= 75 cm ----
sable4 <- sable %>% 
  filter(sex == 2 & length >= 75) %>% 
  mutate(condition_stratum = NA,
         species_code = 205104,
         common_name = "GOA LL Survey: Large females >= 75 cm",
         area_biomass = 1)

sable4 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

# 5: GOA Fishery: large females >= 75 cm ----
sable5 <- fsh %>% 
  filter(sex == 2 & length >= 75 & fmp == "GOA") %>%
  mutate(condition_stratum = NA,
         species_code = 205105,
         common_name = "GOA Fishery: Large females >= 75 cm",
         area_biomass = 1)
sable5 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

sable5 %>% 
  filter(between(year, 2015, 2021)) %>% 
  ggplot(aes(x=length,y=weight, col = factor(year), fill = factor(year))) + 
  geom_point() +
  stat_smooth(method = 'lm', alpha = 0.15) +
  geom_point(data = sable5 %>% 
               filter(year == YEAR),
             aes(x=length,y=weight),
             size = 4) +
  labs(col=NULL, fill=NULL, title = unique(sable5$common_name)) +
  theme_bw(base_size = 14) #+
  
# gear_code (from afsc.norpac lov gear table)
# 1 = non-pelagic/bottom trawl
# 8 = longline
# 6 = pot or trap
# 2 = pot
# 7 = jig
# 3 = mixed unknown trawl
sable5 %>% group_by(year, gear) %>% tally() %>% print(n=Inf)
sable5 %>% 
  dplyr::count(year, gear) %>% 
  pivot_wider(id_cols = year, names_from = gear, values_from = n) %>% 
  print(n = Inf) 

# 6: BSAI Fishery: large females >= 75 cm ----
sable6 <- fsh %>% 
  filter(sex == 2 & length >= 75 & fmp == "BSAI") %>%
  mutate(condition_stratum = NA,
         species_code = 205106,
         common_name = "BSAI Fishery: Large females >= 75 cm",
         area_biomass = 1)
sable6 %>% 
  dplyr::count(year) %>% 
  print(n = Inf)

# Prep data for condition analysis ----
sable_sub <- sable1 %>% 
  bind_rows(sable2) %>% 
  bind_rows(sable3) %>%
  bind_rows(sable4) %>% 
  bind_rows(sable5) %>% 
  bind_rows(sable6) %>% 
  select(common_name, species_code, year, 
         condition_stratum, area_biomass, length, weight)

(sable_options <- unique(sable_sub$species_code))

# Calculate length weight residuals -----
for(i in 1:length(sable_options)) {
  
  # Separate slope for each stratum. Bias correction according to Brodziak, no outlier detection.
  sable_df <- calc_lw_residuals(len = sable_sub$length[sable_sub$species_code == sable_options[i]], 
                                wt = sable_sub$weight[sable_sub$species_code == sable_options[i]], 
                                year = sable_sub$year[sable_sub$species_code == sable_options[i]], 
                                stratum = sable_sub$condition_stratum[sable_sub$species_code == sable_options[i]], 
                                make_diagnostics = TRUE, # Make diagnostics
                                bias.correction = TRUE, # Bias correction turned on
                                outlier.rm = FALSE, # Outlier removal turned on
                                region = "sable_indicator_group", #"AI",
                                species_code = sable_sub$species_code[sable_sub$species_code == sable_options[i]],
                                include_ci = TRUE)
  sable_sub$resid_mean[sable_sub$species_code == sable_options[i]] <- sable_df$lw.res_mean
  sable_sub$resid_lwr[sable_sub$species_code == sable_options[i]] <- sable_df$lw.res_lwr
  sable_sub$resid_upr[sable_sub$species_code == sable_options[i]] <- sable_df$lw.res_upr
  
}

# Estimate mean and std. err for each stratum, filter out strata with less than 10 samples
sable_resids <- sable_sub %>% 
  dplyr::group_by(common_name, species_code, year, condition_stratum, area_biomass) %>% 
  # dplyr::group_by(COMMON_NAME, SPECIES_CODE, YEAR, INPFC_STRATUM, AREA_BIOMASS) %>%
  dplyr::summarise(stratum_resid_mean = mean(resid_mean),
                   stratum_resid_sd = sd(resid_mean),
                   n = n()) %>%
  dplyr::filter(n >= 10) %>%
  dplyr::mutate(stratum_resid_se = stratum_resid_sd/sqrt(n))

# Weight strata by biomass 
for(i in 1:length(sable_options)) {
  
  sable_resids$weighted_resid_mean[sable_resids$species_code == sable_options[i]] <- weight_lw_residuals(
    residuals = sable_resids$stratum_resid_mean[sable_resids$species_code == sable_options[i]],
    year = sable_resids$year[sable_resids$species_code == sable_options[i]],
    # stratum = rep("no_strata", nrow(sable_resids)),
    stratum = sable_resids$condition_stratum[sable_resids$species_code == sable_options[i]],
    stratum_biomass = sable_resids$area_biomass[sable_resids$species_code == sable_options[i]])
  
  sable_resids$weighted_resid_se[sable_resids$species_code == sable_options[i]] <- weight_lw_residuals(
    residuals = sable_resids$stratum_resid_se[sable_resids$species_code == sable_options[i]],
    year = sable_resids$year[sable_resids$species_code == sable_options[i]],
    # stratum = rep("no_strata", nrow(sable_resids)),
    stratum = sable_resids$condition_stratum[sable_resids$species_code == sable_options[i]],
    stratum_biomass = sable_resids$area_biomass[sable_resids$species_code == sable_options[i]])
}

# Biomass-weighted residual and SE by year
sable_ann_mean_resid_df <- sable_resids %>% 
  dplyr::group_by(year, common_name) %>%
  dplyr::summarise(mean_wt_resid = mean(weighted_resid_mean),
                   se_wt_resid = mean(weighted_resid_se))

# Plot adults -----

bs <- 15 

sable_ann_mean_resid_df %>% 
  filter(grepl("Large", common_name)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, 
               y = mean_wt_resid), 
           stat = "identity", 
           fill = "goldenrod", 
           color = "black") +
  geom_errorbar(aes(x = year, 
                    ymax = mean_wt_resid + 1.96 * se_wt_resid,
                    ymin = mean_wt_resid - 1.96 * se_wt_resid),
                width = 0.2) +
  geom_hline(yintercept = 0) +
  # geom_point(data = old_indicator_dat, 
  #            aes(x = YEAR, 
  #                y = ymeans)) +
  facet_wrap(~common_name, ncol = 1) + #, scales = "free_y"
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Length-weight residual (ln(g))") +
  theme_minimal(base_size = bs) +
  ggtitle("Condition indicators for large female sablefish\n")

ggsave(paste0(out_path, "/large_sable_condition_", YEAR, ".png"),
       width = 6, height = 7, units = "in", bg = 'white')

# Plot smaller fish ----

sable_ann_mean_resid_df %>% 
  filter(grepl("Age-4|Immature|immature|age-4", common_name)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, 
               y = mean_wt_resid), 
           stat = "identity", 
           fill = "plum", 
           color = "black") +
  geom_errorbar(aes(x = year, 
                    ymax = mean_wt_resid + 1.96 * se_wt_resid,
                    ymin = mean_wt_resid - 1.96 * se_wt_resid),
                width = 0.2) +
  geom_hline(yintercept = 0) +
  # geom_point(data = old_indicator_dat, 
  #            aes(x = YEAR, 
  #                y = ymeans)) +
  facet_wrap(~common_name, ncol = 1) + #, scales = "free_y"
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Length-weight residual (ln(g))") +
  theme_minimal(base_size = bs) +
  ggtitle("Condition indicators for small sablefish\n")

ggsave(paste0(out_path, "/small_sable_condition_", YEAR, ".png"),
       width = 6, height = 7, units = "in", bg = 'white')

## Recommended indicators -----

sable_ann_mean_resid_df %>% #ungroup() %>% distinct(common_name)
  filter(common_name %in% c("GOA LL Survey: Age-4 immature females",
                            "GOA LL Survey: Large females >= 75 cm",
                            "GOA Fishery: Large females >= 75 cm",
                            "BSAI Fishery: Large females >= 75 cm")) %>%
  mutate(common_name = factor(common_name, 
                              levels = c("GOA LL Survey: Age-4 immature females",
                                         "GOA LL Survey: Large females >= 75 cm",
                                         "GOA Fishery: Large females >= 75 cm",
                                         "BSAI Fishery: Large females >= 75 cm"),
                              ordered = TRUE)) %>% 
  ggplot() + 
  geom_bar(aes(x = year, 
               y = mean_wt_resid), 
           stat = "identity", 
           fill = "plum", 
           color = "black") +
  geom_errorbar(aes(x = year, 
                    ymax = mean_wt_resid + 1.96 * se_wt_resid,
                    ymin = mean_wt_resid - 1.96 * se_wt_resid),
                width = 0.2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~common_name, ncol = 1, scales = "free_y") + 
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Length-weight residual (ln(g))") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste0("Sablefish condition indicators for ", YEAR, "\nwith 95% confidence intervals\n"))

ggsave(paste0(out_path, "/recommended_sable_condition_", YEAR, ".png"),
       width = 6, height = 7.5, units = "in", bg = 'white')

# # Restandardize ----
# sable_ann_mean_resid_df %>% 
#   filter(common_name == "Sablefish: age-4 immature females") %>% 
#   select(year, mean_wt_resid) %>% 
#   mutate(rescale = scales::rescale(mean_wt_resid, -1, 1))

# Write results ----

out_file <- sable_sub %>% 
  group_by(common_name, year) %>% 
  dplyr::summarize(n = n()) %>% 
  left_join(sable_ann_mean_resid_df) %>% 
  select(indicator = common_name, year, n, mean_wt_resid, se_wt_resid) %>% 
  dplyr::arrange(indicator)
write_csv(out_file, paste0(out_path, "/sable_condition_indicators_", YEAR, ".csv"))

out_file %>% 
  filter(indicator %in% c("GOA LL Survey: Age-4 immature females",
                          "GOA LL Survey: Large females >= 75 cm",
                          "GOA Fishery: Large females >= 75 cm",
                          "BSAI Fishery: Large females >= 75 cm")) %>%
  write_csv(paste0(out_path, "/recommended_sable_condition_indicators_", YEAR, ".csv"))

# reformat output for new esp submission tool ----

# Summer_Sablefish_Condition_Female_Adult_GOA_Survey
out_file %>% 
  filter(indicator == "GOA LL Survey: Large females >= 75 cm") %>% 
  mutate(indicator = 'Summer_Sablefish_Condition_Female_Adult_GOA_Survey') %>% 
  dplyr::select(YEAR = year, INDICATOR_NAME = indicator, DATA_VALUE = mean_wt_resid) %>% 
  write_csv(paste0(out_path, "/Summer_Sablefish_Condition_Female_Adult_GOA_Survey_", YEAR, ".csv"))

out_file %>% 
  filter(indicator == "GOA LL Survey: Age-4 immature females") %>% 
  mutate(indicator = 'Summer_Sablefish_Condition_Female_Age4_GOA_Survey') %>% 
  dplyr::select(YEAR = year, INDICATOR_NAME = indicator, DATA_VALUE = mean_wt_resid) %>% 
  write_csv(paste0(out_path, "/Summer_Sablefish_Condition_Female_Age4_GOA_Survey_", YEAR, ".csv"))

out_file %>% 
  filter(indicator == "BSAI Fishery: Large females >= 75 cm") %>% 
  mutate(indicator = 'Annual_Sablefish_Condition_Female_Adult_BSAI_Fishery') %>% 
  select(YEAR = year, INDICATOR_NAME = indicator, DATA_VALUE = mean_wt_resid) %>% 
  write_csv(paste0(out_path, "/Annual_Sablefish_Condition_Female_Adult_BSAI_Fishery_", YEAR, ".csv"))

out_file %>% 
  filter(indicator == "GOA Fishery: Large females >= 75 cm") %>% 
  mutate(indicator = 'Annual_Sablefish_Condition_Female_Adult_GOA_Fishery') %>% 
  select(YEAR = year, INDICATOR_NAME = indicator, DATA_VALUE = mean_wt_resid) %>% 
  write_csv(paste0(out_path, "/Annual_Sablefish_Condition_Female_Adult_GOA_Fishery_", YEAR, ".csv"))

# reformat again for the latest submission tool (2023) ----
source('R/write_indicator.R')

# GOA LL Survey: Age-4 immature females:
tmp <- "GOA LL Survey: Age-4 immature females"

INDICATOR_YEAR <- out_file %>% filter(indicator == tmp) %>% 
  pull(year) %>% paste(collapse = " ", sep = " ")

INDICATOR_VALUE <- out_file %>% filter(indicator == tmp) %>% 
  pull(mean_wt_resid) %>% round(digits = 4) %>% paste(collapse = " ", sep = " ")

write_indicator(SUBMISSION_YEAR = YEAR,
                INDICATOR_NAME = "Summer_Sablefish_Condition_Female_Age4_GOA_Survey",
                DESCRIPTION = "Summer sablefish condition for age-4, immature female sablefish. Body condition was estimated using a length-weight relationship (Laman and Rohan, 2020) from data collected randomly for otoliths in the annual GOA AFSC longline survey (legs 2-7 including slope and cross gully stations), 1996 to present.",
                STATUS_TRENDS = "This indicator is lagged by one year because it relies on age data, which take longer to provide. The condition of age-4 immature females was below average in 2022.",
                FACTORS = "Factors influencing the condition of age-4, immature female sablefish in 2022 could include poor environmental conditions, reduced in prey availability or prey quality, or increased inter- or intra-specific competition.",
                IMPLICATIONS = "Poor condition indicators for age-4, immature female sablefish in 2022 could translate into slower maturation and somatic growth, or reduced survival rates.",
                REFERENCES = "Rohan S, O'Leary C (2023). _akfishcondition: Groundfish morphometric condition indicator_. R package version 3.1.0.",
                INDICATOR_YEAR = INDICATOR_YEAR,
                INDICATOR_VALUE = INDICATOR_VALUE,
                OUTPATH = out_path)

# GOA LL Survey: Large females >= 75 cm
tmp <- "GOA LL Survey: Large females >= 75 cm"

INDICATOR_YEAR <- out_file %>% filter(indicator == tmp) %>% 
  pull(year) %>% paste(collapse = " ", sep = " ")

INDICATOR_VALUE <- out_file %>% filter(indicator == tmp) %>% 
  pull(mean_wt_resid) %>% round(digits = 4) %>% paste(collapse = " ", sep = " ")

write_indicator(SUBMISSION_YEAR = YEAR,
                INDICATOR_NAME = "Summer_Sablefish_Condition_Female_Adult_GOA_Survey",
                DESCRIPTION = "Summer sablefish condition for large adult (>=750 mm) female sablefish. Body condition was estimated using a length-weight relationship (Laman and Rohan, 2020) from data collected randomly for otoliths in the annual GOA AFSC longline survey (legs 2-7 including slope and cross gully stations), 1996 to present.",
                STATUS_TRENDS = "The condition of large adult (>=750 mm) female sablefish improved from below average in 2022 to average or slightly above average in 2023.",
                FACTORS = "Factors influencing the condition of large adult (>=750 mm) female sablefish in 2023 could include improved environmental conditions, increase in prey availability or prey quality, or reduced inter- or intra-specific competition relative to 2022.",
                IMPLICATIONS = "Improved condition indicators for large adult (>=750 mm) female sablefish in 2023 could translate into a lower likelihood of skip spawning, increased somatic growth rates, or increased survival rates relative to 2022.",
                REFERENCES = "Rohan S, O'Leary C (2023). _akfishcondition: Groundfish morphometric condition indicator_. R package version 3.1.0.",
                INDICATOR_YEAR = INDICATOR_YEAR,
                INDICATOR_VALUE = INDICATOR_VALUE,
                OUTPATH = out_path)

# GOA Fishery: Large females >= 75 cm
tmp <- "GOA Fishery: Large females >= 75 cm"

INDICATOR_YEAR <- out_file %>% filter(indicator == tmp) %>% 
  pull(year) %>% paste(collapse = " ", sep = " ")

INDICATOR_VALUE <- out_file %>% filter(indicator == tmp) %>% 
  pull(mean_wt_resid) %>% round(digits = 4) %>% paste(collapse = " ", sep = " ")

write_indicator(SUBMISSION_YEAR = YEAR,
                INDICATOR_NAME = "Annual_Sablefish_Condition_Female_Adult_GOA_Fishery",
                DESCRIPTION = "Annual sablefish condition for large adult (>=750 mm) female sablefish in the GOA sablefish fishery. Body condition was estimated using a length-weight relationship (Laman and Rohan, 2020) from data collected randomly for otoliths in the annual GOA fishery, 1999 to present.",
                STATUS_TRENDS = "The condition of large adult (>=750 mm) female sablefish improved from below average in 2022 to average or slightly below average in 2023.",
                FACTORS = "Factors influencing the condition of large adult (>=750 mm) female sablefish in 2023 could include improved environmental conditions, increase in prey availability or prey quality, or reduced inter- or intra-specific competition in the GOA relative to 2022.",
                IMPLICATIONS = "Improved condition indicators for large adult (>=750 mm) female sablefish in 2023 could translate into a lower likelihood of skip spawning, increased somatic growth rates, or increased survival rates relative to 2022.",
                REFERENCES = "Rohan S, O'Leary C (2023). _akfishcondition: Groundfish morphometric condition indicator_. R package version 3.1.0.",
                INDICATOR_YEAR = INDICATOR_YEAR,
                INDICATOR_VALUE = INDICATOR_VALUE,
                OUTPATH = out_path)

# BSAI Fishery: Large females >= 75 cm
tmp <- "BSAI Fishery: Large females >= 75 cm"

INDICATOR_YEAR <- out_file %>% filter(indicator == tmp) %>% 
  pull(year) %>% paste(collapse = " ", sep = " ")

INDICATOR_VALUE <- out_file %>% filter(indicator == tmp) %>% 
  pull(mean_wt_resid) %>% round(digits = 4) %>% paste(collapse = " ", sep = " ")

write_indicator(SUBMISSION_YEAR = YEAR,
                INDICATOR_NAME = "Annual_Sablefish_Condition_Female_Adult_BSAI_Fishery",
                DESCRIPTION = "Annual sablefish condition for large adult (>=750 mm) female sablefish in the BSAI sablefish fishery. Body condition was estimated using a length-weight relationship (Laman and Rohan, 2020) from data collected randomly for otoliths in the annual GOA fishery, 1999 to present.",
                STATUS_TRENDS = "There was insufficient data in the BSAI sablefish fishery to estimate a condition factor for 2023. I recommend removing this indicator from the sablefish ESP.",
                FACTORS = "Insufficient data.",
                IMPLICATIONS = "Insufficient data.",
                REFERENCES = "Rohan S, O'Leary C (2023). _akfishcondition: Groundfish morphometric condition indicator_. R package version 3.1.0.",
                INDICATOR_YEAR = INDICATOR_YEAR,
                INDICATOR_VALUE = INDICATOR_VALUE,
                OUTPATH = out_path)

# Comment left in 2023 indicator submission: I recommend removing this indicator from the sablefish ESP. There is insufficient data for it to be meaningful and that is unlikely to change in the future.

# move diagnostics ----

# clean up the default printing of diagnostic files for consistency with this
# project - move diagnostic output, delete default folder
diag_out <- file.path(paste0(out_path, "/diagnostics"))
dir.create(diag_out)

default_diag <- file.path("output/sable_indicator_group")
diag_ls <- list.files(default_diag)
diag_ls <- paste0(default_diag, "/", diag_ls)
file.copy(from = diag_ls, to = diag_out, overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

unlink(file.path("output"), recursive = TRUE)

# weight-at-age plots ----
sable %>% 
  select(year, sex, age, weight) %>% 
  filter(!is.na(age) & sex %in% c(1, 2)) %>%
  group_by(year, sex, age) %>% 
  dplyr::summarise(weight = mean(weight) %>% round(4)) %>%  
  ungroup() %>% 
  mutate(Year = as.character(year),
         Sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
         Age = factor(age),
         cohort = year - age,
         Cohort = as.factor(cohort))-> df

library(ggthemes)
pal <- ggthemes::canva_pal("Warm and cool")(4) 

# By cohort
df_cohort <- df %>% 
  filter(cohort >= 2000 & cohort <= YEAR-3 & age >=2 & age <= 5) %>% 
  droplevels()

# Axis ticks for plot (see helper.r tickr() fxn for details)
# axis <- tickr(df_cohort, year, 2)

ggplot(df_cohort, aes(year, weight, colour = Cohort, group = Cohort)) +
  # geom_line(size = 1) +
  # stat_smooth(method = 'lm', se = F) +
  geom_point(aes(fill = Cohort), show.legend = FALSE, size = 1) +
  facet_grid(~Sex) +
  labs(x = "Year", y = "Weight-at-age (kg)\n", colour = "Cohort") +
  guides(colour = guide_legend(ncol = 9)) +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df_cohort$Cohort))) +
  theme(legend.position = "bottom") #+
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels)# -> waa_cohort_plot

# ggsave("figures/waa_cohort.png", dpi = 300, height = 5, width = 7, units = "in")

df %>% 
  filter(Age %in% c("4", "5", "6")) %>% 
  droplevels() -> df

df %>% 
  group_by(Age, Sex) %>% 
  dplyr::summarize(mean_weight = mean(weight, na.rm = TRUE)) -> means

# axis <- tickr(df, year, 5)

ggplot(df, 
       aes(year, weight, group = Age, colour = Age)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ Sex, ncol = 1, scales = "free") +
  geom_hline(data = means, aes(colour = Age, yintercept = mean_weight), alpha = 0.4, linetype = 2) + 
  labs(x = "Year", y = "Weight-at-age (kg)\n", colour = "Age") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df$Age))) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")#+
  # scale_x_continuous(breaks = axis$breaks, labels = axis$labels)

ggsave(paste0(out_path, "/sable_srv_waa.png"))

# length-at-age ----
sable %>% 
  select(year, sex, age, length) %>% 
  filter(!is.na(age) & sex %in% c(1, 2)) %>%
  group_by(year, sex, age) %>% 
  dplyr::summarise(length = mean(length) %>% round(4)) %>%  
  ungroup() %>% 
  mutate(Year = as.character(year),
         Sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
         Age = factor(age),
         cohort = year - age,
         Cohort = as.factor(cohort))-> df

# By cohort
df_cohort <- df %>% 
  filter(cohort >= 2000 & cohort <= YEAR-3 & age >=2 & age <= 5) %>% 
  droplevels()

# Axis ticks for plot (see helper.r tickr() fxn for details)
# axis <- tickr(df_cohort, year, 2)

ggplot(df_cohort, aes(year, length, colour = Cohort, group = Cohort)) +
  geom_line(size = 1) +
  geom_point(aes(fill = Cohort), show.legend = FALSE, size = 1) +
  facet_grid(~Sex) +
  labs(x = "Year", y = "length-at-age (cm)\n", colour = "Cohort") +
  guides(colour = guide_legend(ncol = 9)) +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df_cohort$Cohort))) +
  theme(legend.position = "bottom") #+
# scale_x_continuous(breaks = axis$breaks, labels = axis$labels)# -> waa_cohort_plot

df %>% 
  filter(Age %in% c("4", "5", "6")) %>% 
  droplevels() -> df
df %>% 
  group_by(Age, Sex) %>% 
  dplyr::summarize(mean_length = mean(length, na.rm = TRUE)) -> means

# axis <- tickr(df, year, 5)

ggplot(df, 
       aes(year, length, group = Age, colour = Age)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ Sex, ncol = 1, scales = "free") +
  # geom_hline(data = means, aes(colour = Age, yintercept = mean_length), alpha = 0.4, linetype = 2) + 
  labs(x = "Year", y = "Length-at-age (cm)\n", colour = "Age") +
  scale_colour_manual(values = colorRampPalette(pal)(n_distinct(df$Age))) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")#+
# scale_x_continuous(breaks = axis$breaks, labels = axis$labels)

ggsave(paste0(out_path, "/sable_srv_laa.png"))
