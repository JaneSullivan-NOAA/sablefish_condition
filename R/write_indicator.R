# Ecosystem and Socioeconomic Profile (ESP) indicator contribution for stocks
# managed under the North Pacific Fisheries Management Council This template is
# required for updating ESP indicator contribution information There are two
# required sections to check or update (see below): Indicator Review and
# Indicator Data Please fill in the text (surrounded by " ") or data as values
# in the line after each field marked with a # and capitalized name (e.g.,
# #INDICATOR_NAME, the next line should be the name of your indicator, for
# example "Annual_Arrowtooth_Biomass_GOA_Model") Note that all fields are
# described in the Alaska ESP User Guide, please see pdf attached to indicator
# assignment email for more details

write_indicator <- function(SUBMISSION_YEAR=YEAR, #Current year of contribution submission
                            INDICATOR_NAME=INDICATOR_NAME, #Composite key (meaning this must be unique to the indicator) based on the ESP naming convention and used for joining ESP data tables. Please see email with your indicator names, and copy/paste name to this location. Note: this name must match the ESP records provided in the email, please do not change. Questions, contact kalei.shotwell@noaa.gov
                            DESCRIPTION=DESCRIPTION, #Brief description of the indicator and why it is important to groundfish fishery management. Please make sure this description includes information on the spatial distribution of the indicator and how the data for the indicator are collected. The spatial resolution can be a cell size dimension (e.g., 5 km resolution for gridded data) or area of sampling for a survey (e.g., Shelikof Strait). The data collection method can be brief (e.g., survey name and gear type, satellite sensor and version, stock assessment model output, fishery observer data, community reports, etc.) and can include a reference to methods detailed elswhere. (Limit to 4000 characters)  
                            STATUS_TRENDS=STATUS_TRENDS, #Information on the current status of the indicator in the context of historical trends. This is similar to the Ecosystem Status Report contribution (Limit to 4000 characters)
                            FACTORS=FACTORS, #Information on the potential causes for observed trends and current status (focus on most recent year). This is similar to the Ecosystem Status Report contribution. (Limit to 4000 characters)
                            IMPLICATIONS=IMPLICATIONS, #Information that briefly answers these questions: What are the implications or impacts of the observed trends on the ecosystem or ecosystem components? What do the trends mean? Why are they important? How can this information be used to inform groundfish management decisions? This is similar to the Ecosystem Status Report contribution. (Limit to 4000 characters)
                            REFERENCES=REFERENCES, #Include any full references that are associated with the indicator. This may include data references such as from an ERDDAP webpage or literature cited (plus the DOI where possible). Please also provide the full reference if there is an associated Ecosystem Status Report contribution that provides more details on the indicator.
                            INDICATOR_YEAR=INDICATOR_YEAR, #List of years for the indicator contribution 
                            INDICATOR_VALUE=INDICATOR_VALUE, #List of data values for the indicator contribuion (must match the year list length)
                            OUTPATH=out_path) {
  
  # example:
  # SUBMISSION_YEAR <- YEAR
  # INDICATOR_NAME <- "Summer_Sablefish_Condition_Female_Age4_GOA_Survey"
  # DESCRIPTION <- "Summer sablefish condition for age-4, immature female sablefish. Body condition was estimated using a length-weight relationship (Laman and Rohan, 2020) from data collected randomly for otoliths in the annual GOA AFSC longline survey (legs 2-7 including slope and cross gully stations), 1996 to present."
  # STATUS_TRENDS <- "This indicator is lagged by one year because it relies on age data, which take longer to provide. The condition of age-4 immature females was below average in 2022."
  # FACTORS <- "Factors influencing the condition of age-4, immature female sablefish in 2022 could include improved environmental conditions, increase in prey availability or prey quality, or reduced inter- or intra-specific competition."
  # IMPLICATIONS <- "Improved condition indicators for age-4, immature female sablefish in 2022 could translate into earlier maturation and faster somatic growth."
  # REFERENCES <- "None"
  # INDICATOR_YEAR <- seq(1990,2000,2)
  # INDICATOR_VALUE <- rnorm(n=length(seq(1990,2000,2)), mean = 0, sd = 0.15)
   
  cat('#Ecosystem and Socioeconomic Profile Contribution', '\n',
      '#INDICATOR_REVIEW ----------------------------------------------------------------------------------------', '\n',
      '#SUBMISSION_YEAR', '\n',
      SUBMISSION_YEAR, '\n',
      '#INDICATOR_NAME', '\n',
      '"', INDICATOR_NAME, '"', '\n',
      '#DESCRIPTION', '\n',
      '"', DESCRIPTION, '"', '\n',
      '#STATUS_TRENDS', '\n',
      '"', STATUS_TRENDS, '"', '\n',
      '#FACTORS', '\n',
      '"', FACTORS, '"', '\n',
      '#IMPLICATIONS', '\n',
      '"', IMPLICATIONS, '"', '\n',
      '#REFERENCES', '\n',
      '"', REFERENCES, '"', '\n',
      '#INDICATOR_DATA ----------------------------------------------------------------------------------------', '\n',
      '#YEAR', '\n',
      INDICATOR_YEAR, '\n',
      '#INDICATOR_VALUE', '\n',
      INDICATOR_VALUE, '\n',
      sep = '', file = paste0(OUTPATH, '/', INDICATOR_NAME, '_', SUBMISSION_YEAR, '.txt')
  )
}
