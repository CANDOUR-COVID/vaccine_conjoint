#### 0. Dependencies ####
library(stargazer)
library(tidyverse)
library(broom)
library(texreg)
library(xtable)
library(pals)
library(ggridges)
library(ggpubr)
set.seed(89)

rm(list = ls())

source("conjoint_functions.R")
eval(parse("wtp_functions.R", encoding="UTF-8"))
source("IPUMS_fn.R")

#### 0. Cleaning and Combining ####

# Creating homogenized demographics variables

for (country in c("AUS", "BR", "CAN", "CHL", "CHN", "COL", "FR",
                  "IT", "SP", "UK", "US", "UGA", "IND")) {
  assign(paste0("data_", country), read_csv(paste0("data/data_", country, ".csv")))
  assign(paste0("data_", country), IPUMS_contract(get(paste0("data_", country))))
  assign(paste0("data_", country), IPUMS_education(get(paste0("data_", country))))
  assign(paste0("data_", country), IPUMS_employment(get(paste0("data_", country))))
  assign(paste0("data_", country), IPUMS_work(get(paste0("data_", country))))
  
  assign(paste0("data_", country), select(get(paste0("data_", country)), -c(starts_with("Q"))))
}

# Merging weights

for (country in c("AUS", "BR", "CHN", "COL", "FR",
                  "IT", "UK", "US")) {
  assign(paste0(country, "_w"), read_csv(paste0("weights/", country, "_w.csv")))
  assign(paste0("data_", country), left_join(get(paste0("data_", country)), get(paste0(country, "_w")), by = "id"))
  assign(paste0("data_", country), mutate(get(paste0("data_", country)), weights = weights*length(paste0("data_", country))/100))
}

data_CHL$eq5d_scale_pre <- as.character(data_CHL$eq5d_scale_pre)
data_CHN$eq5d_scale_pre <- as.character(data_CHN$eq5d_scale_pre)
data_IT$eq5d_scale_pre <- as.character(data_IT$eq5d_scale_pre)

data <- bind_rows(data_AUS, data_BR, data_CAN, data_CHL, data_CHN, data_COL, 
                  data_FR, data_IT, data_IND, data_SP, data_UGA, data_UK, data_US)

data <- data %>%
  select(-X1)

write_csv(data, "data/data_combined.csv")

#### 1. Format ####

# Read in data
country_data <- list()

country_data[["can"]] <- read_csv("../CANDOUR/data/data_CAN.csv")
country_data[["col"]] <- read_csv("../CANDOUR/data/data_col.csv")
country_data[["esp"]] <- read_csv("../CANDOUR/data/data_SP.csv")
country_data[["usa"]] <- read_csv("../CANDOUR/data/data_US.csv")
country_data[["aus"]] <- read_csv("../CANDOUR/data/data_AUS.csv")
country_data[["uk"]] <- read_csv("../CANDOUR/data/data_UK.csv") %>% rename(int_pol_implem_6 = Q14.8_6)
country_data[["fra"]] <- read_csv("../CANDOUR/data/data_FR.csv")
country_data[["ita"]] <- read_csv("../CANDOUR/data/data_IT.csv")
country_data[["chn"]] <- read_csv("../CANDOUR/data/data_CHN.csv") %>% mutate(ideology = NA)
country_data[["bra"]] <- read_csv("../CANDOUR/data/data_BR.csv")
country_data[["chl"]] <- read_csv("../CANDOUR/data/data_CHL.csv")
country_data[["uga"]] <- read_csv("../CANDOUR/data/data_UGA.csv") %>% rename(int_pol_implem_6 = Q14.8_6)
country_data[["ind"]] <- read_csv("../CANDOUR/data/data_IND.csv") %>% rename(int_pol_implem_6 = Q14.8_6)

data <- read_csv("data/data_combined.csv")

## First we recode the data:
# To translate survey responses across countries into English
# To create binned or dichotomous versions of variables used in the heterogeneity analysis
# Please see 'conjoint_functions.R' for the generic code
country_codes <- c("aus","bra","can","chl","chn","col",
                   "fra","ind","ita","esp","uga","uk","usa")

for (country in country_codes) {
  
  country_data[[country]] <- recode_inc(country_data[[country]], 
                                        ccode = country)
}

# Recode data for conjoint format
global_data <- recode_for_conjoint(country_data[[1]])
for (i in 2:length(country_data)) {
  global_data <- rbind(global_data, recode_for_conjoint(country_data[[i]]))
}

# Recode data for wtp format
wtp_data <- recode_for_wtp(data)

# Save CSV for reference
write_csv(global_data, "data/nbh_clean_conjoint_global.csv")
write_csv(wtp_data, "data/clean_wtp_global.csv")

# Save RDS to preserve factor coding for use in replication.R
write_rds(global_data, "data/nbh_clean_conjoint_global.rds")
write_rds(wtp_data, "data/clean_wtp_global.rds")

