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

source("conjoint_functions.R")

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

# Save CSV for reference
write_csv(global_data, "data/nbh_clean_conjoint_global.csv")

# Save RDS to preserve factor coding for use in replication.R
write_rds(global_data, "data/nbh_clean_conjoint_global.rds")

