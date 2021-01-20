setwd('C:/Users/Matias/Dropbox/Predoctoral/Vaccine Study/Github/CANDOUR/')

rm(list = ls())

source("IPUMS cleaner/IPUMS_fn.R")

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