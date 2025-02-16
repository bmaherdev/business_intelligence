###Install packages
install.packages(tidyverse)

###Load libraries
library(tidyverse)

###Get working directory
getwd()

###Import diabetes datasets
diabetes_2013 <- read_csv("DiabetesAtlas_CountyData_2013.csv", skip = 2)
diabetes_2015 <- read_csv("DiabetesAtlas_CountyData_2015.csv", skip = 2)
diabetes_2017 <- read_csv("DiabetesAtlas_CountyData_2017.csv", skip = 2)
diabetes_2020 <- read_csv("DiabetesAtlas_CountyData_2020.csv", skip = 2)

###Import income dataset
income <- read_csv("economics_24.csv")

###Import physician datasets
physician_2013 <- read_csv("2013_Rate_and_Rank.csv")
physician_2015 <- read_csv("2015_Rate_and_Rank.csv")
physician_2017 <- read_csv("2017_Rate_and_Rank.csv")
physician_2020 <- read_csv("2020_Rate_and_Rank.csv")
