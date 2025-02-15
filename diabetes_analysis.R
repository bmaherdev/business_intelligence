###Install packages
install.packages(tidyverse)

###Load libraries
library(tidyverse)

###Get working directory
getwd()

###Import the datasets
diabetes_2013 <- read_csv("DiabetesAtlas_CountyData_2013.csv", skip = 2)
diabetes_2015 <- read_csv("DiabetesAtlas_CountyData_2015.csv", skip = 2)
diabetes_2017 <- read_csv("DiabetesAtlas_CountyData_2017.csv", skip = 2)
diabetes_2020 <- read_csv("DiabetesAtlas_CountyData_2020.csv", skip = 2)
income <- read_csv("economics_24.csv")
