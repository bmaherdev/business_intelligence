### Install packages (if not already installed)
install.packages("tidyverse")

### Load libraries
library(tidyverse)

### Get working directory
getwd()

#### - - - - - - - - - - Import land area dataset - - - - - - - - - - -
land_area <- read_csv("land_area.csv") %>%
  rename(SqMiles = `SqMiles`) %>%  
  mutate(County = str_to_upper(County)) %>%  
  select(County, SqMiles)

print(land_area)

#### - - - - - - - - - - Import obesity dataset - - - - - - - - - - -
obesity <- read_csv("obesity.csv") %>%
  rename(ObesityCases = `Number`) %>%  
  mutate(County = str_to_upper(County))  

print(obesity)

#### - - - - - - - - - - Import physical inactivity dataset - - - - - - - - - - -
physical_inactivity <- read_csv("physical_inactivity.csv") %>%
  rename(PhysicallyInactiveCases = `Number`) %>%  
  mutate(County = str_to_upper(County))  

print(physical_inactivity)

#### - - - - - - - - - - Import diabetes datasets - - - - - - - - - - -
diabetes_2013 <- read_csv("DiabetesAtlas_CountyData_2013.csv", skip = 2)
diabetes_2015 <- read_csv("DiabetesAtlas_CountyData_2015.csv", skip = 2)
diabetes_2017 <- read_csv("DiabetesAtlas_CountyData_2017.csv", skip = 2)
diabetes_2020 <- read_csv("DiabetesAtlas_CountyData_2020.csv", skip = 2)

diabetes_2013 <- diabetes_2013 %>% mutate(Year = 2013)
diabetes_2015 <- diabetes_2015 %>% mutate(Year = 2015)
diabetes_2017 <- diabetes_2017 %>% mutate(Year = 2017)
diabetes_2020 <- diabetes_2020 %>% mutate(Year = 2020)

colnames(diabetes_2013) <- colnames(diabetes_2015) <- colnames(diabetes_2017) <- colnames(diabetes_2020)

clean_county <- function(df) {
  df %>%
    mutate(County = str_replace(County, " County$", "") %>% str_to_upper())
}

diabetes_2013 <- clean_county(diabetes_2013)
diabetes_2015 <- clean_county(diabetes_2015)
diabetes_2017 <- clean_county(diabetes_2017)
diabetes_2020 <- clean_county(diabetes_2020)

diabetes_merged <- bind_rows(diabetes_2013, diabetes_2015, diabetes_2017, diabetes_2020) %>%
  arrange(County, Year)

print(diabetes_merged)

#### - - - - - - - - - - Import income dataset - - - - - - - - - - -
income <- read_csv("economics_24.csv")

excluded_values <- c("GEORGIA", "SOUTHEAST", "UNITED STATES", "NA", 
                     "Source:", "Notes:", "Per Capita Personal Income", 
                     "The local area estimates", "Net earnings")

income_long <- income %>%
  pivot_longer(
    cols = c("2013 Per Capita Personal Income", 
             "2015 Per Capita Personal Income", 
             "2017 Per Capita Personal Income", 
             "2020 Per Capita Personal Income"),
    names_to = "Year", values_to = "Per Capita Income") %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d{4}")),
         `Per Capita Income` = parse_number(`Per Capita Income`)) %>%
  filter(!County %in% excluded_values & !is.na(County) & !is.na(`Per Capita Income`)) %>%
  select(County, Year, `Per Capita Income`) %>%
  arrange(County, Year)

print(income_long)

#### - - - - - - - - - - Import physician datasets - - - - - - - - - - -
physician_2013 <- read_csv("2013_Rate_and_Rank.csv")
physician_2015 <- read_csv("2015_Rate_and_Rank.csv")
physician_2017 <- read_csv("2017_Rate_and_Rank.csv")
physician_2020 <- read_csv("2020_Rate_and_Rank.csv")

physician_merged <- bind_rows(physician_2013, physician_2015, physician_2017, physician_2020) %>%
  rename(County = `County Name`) %>%
  mutate(County = str_to_upper(County)) %>%
  arrange(County, Year)

print(physician_merged)

#### - - - - - - - - - - Join Tibbles - - - - - - - - - - -
final_data <- list(diabetes_merged, income_long, physician_merged, obesity, physical_inactivity) %>%
  reduce(full_join, by = c("County", "Year")) %>%
  full_join(land_area, by = "County") %>%
  arrange(County, Year) %>%
  select(
    County, Year, DiabetesCases = Number, 
    PerCapitaIncome = `Per Capita Income`, Population, 
    TotalPhysicians = `Total Physicians`, 
    PhysicianRatePer100000 = `Rate Per 100000`,
    SqMiles,  # Include land area column
    ObesityCases,  # Include obesity data
    PhysicallyInactiveCases  # Include physical inactivity data
  ) %>%
  # Add Population Density column
  mutate(PopulationDensity = Population / SqMiles)

# Print the final cleaned dataset
print(final_data)

# Export final_data to CSV
write_csv(final_data, "final_data.csv")