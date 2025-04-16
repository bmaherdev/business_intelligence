## - - - - - - - - - -Uncomment to install any needed packages - - - - - - - - - -
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("stargazer")
#install.packages("ranger")
#install.packages("broom")

## - - - - - - - - - - - Load required libraries - - - - - - - - - -
library(tidyverse)
library(caret)
library(stargazer)
library(ranger)
library(broom)

## - - - - - - - - - - Check working directory - - - - - - - - - - 
getwd()

## - - - - - - - - - - Import land area dataset - - - - - - - - - - -
land_area <- read_csv("land_area.csv") %>%
  rename(SqMiles = `SqMiles`) %>%  
  mutate(County = str_to_upper(County)) %>%  
  select(County, SqMiles)
print(land_area)

## - - - - - - - - - - Import obesity dataset - - - - - - - - - - -
obesity <- read_csv("obesity.csv") %>%
  rename(ObesityCases = `Number`) %>%  
  mutate(County = str_to_upper(County))  
print(obesity)

## - - - - - - - - - - Import physical inactivity dataset - - - - - - - - - - -
physical_inactivity <- read_csv("physical_inactivity.csv") %>%
  rename(PhysicallyInactiveCases = `Number`) %>%  
  mutate(County = str_to_upper(County))  
print(physical_inactivity)

## - - - - - - - - - - Import diabetes datasets - - - - - - - - - - -
diabetes_2013 <- read_csv("DiabetesAtlas_CountyData_2013.csv", skip = 2)
diabetes_2015 <- read_csv("DiabetesAtlas_CountyData_2015.csv", skip = 2)
diabetes_2017 <- read_csv("DiabetesAtlas_CountyData_2017.csv", skip = 2)
diabetes_2020 <- read_csv("DiabetesAtlas_CountyData_2020.csv", skip = 2)

## - - - - - - - - - - Create tibbles for each year - - - - - - - - - - 
diabetes_2013 <- diabetes_2013 %>% mutate(Year = 2013)
diabetes_2015 <- diabetes_2015 %>% mutate(Year = 2015)
diabetes_2017 <- diabetes_2017 %>% mutate(Year = 2017)
diabetes_2020 <- diabetes_2020 %>% mutate(Year = 2020)

## - - - - - - - - - -  Set uniform column names - - - - - - - - - - 
colnames(diabetes_2013) <- colnames(diabetes_2015) <- colnames(diabetes_2017) <- colnames(diabetes_2020)

## - - - - - - - - - - Standardize county column - - - - - - - - - - 
clean_county <- function(df) {
  df %>%
    mutate(County = str_replace(County, " County$", "") %>% str_to_upper())
}

diabetes_2013 <- clean_county(diabetes_2013)
diabetes_2015 <- clean_county(diabetes_2015)
diabetes_2017 <- clean_county(diabetes_2017)
diabetes_2020 <- clean_county(diabetes_2020)

## - - - - - - - - - - Merge the data by county and year - - - - - - - - - - 
diabetes_merged <- bind_rows(diabetes_2013, diabetes_2015, diabetes_2017, diabetes_2020) %>%
  arrange(County, Year)
print(diabetes_merged)

## - - - - - - - - - - Import income dataset - - - - - - - - - - -
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

## - - - - - - - - - - Import physician datasets - - - - - - - - - - -
physician_2013 <- read_csv("2013_Rate_and_Rank.csv")
physician_2015 <- read_csv("2015_Rate_and_Rank.csv")
physician_2017 <- read_csv("2017_Rate_and_Rank.csv")
physician_2020 <- read_csv("2020_Rate_and_Rank.csv")

physician_merged <- bind_rows(physician_2013, physician_2015, physician_2017, physician_2020) %>%
  rename(County = `County Name`) %>%
  mutate(County = str_to_upper(County)) %>%
  arrange(County, Year)
print(physician_merged)

## - - - - - - - - - - Join tibbles - - - - - - - - - - -
unfinal_data <- list(diabetes_merged, income_long, physician_merged, obesity, physical_inactivity) %>%
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

## - - - - - - - - - -  Turn variables into rates - - - - - - - - - - 
unfinal_data <- unfinal_data %>%
  mutate(DiabetesRate = DiabetesCases / Population, PhysicianDensity = TotalPhysicians / Population, ObesityRate = ObesityCases / Population, InactivityRate = PhysicallyInactiveCases / Population)

## - - - - - - - - - -  Create table with variables we are interested in - - - - - - - - - - 
final_data <- unfinal_data %>%
  select(County, Year, DiabetesRate, PerCapitaIncome, PhysicianDensity, ObesityRate, InactivityRate, PopulationDensity)
print(final_data)

## - - - - - - - - - -  Export final_data to CSV - - - - - - - - - - 
write_csv(final_data, "final_data.csv")

## - - - - - - - - - -  Transform final_data tibble into dataframe - - - - - - - - - - 
final.data <- as.data.frame(final_data)
final.data

## - - - - - - - - - -  Create descriptive statistics file using stargazer - - - - - - - - - - 
stargazer(final.data[3 : 8], type = "html", title="Descriptive Statistics", digits = 2, out = "descriptiveTable.html")

## - - - - - - - - - - Create correlations matrix - - - - - - - - - - 
correlations <- cor(final.data[3 : 8])
stargazer(correlations, title = "Correlation Matrix", out = "CorrelationMatrix.html")

## - - - - - - - - - - Assign categories for years - - - - - - - - - - 
final_data  <- final_data  %>% 
  mutate(YearN = case_when(
    Year == "2013" ~ 0,
    Year == "2015" ~ 1,
    Year == "2017" ~ 2,
    Year == "2020" ~ 3
  ))

## - - - - - - - - - - Select variables for model and drop NAs- - - - - - - - - - 
final_data <- final_data %>%
  select (DiabetesRate, PerCapitaIncome, PhysicianDensity, ObesityRate, InactivityRate, PopulationDensity, YearN)
finalData <- drop_na(final_data)

## - - - - - - - - - - Explanatory/inference model - - - - - - - - - - 
lm(DiabetesRate ~ PerCapitaIncome + PhysicianDensity + ObesityRate + InactivityRate + YearN, data = finalData) %>%
tidy()

### - - - - - - - - - - ML Model - - - - - - - - - - 

## - - - - - - - - - - Set Seed - - - - - - - - - - 
set.seed(15L)

## - - - - - - - - - - Set train index, selecting 70% of the data - - - - - - - - - - 
trainIndex <- createDataPartition( finalData$DiabetesRate, p= 0.7, list = FALSE, times = 1)
trainIndex

## - - - - - - - - - - Create train/test split - - - - - - - - - - 
train_set <- finalData[trainIndex, ]
test_set <- finalData[-trainIndex, ]


## - - - - - - - - - - Transform tibble into dataframe to use with Stargazer - - - - - - - - - - 
train_set_df<- as.data.frame(train_set)
train_set_df

##  - - - - - - - - - - Create a descriptive statistics text file for the training data - - - - - - - - - - 
stargazer(train_set_df, type = "text", title="Descriptive Statistics", digits = 1, out = "descriptiveStatistics.txt")

## - - - - - - - - - - Calculate correlations - - - - - - - - - - 
cor(train_set)

## - - - - - - - - - - Build the model using the test set and linear regression - - - - - - - - - - 
## I selected 3 independent variables HB41_CurrentSmoker, HO49_Diabetes, HO33_OverwtAdult
model <- train(DiabetesRate ~ ObesityRate + InactivityRate + YearN, data = train_set, method = "lm")

## - - - - - - - - - - Display the model - - - - - - - - - - 
summary(model)

## - - - - - - - - - - Evaluate the model performance - - - - - - - - - - 
## - - - - - - - - - - Predict the outcomes in the test set - - - - - - - - - - 
p <- predict(model, test_set) 

## - - - - - - - - - - Calculate RMSE and R2 for the prediction - - - - - - - - - - 
postResample(pred = p, test_set$DiabetesRate)

## - - - - - - - - - - Run the random forest model with ranger - - - - - - - - - - 
model2 <- train(DiabetesRate ~ ObesityRate + InactivityRate + YearN, data = train_set, method = "ranger")
model2

## - - - - - - - - - - Predict the outcomes in the test set - - - - - - - - - - 
p1 <- predict(model2, test_set) 

## - - - - - - - - - - Evaluate model performance using RMSE and R2 - - - - - - - - - - 
postResample(pred = p1, test_set$DiabetesRate)

## - - - - - - - - - - Create plot for Actual vs Predicted Diabetes Rate (Linear Model) - - - - - - - - - -
test_set$predicted_diabetes <- predict(model2, test_set)

ggplot(test_set, aes(x = predicted_diabetes, y = DiabetesRate)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Actual vs Predicted Diabetes Rate (Linear Model)",
       x = "Predicted Diabetes Rate",
       y = "Actual Diabetes Rate") +
  theme_minimal() +
  theme(panel.grid = element_blank())