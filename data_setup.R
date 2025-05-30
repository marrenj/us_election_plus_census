## Marren Jenkins
## Stat 301-2 Data Science 2 with R
## Final Project
## Data Setup


# loading packages
library(tidyverse)
#library(tidymodels)
library(jsonlite)
#library(httr)
library(tidycensus)


# handle conflicts
#tidymodels_prefer()


# set seed
set.seed(3012)


# save your personal census API Key
# census_api_key(key = "YOUR CENSUS API KEY", install = TRUE)

census_api_key(key = "45801f604c07c5c468f5d3419372ea42ea119f54",
               install = TRUE)


##-----------------------------------------------------------------------------------------------------------------------##

## COVID DATA ####

# Covid Act Now API Key:
# 113ef48c7f7b4bb6b02c3f6e5e13fd45

# # read in the covid data 
# covid_dta <- GET("https://api.covidactnow.org/v2/counties.json?apiKey=113ef48c7f7b4bb6b02c3f6e5e13fd45")
# 
# # transform covid data into a usable format
# covid_dta <- fromJSON(rawToChar(covid_dta$content))
# 
# covid_dta <- covid_dta %>% 
#   unnest(cols = c(metrics, riskLevels, actuals, annotations), names_repair = "universal") %>% 
#   mutate(vaccinations_completed_prop = vaccinationsCompletedRatio...18) %>% 
#   select(fips, country, state, county, vaccinations_completed_prop) %>% 
#   rename(GEOID = fips)
# 
# # perform a quick skim of the outcome variable (the proportion of the population with vaccinations completed)
# skimr::skim_without_charts(covid_dta$vaccinations_completed_prop)
# 
# # look at the distribution of the outcome variable
# covid_dta %>% 
#   ggplot(aes(x = vaccinations_completed_prop)) +
#   geom_histogram()

# The distribution of the outcome variable is unimodal. However, it is somewhat skewed. Because of this, I would like to use 
# stratified sampling when splitting the data.


##-----------------------------------------------------------------------------------------------------------------------##


## CENSUS DATA ####

subject_var_names_s <- load_variables(year = 2023, dataset = "acs5/subject", cache = TRUE)

subject_var_names_dp <- load_variables(year = 2023, dataset = "acs5/profile", cache = TRUE)

acs_dta_s <- get_acs(
  # we are getting data at the county level
  geography = "county", 
  # we query each variable by its variable ID, which we found by hand in the `subject_var_names` data set
  variables = c(median_age = "S0101_C01_032E",
                pct_age_over_65 = "S0101_C02_030E",
                pct_age_over_85 = "S0101_C02_019E",
                med_household_income = "S2503_C01_013E",
                sex_ratio = "S0101_C01_033E",
                total_population = "S0102_C01_054E",
                total_foreign_born = "S0102_C01_056E",
                pct_other_language = "S1601_C02_003E"
  #               in_housing = "S0701_C01_053",
  #               pop_over_1 = "S0701_C01_001",
  #               num_households = "S1101_C01_001",
  #               avg_house_size = "S1101_C01_002",
  #               pct_housholds_speaking_spanish = "S1602_C02_002",
  #               pct_less_than_grade_9 = "S1501_C02_007",
  #               pct_hs_diploma = "S1501_C02_009",
  #               pct_bachelors = "S1501_C02_012",
  #               pct_graduate_degree = "S1501_C02_013",
  #               pct_english_only = "S1601_C02_002",
  #               pct_other_english = "S1601_C02_003",
  #               pct_below_poverty_last12mos = "S1702_C02_001",
  #               pct_disabled = "S1810_C03_001",
  #               pct_veterans = "S2101_C04_001",
  #               pct_over_150000 = "S2503_C02_012",
  #               pct_insured = "S2701_C03_001",
  #               pct_public_ins = "S2704_C03_001",
  #               pct_uninsured = "S2701_C05_001",
  #               pct_wo_internet = "S2801_C02_019"
  ),
  # this is the ACS from 2023
  year = 2023, 
  geometry = FALSE,
  survey = "acs5",
  output = "wide"
  ) 

# here we need to `pivot_wider` in order to turn rows into columns in our data set
acs_dta_s <- acs_dta_s %>% 
  pivot_wider(names_from = variable, values_from = estimate)

skimr::skim_without_charts(acs_dta_s)

acs_dta_p <- get_acs(
  geography = "county", 
  variables = c(pct_white_only = "DP05_0037PE",
                pct_black_or_afam = "DP05_0070PE",
                pct_hispanic_or_latino = "DP05_0076PE",
                pct_high_school = "DP02_0062PE",
                pct_bachelors = "DP02_0065PE",
                pct_grad_degree = "DP02_0066PE"
                # pop_total = "DP05_0001",
                # med_age = "DP05_0018",
                # pct_males = "DP05_0002P",
                # pct_over_65 = "DP05_0029P",
                # pct_white = "DP05_0037P",
                # pct_black_afam = "DP05_0065P",
                # pct_hispanic = "DP05_0071P"
    ),
  year = 2019, 
  geometry = FALSE) %>% 
  select(-c(moe))

# here we need to `pivot_wider` in order to turn rows into columns in our data set
acs_dta_p <- acs_dta_p %>% 
  pivot_wider(names_from = variable, values_from = estimate)

skimr::skim_without_charts(acs_dta_p)

acs_dta <- acs_dta_s %>% 
  inner_join(acs_dta_p, by = c("GEOID", "NAME")) %>% 
  mutate(pct_in_housing = in_housing / pop_over_1) %>% 
  select(-c(in_housing, pop_over_1))


##-----------------------------------------------------------------------------------------------------------------------##


## 2016 ELECTION DATA ####

#election_dta <- election_dta %>% 
#  filter(GEOID %in% data$GEOID)

election_dta <- read_csv("data/countypres_2000_2020.csv") %>% 
  filter(year == 2020) %>% 
  select(-c(party, mode)) %>% 
  drop_na(county_fips) %>% 
  pivot_wider(names_from = "candidate", values_from = "candidatevotes", values_fn = mean) %>% 
  rename(trump = "DONALD J TRUMP",
         GEOID = county_fips) %>% 
  # mutate(trump = as.numeric(trump))
  mutate(percent_trump = trump/totalvotes) %>% 
  select(GEOID, percent_trump) 


##-----------------------------------------------------------------------------------------------------------------------##


## JOINING OUR DATASETS ####

# join acs and covid data
data <- covid_dta %>% 
  inner_join(acs_dta, by = "GEOID") %>% 
  select(-c("NAME", "country"))

data <- election_dta %>% 
  right_join(data, by = "GEOID")  %>% 
  drop_na(vaccinations_completed_prop)



##----------------------------------------------------------------------------------------##


# SPLITTING THE DATA ####

data_split <- initial_split(data, prop = .70, strata = vaccinations_completed_prop)

data_train <- data_split %>% training()

data_test <- data_split %>% testing()

eda_dta <- sample_frac(data_train, size = .5)


##----------------------------------------------------------------------------------------##


## RESAMPLING ####

# Set 10-fold cross-validation with 5 repeats
data_folds <- 
  data_train %>% 
  vfold_cv(v = 10, repeats = 5, strata = vaccinations_completed_prop)

# Set re-sampling options
keep_pred <- control_grid(save_pred = TRUE, save_workflow= TRUE)


##----------------------------------------------------------------------------------------##


## SETTING UP A RECIPE ####

data_recipe <- recipe(vaccinations_completed_prop ~ ., data_train) %>% 
  step_rm(pct_insured, num_households, GEOID, state, county, pct_other_english) %>% 
  step_impute_linear(percent_trump, med_income, pct_in_housing,
                     impute_with = imp_vars(all_predictors(), -c(percent_trump, med_income, pct_in_housing))) %>%
  step_normalize(all_numeric_predictors()) 
#prep() %>% 
#bake(new_data = NULL)


##----------------------------------------------------------------------------------------##


## SAVING NECESSARY OBJECTS ####


save(data, data_split, data_folds, keep_pred, data_train, data_test, data_recipe, file = "data/data_setup.Rdata")


##----------------------------------------------------------------------------------------##


## EDA

# perform a quick skim for missingness
data %>% 
  skimr::skim_without_charts()






