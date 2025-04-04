# step 3 modelling with ordinal confounder
rm(list = objects())
library(devtools)
library(epiDisplay)
library(survey)
load("step1data.rda")
#devtools::install_github("https://github.com/chasehmathis/ordinalconfounder")

library(ordinalconfounder)
full_data <- full_data |> 
  mutate(periodontal_disease = as.factor(periodontal_disease),
         education = as.factor(education),
         race = as.factor(race),
         gender = as.factor(gender),
         smoking = as.factor(smoking),
         activity = as.factor(activity)) %>%
  rename(T = periodontal_disease) 



## USING SURVEY WEIGHTS
nhanes_design <- svydesign(
  id = ~SDMVPSU,  # Primary sampling unit
  strata = ~SDMVSTRA,  # Stratification variable
  weights = ~WTMEC2YR,  # Use examination weight
  nest = TRUE,  # Nest PSUs within strata
  data = full_data
)
browser()



mod1s <- svyglm(ifg ~ T + education + race + age + gender + smoking + 
                  kcal + alcohol + activity + bmi,data = full_data,
       family = binomial(), design = nhanes_design)


mod2s <- svyglm(oral_gluc ~ T + education + race + age + gender + smoking + 
                  kcal + alcohol + activity + bmi, data = full_data,
               family = binomial(), design = nhanes_design)
library(broom)
browser()
mod1s_odds <- tidy(mod1s, exponentiate = TRUE, conf.int = TRUE)
mod2s_odds <- tidy(mod2s, exponentiate = TRUE, conf.int = TRUE)

# Create a table for dat1 (ifg) with only T2, T3
ifg_table <- mod1s_odds %>%
  dplyr::filter(term %in% c("T2", "T3")) %>%
  dplyr::select(term, estimate, conf.low, conf.high)

# Create a table for dat2 (oral_gluc) with only T2, T3
oral_gluc_table <- mod2s_odds %>%
  dplyr::filter(term %in% c("T2", "T3")) %>%
  dplyr::select(term, estimate, conf.low, conf.high)

# Print the tables
print(ifg_table)
print(oral_gluc_table)