#Step5 / Model 5

# step 3 modelling with ordinal confounder
rm(list = objects())
library(devtools)
library(epiDisplay)
library(survey)
load("step1data.rda")
#devtools::install_github("https://github.com/chasehmathis/ordinalconfounder")

library(ordinalconfounder)
dat <- full_data |> 
  mutate(periodontal_disease = as.factor(periodontal_disease),
         education = as.factor(education),
         race = as.factor(race),
         gender = as.factor(gender),
         smoking = as.factor(smoking),
         activity = as.factor(activity),
         bmi = as.factor(bmi))
dat1 <- dat
dat1["Ystar"] <- dat["ifg*"]
dat1["Tstar"] <- dat["avg_depths"]
dat1["T"] <- dat["periodontal_disease"]
dat1["Y"] <- dat["ifg"]
dat1["O"] <- dat["education"]


dat2 <- dat1
dat2["Y"] <- dat['oral_gluc']
dat2["Ystar"] <- dat["oral_gluc*"]

## USING SURVEY WEIGHTS
what1 <- estimate(dat1, family = "binomial", return_what = TRUE)$what

full_data1 <- data.frame(cbind(dat1$Y, dat1$T, dat$education, dat$race, dat$age, 
                               dat$gender, dat$smoking, dat$activity, dat$kcal, 
                               dat$alcohol, dat$bmi,
                               dat$SDMVPSU,
                               dat$SDMVSTRA,
                               dat$WTMEC2YR))
colnames(full_data1) <- c("Y", "T", "education", "race", "age", 
                          "gender", "smoking", "activity", "kcal", "alcohol", 
                          "bmi", "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
full_data1 <- full_data1 |> 
  mutate(T = as.factor(T),
         race = as.factor(race),
         gender = as.factor(gender),
         smoking = as.factor(smoking),
         activity = as.factor(activity),
         education = as.factor(education),
         bmi = as.factor(bmi))

nhanes_design_1 <- svydesign(
  id = ~SDMVPSU,  # Primary sampling unit
  strata = ~SDMVSTRA,  # Stratification variable
  weights = ~WTMEC2YR,  # Use examination weight
  nest = TRUE,  # Nest PSUs within strata
  data = full_data1
)

mod1s <- svyglm(Y ~ T + factor(education, ordered = TRUE) + race + age + gender,
                data = full_data1,
                family = binomial(), design = nhanes_design_1)

what2 <- estimate(dat2, family = "binomial", return_what = TRUE)$what
full_data2 <- data.frame(cbind(dat2$Y, dat2$T, dat$education, dat$race, dat$age, 
                               dat$gender, dat$smoking, dat$activity, dat$kcal, 
                               dat$alcohol, dat$bmi,
                               dat$SDMVPSU,
                               dat$SDMVSTRA,
                               dat$WTMEC2YR))
colnames(full_data2) <- c("Y", "T", "education", "race", "age", 
                          "gender", "smoking", "activity", "kcal", "alcohol", 
                          "bmi", "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
full_data2 <- full_data2 |> 
  mutate(T = as.factor(T),
         race = as.factor(race),
         gender = as.factor(gender),
         smoking = as.factor(smoking),
         activity = as.factor(activity),
         education = as.factor(education),
         bmi = as.factor(bmi))
nhanes_design_2 <- svydesign(
  id = ~SDMVPSU,  # Primary sampling unit
  strata = ~SDMVSTRA,  # Stratification variable
  weights = ~WTMEC2YR,  # Use examination weight
  nest = TRUE,  # Nest PSUs within strata
  data = full_data2
)

mod2s <- svyglm(Y ~ T + factor(education, ordered = TRUE) + race + age + gender, data = full_data2,
                family = binomial(), design = nhanes_design_2)
summary(mod1s, df = degf(nhanes_design_1))

summary(mod2s, df = degf(nhanes_design_2))
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
print("ifg")
print(ifg_table)
print("oral_gluc")
print(oral_gluc_table)