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
         gender = as.factor(gender))
dat1 <- dat
dat1["Ystar"] <- dat["ifg*"]
dat1["Tstar"] <- dat["avg_depths"]
dat1["T"] <- dat["periodontal_disease"]
dat1["Y"] <- dat["ifg"]
dat1["O"] <- dat["education"]
what <- estimate(dat1, family = "binomial", return_what = TRUE)$what


modboot <- glm(Y ~ T + what + race + age + gender, dat1, family = binomial())
#vs
modnaive <- glm(Y ~ T + education + race + age + gender, dat1, family = binomial())



dat2 <- dat1
dat2["Y"] <- dat['oral_gluc']
dat2["Ystar"] <- dat["oral_gluc*"]

what <- estimate(dat2, family = "binomial", return_what = TRUE)$what
modboot <- glm(Y ~ T + what + race + age + gender, dat2, family = binomial())
modnaive <- glm(Y ~ T + education + race + age + gender, dat2, family = binomial())
logistic.display(modboot)
logistic.display(modnaive)


## USING SURVEY WEIGHTS
what1 <- estimate(dat1, family = "binomial", return_what = TRUE)$what
full_data1 <- cbind(dat1, full_data, what1)
nhanes_design <- svydesign(
  id = ~SDMVPSU,  # Primary sampling unit
  strata = ~SDMVSTRA,  # Stratification variable
  weights = ~WTMEC2YR,  # Use examination weight
  nest = TRUE,  # Nest PSUs within strata
  data = full_data1
)

mod1s <- svyglm(Y ~ T + what1 + race + age + gender,data = full_data1,
       family = binomial(), design = nhanes_design)

what2 <- estimate(dat2, family = "binomial", return_what = TRUE)$what
full_data2 <- cbind(dat2, full_data, what2)
nhanes_design <- svydesign(
  id = ~SDMVPSU,  # Primary sampling unit
  strata = ~SDMVSTRA,  # Stratification variable
  weights = ~WTMEC2YR,  # Use examination weight
  nest = TRUE,  # Nest PSUs within strata
  data = full_data2
)

mod2s <- svyglm(Y ~ T + what2 + race + age + gender, data = full_data2,
               family = binomial(), design = nhanes_design)
library(broom)

mod1s_odds <- tidy(mod1s, exponentiate = TRUE, conf.int = TRUE)
mod2s_odds <- tidy(mod2s, exponentiate = TRUE, conf.int = TRUE)

# Create a table for dat1 (ifg) with only T2, T3
ifg_table <- mod1s_odds %>%
  dplyr::filter(term %in% c("T2", "T3")) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

# Create a table for dat2 (oral_gluc) with only T2, T3
oral_gluc_table <- mod2s_odds %>%
  dplyr::filter(term %in% c("T2", "T3")) %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)

# Print the tables
print("ifg")
print(ifg_table)
print("oral_gluc")
print(oral_gluc_table)
