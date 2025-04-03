# Step 1: Data Processing for NHANES Analysis
# ==========================================

# Setup and Library Loading
# ------------------------
rm(list = objects())
library(haven)
library(dplyr)
library(tidyr)

# Data Import and Initial Processing
# --------------------------------
data.dir <- paste0("/nhanes2009/")

# Function to read and process datasets
process_dataset <- function(filename, filter_conditions = NULL) {
  data <- read_xpt(paste0(data.dir, filename))
  if (!is.null(filter_conditions)) {
    data <- filter(data, !!!filter_conditions)
  }
  return(data)
}

# Process each dataset independently with specific conditions
datasets <- list(
  diabetes = process_dataset("DIQ_F.xpt", 
                           list(quo(DIQ010 != 1))),
  
  glucose = process_dataset("GLU_F.xpt",
                          list(quo(LBXGLU < 126))),
  
  a1c = process_dataset("GHB_F.xpt",
                       list(quo(LBXGH < 6.5))),
  
  per = process_dataset("OHXPER_F.xpt",
                       list(quo(OHDPDSTS == 1))),
  
  oral_glucose = process_dataset("OGTT_F.xpt",
                               list(quo(PHAFSTHR >= 9),
                                    quo(GTDCODE == 0),
                                    quo(LBXGLT < 200))),
  
  demographics = process_dataset("DEMO.xpt"),
  
  physical = process_dataset("PAQ_F.xpt"),
  
  smoking = process_dataset("SMQ_F.xpt"),
  
  diet = process_dataset("DR1TOT_F.xpt",
                        list(quo(!is.na(DR1TKCAL)))),
  
  bmi = process_dataset("BMX_F.xpt",
                       list(quo(!is.na(BMXBMI)))),
  
  blood_pressure = process_dataset("BPX_F.xpt",
                                 list(quo(!is.na(BPXSY1) | !is.na(BPXSY2) | 
                                        !is.na(BPXSY3) | !is.na(BPXSY4)),
                                      quo(!is.na(BPXDI1) | !is.na(BPXDI2) | 
                                         !is.na(BPXDI3) | !is.na(BPXDI4)))),
  
  hdl = process_dataset("HDL_F.xpt",
                       list(quo(!is.na(LBDHDD)))),
  

  
  blood_count = process_dataset("CBC_F.xpt",
                              list(quo(!is.na(LBXWBCSI)))),
  ldl = process_dataset("TRIGLY_F.xpt",
                        list(quo(!is.na(LBDLDL)))),
  crp = process_dataset("CRP_F.xpt")
)

# Get sequence numbers from each dataset
seqn_lists <- lapply(datasets, function(x) x$SEQN)

# Find common participants across all datasets
keep_no <- Reduce(intersect, seqn_lists)

# Process demographics
demographics <- datasets$demographics |> 
  filter(SEQN %in% keep_no) |> 
  mutate(
    DMDEDUC2 = replace(DMDEDUC2, DMDEDUC2 %in% c(7,9), NA),
    DMDEDUC2 = if_else(DMDEDUC2 == 2, 1, DMDEDUC2),
    RIDRETH1 = if_else(RIDRETH1 == 2, 1, RIDRETH1)
  )

# Extract and check demographic variables
demographic_vars <- list(
  gender = demographics$RIAGENDR,
  age = demographics$DMDHRAGE,
  educ2 = demographics$DMDEDUC2,
  race = demographics$RIDRETH1,
  poverty = demographics$INDFMPIR
)

# Update keep_no based on complete demographic data
complete_demographics <- !is.na(demographic_vars$gender) & 
                        !is.na(demographic_vars$age) & 
                        !is.na(demographic_vars$educ2) & 
                        !is.na(demographic_vars$race)
dem_list <- demographics[complete_demographics,]$SEQN


# Probing Depths
per <- datasets$per |> 
  filter(SEQN %in% keep_no)
probing_depths <- per %>%
  dplyr::select(starts_with("OHX")) |> 
  mutate(across(everything(), ~replace(., . == 99, NA)))


pd_interproximal_sites <- grepl("OHX\\d{2}PC[DSPA]", names(probing_depths))
loss_attatch_sites <- grepl("OHX\\d{2}LA[DSPA]", names(probing_depths))
# LA interproximal LAD -
# mostly empty
all_empty <- apply(probing_depths[loss_attatch_sites | pd_interproximal_sites], 
                   1, \(x) mean(is.na(x))) == 1
probing_depths <- probing_depths[!all_empty,]
per_list <- per[!all_empty,]$SEQN



keep_no <- Reduce(intersect,list(per_list, keep_no, dem_list))
# Update all datasets in datasets list to filter keep_no
datasets <- lapply(datasets, function(data) {
  filter(data, SEQN %in% keep_no)
})

probing_depths <- datasets$per |> dplyr::select(starts_with("OHX")) |> 
  mutate(across(everything(), ~replace(., . == 99, NA)))



# Extract variables from filtered datasets
physical_data <- datasets$physical
vigorous <- physical_data$PAQ605 == 1 | physical_data$PAQ650 == 1
moderate <- physical_data$PAQ620 == 1 | physical_data$PAQ665 == 1
activity <- 1*moderate + 1*vigorous

#SMOKING dataset

never <- datasets$smoking$SMQ020== 2
former_smoker <-  datasets$smoking$SMQ020 == 1 & (datasets$smoking$SMQ040 ==3 | is.na(datasets$smoking$SMQ040))
current_smoker <- datasets$smoking$SMQ040 != 3
current_smoker[is.na(current_smoker)] <- FALSE
smoking <- factor(former_smoker*1 + current_smoker*2)

# DIET dataset
kcal <- datasets$diet$DR1TKCAL

#Alcohol
alc_grams <- datasets$diet$DR1TALCO

# BMI 
bmi <- datasets$bmi$BMXBMI

#Blood Pressure
syst <- rowMeans(datasets$blood_pressure[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)
dyst <- rowMeans(datasets$blood_pressure[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)

# HDL
HDL <- datasets$hdl$LBDHDD

#LDL
LDL <- datasets$ldl$LBDLDL

# blood cell
wbc <- datasets$blood_count$LBXWBCSI

# CRP
crp <- datasets$crp$LBXCRP

# Oral Glucose Y
Y_oral_gluc <- as.numeric(datasets$oral_glucose$LBXGLT >= 140 & datasets$oral_glucose$LBXGLT < 200)

#IFG Y
Y_ifg <- as.numeric(datasets$glucose$LBXGLU >= 100 & datasets$glucose$LBXGLU < 126)

# DEMOGRAPHICS
## Gender
demographic_vars <- list(
  gender =  datasets$demographics$RIAGENDR,
  age =     datasets$demographics$DMDHRAGE,
  educ2 =   datasets$demographics$DMDEDUC2,
  race =    datasets$demographics$RIDRETH1,
  poverty = datasets$demographics$INDFMPIR
)



# calculate periodontal disease 
source("calculate_periodontal_disease.R")
periodontal_disease <- calculate_periodontal_disease(probing_depths)

covariates <- cbind(demographic_vars$age, demographic_vars$gender, 
                    demographic_vars$race, demographic_vars$educ2, 
                    smoking,kcal,alc_grams,activity, bmi)
x <- as.factor(periodontal_disease)


full_data <- tibble(data.frame(cbind(x, covariates, Y_ifg, Y_oral_gluc)))
full_data <- full_data |> 
  mutate(Y_only_ifg = 1*(Y_ifg & !Y_oral_gluc),
         Y_only_gluc = 1*(!Y_ifg & Y_oral_gluc))
avg_depths <- apply(probing_depths, 1, \(x) mean(x, na.rm = T))



colnames(full_data) <- c("periodontal_disease", "age", "gender", 
                         "race", "education", "smoking", "kcal", "alcohol",
                         "activity", "bmi", "ifg", "oral_gluc", "ifg_0", "oral_gluc0")

full_data <- full_data |> 
  mutate(SEQN = keep_no)
full_data <- cbind(avg_depths, full_data)

full_data <- cbind(datasets$glucose$LBXGLU, full_data)
full_data <- cbind(datasets$oral_glucose$LBXGLT, full_data)
colnames(full_data) <- c("ifg*", "oral_gluc*", colnames(full_data)[3:ncol(full_data)])
# Merge survey weights

survey_weights <- demographics |> 
  dplyr::select(SEQN, WTINT2YR, WTMEC2YR, SDMVPSU,SDMVSTRA )

full_data <- full_data |> 
  left_join(survey_weights, by = "SEQN")

save(full_data, file = "step1data.rda")

