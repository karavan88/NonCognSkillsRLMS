# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS - QUANTILE REGRESSION ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         03_regression_returns.R
# Purpose:      Quantile regression analysis of returns to non-cognitive skills
# 
# Description:  This script estimates quantile regression models to analyze
#               heterogeneous returns to non-cognitive skills across the wage
#               distribution. Uses linear mixed quantile models (LQMM) to account
#               for individual heterogeneity while examining differential effects
#               at various quantiles (10%, 25%, 50%, 75%, 90%).
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 16-29 years (2016-2019)
# Methodology:  Linear Quantile Mixed Models (LQMM) with random intercepts
#
# Models:       M1 - Baseline quantile regression across all quantiles
#               M2 - Models with IPW (Inverse Probability Weighting)
#               M3 - Models by education levels
#               M4 - Models by gender
#               M5 - Lifecycle analysis models
#
# Key Variables: log_wage (dependent variable)
#               O, C, E, A, ES (Big Five personality traits)
#               exp_imp (work experience)
#               Controls: area, sex, marital_status, region
#
# Author:       Garen Avanesian  
# Institution:  Southern Federal University
# Created:      December 1, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: lqmm, tidyverse, broom
# Runtime:      ~10-15 minutes (quantile models are computationally intensive)
#
# Notes:        Quantile regression allows analysis of heterogeneous effects
#               LQMM accounts for individual-level random effects
#               Results saved as CSV files for output generation
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("📊 RETURNS TO NCS - QUANTILE REGRESSION ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 03_regression_returns.R\n")
cat("🎯 Purpose: Quantile regression analysis of NCS returns\n")
cat("📈 Processing: Youth returns data → LQMM models → Coefficient exports\n\n")

# SECTION 1: DATA PREPARATION AND FACTOR SETUP
cat("🔧 SECTION 1: DATA PREPARATION AND FACTOR SETUP\n")
cat("Preparing categorical variables and factor levels...\n")
factor_start <- Sys.time()

youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Female", "Male"))
youth_master_returns$edu_lvl <- factor(youth_master_returns$edu_lvl, levels = c("1. No school", 
                                                                "2. Secondary School",
                                                                "3. Secondary Vocational",
                                                                "4. Tertiary"))

factor_end <- Sys.time()
cat("✅ Factor preparation completed in", round(difftime(factor_end, factor_start, units = "secs"), 2), "seconds\n")
cat("   - Sex factor levels:", paste(levels(youth_master_returns$sex), collapse = ", "), "\n")
cat("   - Education factor levels:", paste(levels(youth_master_returns$edu_lvl), collapse = ", "), "\n")
cat("   - Sex distribution:\n")
print(summary(youth_master_returns$sex))
cat("   - Education distribution:\n")
print(summary(youth_master_returns$edu_lvl))
cat("\n")

# SECTION 2: AREA VARIABLE CREATION
cat("🏘️  SECTION 2: AREA VARIABLE CREATION\n")
cat("Creating urban/rural classification...\n")
area_start <- Sys.time()

# Load the youth returns dataset
youth_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "youth_master_returns.rds"))

# Generate urban/rural variable
youth_master_returns$area1 = ifelse( youth_master_returns$area %in% c("Областной центр", "Город"), "urban", "rural")
# Set factor levels with rural as base
youth_master_returns$area1 = factor(youth_master_returns$area1, levels = c("rural", "urban"))

area_end <- Sys.time()
cat("✅ Area classification completed in", round(difftime(area_end, area_start, units = "secs"), 2), "seconds\n")
cat("   - Urban areas: Областной центр, Город\n")
cat("   - Rural areas: All other areas\n")
cat("   - Area distribution:\n")
print(summary(youth_master_returns$area1))
cat("\n")

# SECTION 3: MODEL SPECIFICATION AND BASELINE QUANTILE REGRESSION
cat("📊 SECTION 3: BASELINE QUANTILE REGRESSION MODEL (M1)\n")
cat("Setting up general formula and fitting LQMM across quantiles...\n")
cat("Quantiles: 10%, 25%, 50%, 75%, 90%\n")
model_start <- Sys.time()

# Define general regression formula
general_formula <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + sex  + marital_status + 
  region + O + C + E + A + ES 

cat("   - Formula: log_wage ~ exp_imp + I(exp_imp^2) + area + sex + marital_status + region + NCS\n")
cat("   - NCS variables: O, C, E, A, ES\n")
cat("   - Random effects: individual-level random intercepts\n")
cat("   - Fitting model across 5 quantiles...\n")

# Fit baseline quantile regression model
m1 <- lqmm(
  general_formula,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

model_end <- Sys.time()
cat("✅ Baseline quantile regression (M1) completed in", round(difftime(model_end, model_start, units = "mins"), 2), "minutes\n")

# Generate model summary
cat("   - Generating model summary...\n")
m1_summary <- summary(m1)
cat("✅ M1 model summary generated\n\n")

# SECTION 4: M1 COEFFICIENT EXTRACTION AND EXPORT
cat("💾 SECTION 4: M1 COEFFICIENT EXTRACTION AND EXPORT\n")
cat("Extracting model fit statistics and coefficients...\n")
coef_start <- Sys.time()

# Extract model fit statistics
m1_aic <- m1_summary$aic
m1_loglik <- m1_summary$logLik
cat("   - AIC:", round(m1_aic, 2), "\n")
cat("   - Log-likelihood:", round(m1_loglik, 2), "\n")

# Extract and process coefficients
m1_coefs <-
  m1_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%  # Remove region effects for cleaner output
  mutate_if(is.numeric, round, 3)

cat("   - Coefficients extracted for", nrow(m1_coefs), "variables\n")
cat("   - Regional effects excluded from export\n")

# Define column names for quantile results
new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m1_coefs) <- new_names

# Export coefficients to CSV
write_csv(m1_coefs, file.path(outputsReturnsNcs, "m1_coefs.csv"))

coef_end <- Sys.time()
cat("✅ M1 coefficients exported in", round(difftime(coef_end, coef_start, units = "secs"), 2), "seconds\n")
cat("   - File: m1_coefs.csv\n")
cat("   - Contains estimates, standard errors, confidence intervals, p-values for all quantiles\n\n")

# SECTION 5: IPW-WEIGHTED BASELINE MODEL (M1_IPW)
cat("⚖️  SECTION 5: IPW-WEIGHTED BASELINE MODEL (M1_IPW)\n")
cat("Fitting baseline model with Inverse Probability Weighting...\n")
ipw_start <- Sys.time()

m1_ipw <- lqmm(
  general_formula,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw,
  control = list(method = "df")
)

ipw_end <- Sys.time()
cat("✅ IPW baseline model (M1_IPW) completed in", round(difftime(ipw_end, ipw_start, units = "mins"), 2), "minutes\n")

# Extract IPW model results
cat("   - Extracting IPW model summary and coefficients...\n")
m1_ipw_summary <- summary(m1_ipw)

m1_ipw_aic <- m1_ipw_summary$aic
m1_ipw_loglik <- m1_ipw_summary$logLik
cat("   - IPW AIC:", round(m1_ipw_aic, 2), "\n")
cat("   - IPW Log-likelihood:", round(m1_ipw_loglik, 2), "\n")

m1_ipw_coefs <-
  m1_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m1_ipw_coefs) <- new_names

# Export IPW coefficients
write_csv(m1_ipw_coefs, file.path(outputsReturnsNcs, "m1_ipw_coefs.csv"))
cat("✅ M1_IPW coefficients exported to m1_ipw_coefs.csv\n\n")


# SECTION 6: EDUCATION-EXTENDED MODELS (M2 & M2_IPW)
cat("🎓 SECTION 6: EDUCATION-EXTENDED QUANTILE MODELS\n")
cat("Fitting models with explicit education level controls...\n")

# Define education-extended formula
formula_edu <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2)  + sex + area + region + marital_status + 
  O + C + E + A + ES

cat("   - Extended formula includes explicit education levels\n")
cat("   - Education levels: No school, Secondary, Secondary Vocational, Tertiary\n")

# Fit M2 (education-extended without IPW)
cat("   - Fitting M2 (education-extended model)...\n")
m2_start <- Sys.time()

m2 <- lqmm(
  formula_edu,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

m2_end <- Sys.time()
cat("✅ M2 model completed in", round(difftime(m2_end, m2_start, units = "mins"), 2), "minutes\n")

# Fit M2_IPW (education-extended with IPW)
cat("   - Fitting M2_IPW (education-extended with IPW)...\n")
m2_ipw_start <- Sys.time()

m2_ipw <- lqmm(
  formula_edu,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw,
  control = list(method = "df")
)

m2_ipw_end <- Sys.time()
cat("✅ M2_IPW model completed in", round(difftime(m2_ipw_end, m2_ipw_start, units = "mins"), 2), "minutes\n")

# Extract model summaries and coefficients
cat("   - Extracting M2 and M2_IPW coefficients...\n")
coef_extract_start <- Sys.time()

m2_summary <- summary(m2)

m2_ipw_summary <- summary(m2_ipw)

# Extract M2 coefficients
m2_coefs <-
  m2_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m2_coefs) <- new_names
write_csv(m2_coefs, file.path(outputsReturnsNcs, "m2_coefs.csv"))

# Extract M2_IPW coefficients  
m2_ipw_coefs <-
  m2_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m2_ipw_coefs) <- new_names
write_csv(m2_ipw_coefs, file.path(outputsReturnsNcs, "m2_ipw_coefs.csv"))

coef_extract_end <- Sys.time()
cat("✅ M2 and M2_IPW coefficients extracted and exported in", round(difftime(coef_extract_end, coef_extract_start, units = "secs"), 2), "seconds\n")
cat("   - Files: m2_coefs.csv, m2_ipw_coefs.csv\n")
cat("   - Education effects now explicitly modeled\n\n")


# SECTION 7: GENDER-STRATIFIED QUANTILE MODELS (M6)
cat("👫 SECTION 7: GENDER-STRATIFIED QUANTILE MODELS\n")
cat("Fitting separate models for males and females...\n")

# Define gender-stratified formula (excluding sex as it's stratified)
formula_sex <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + marital_status + 
  region + O + C + E + A + ES 

cat("   - Formula excludes sex variable (stratified by gender)\n")
cat("   - Sample sizes by gender:\n")
gender_table <- table(youth_master_returns$sex)
print(gender_table)

# Fit female model
cat("   - Fitting model for females...\n")
female_start <- Sys.time()

m6_female <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Female", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw[youth_master_returns$sex == "Female"],
       control = list(method = "df")
  )

female_end <- Sys.time()
cat("✅ Female model completed in", round(difftime(female_end, female_start, units = "mins"), 2), "minutes\n")

# Extract female model results
cat("   - Extracting female model coefficients...\n")
m6_female_summary <- summary(m6_female)

m6_female_coefs <-
  m6_female_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

# Reshape female results to long format for analysis
m6_female_long <- 
  m6_female_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>%
  mutate(sex = "Female")

cat("✅ Female coefficients extracted and reshaped\n")

# Fit male model
cat("   - Fitting model for males...\n")
male_start <- Sys.time()

m6_male <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Male", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw[youth_master_returns$sex == "Male"],
       control = list(method = "df")
  )

male_end <- Sys.time()
cat("✅ Male model completed in", round(difftime(male_end, male_start, units = "mins"), 2), "minutes\n")

# Extract male model results
cat("   - Extracting male model coefficients...\n")
m6_male_summary <- summary(m6_male)

m6_male_coefs <-
  m6_male_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

# Reshape the dataset to long format
m6_male_long <- 
  m6_male_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>% 
  mutate(sex = "Male")

# View(m6_male_long)

# bind data
m6_coefs <- 
  bind_rows(m6_female_long, m6_male_long)

new_names_sex_model <- c("variable", "quantile",  "estimate", "std.error", "ci.low", "ci.upp", "p.value", "sex")
names(m6_coefs) <- new_names_sex_model

m6_coefs$quantile = as.numeric(m6_coefs$quantile) * 100
m6_coefs$quantile = paste0("Q", m6_coefs$quantile)

# choose the ncs only

m6_ncs <- 
  m6_coefs %>%
  filter(str_detect(variable, "O|C|E|A|ES")) %>%
  filter(!str_detect(variable, "area")) %>%
  filter(!str_detect(variable, "marital")) %>%
  select(variable, sex, quantile, estimate, std.error, ci.low, ci.upp, p.value
  )

# View(m6_ncs)

###-----------------------------------------------------------------------------
### -------------- Interaction 
###-----------------------------------------------------------------------------

# youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Male", "Female"))


formula_gend_int <- 
  log_wage ~  exp_imp + I(exp_imp^2)  + area + sex + marital_status + region + 
  O + C + E + A + ES  +
  O*sex + C*sex + E*sex + A*sex + ES*sex 

# we need to make female as a base level in sex variable
# data_merged$sex <- relevel(data_merged$sex, ref = "female")
# levels(data_merged$sex)

m7 <- lqmm(formula_gend_int,
           data = youth_master_returns,
           random = ~1,
           group = idind,
           tau = 0.50,
           control = list(method = "df")
)

m7_ipw <- lqmm(formula_gend_int,
               data = youth_master_returns,
               random = ~1,
               group = idind,
               tau = c(0.5, 0.75, 0.9),
               weights = youth_master_returns$ipw,
               control = list(method = "df")
)

# extract the coefficients
m7_summary <- 
  summary(m7)

m7_ipw_summary <-
  summary(m7_ipw)

m7_coefs <-
  m7_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

m7_ipw_coefs <-
  m7_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

new_names_upp <- c("variable", 
                   "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
                   "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
                   "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m7_ipw_coefs) <- new_names_upp

# View(m7_ipw_coefs)

write.csv(m7_ipw_coefs, file.path(outputsReturnsNcs, "m7_ipw_sex_ncs_int.csv"))



###########################################################################################
#####################   create a lqmm model  by education level        ####################
###########################################################################################

### Tertiary
m4_tert <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
  control = list(method = "df")
)

### secondary vocational
m4_voc <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)


### secondary or below
m4_sec <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

### extract the coefs

# tertiary
m4_tert_summary <- 
  summary(m4_tert)

m4_voc_summary <- 
  summary(m4_voc)

m4_sec_summary <- 
  summary(m4_sec)

m4_tert_coefs <- 
  m4_tert_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Tertiary") 

m4_voc_coefs <- 
  m4_voc_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary Vocational") 

m4_sec_coefs <- 
  m4_sec_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary or below")


# merge the data
m4_coefs <- 
  bind_rows(m4_tert_coefs, 
            m4_voc_coefs,  
            m4_sec_coefs) %>%
  rownames_to_column(var = "variable") 

# View(m4_coefs)


write_csv(m4_coefs, file.path(outputsReturnsNcs, "m4_coefs_edu.csv"))

### now do absolutely te same models but with ipw weights 
m4_tert_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl == "4. Tertiary"],
  control = list(method = "df")
)

m4_voc_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl == "3. Secondary Vocational"],
  control = list(method = "df")
)

m4_sec_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School")],
  control = list(method = "df")
)

m4_tert_summary_ipw <- 
  summary(m4_tert_ipw)

m4_voc_summary_ipw <-
  summary(m4_voc_ipw)

m4_sec_summary_ipw <-
  summary(m4_sec_ipw)

m4_tert_coefs_ipw <-
  m4_tert_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Tertiary")

m4_voc_coefs_ipw <-
  m4_voc_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary Vocational")

m4_sec_coefs_ipw <-
  m4_sec_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary or below")

m4_coefs_ipw <-
  bind_rows(m4_tert_coefs_ipw,
            m4_voc_coefs_ipw,
            m4_sec_coefs_ipw) %>%
  rownames_to_column(var = "variable")

write_csv(m4_coefs_ipw, file.path(outputsReturnsNcs, "m4_coefs_edu_ipw.csv"))


# SECTION 8: LIFECYCLE ANALYSIS (MEDIAN QUANTILE BY AGE GROUPS)
cat("👶👩👴 SECTION 8: LIFECYCLE ANALYSIS\n")
cat("Fitting median quantile models by age groups for lifecycle perspective...\n")
cat("Age groups: 16-65 (full sample), 30-39, 40-49, 50-65\n")
lifecycle_start <- Sys.time()

# Full sample (16-65) lifecycle model
cat("   - Fitting full sample model (16-65 years)...\n")
m_lc <- lqmm(
  general_formula,
  data = ind_master_returns,
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl,
  control = list(method = "df")
)

m_lc_summary <- 
  summary(m_lc)

m_lc_coefs <-
  m_lc_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "16-65")

m_lc_3039 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
  control = list(method = "df")
)

m_lc_3039_summary <- 
  summary(m_lc_3039)

m_lc_3039_coefs <-
  m_lc_3039_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "30-40")

m_lc_4049 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
  control = list(method = "df")
)

m_lc_4049_summary <- 
  summary(m_lc_4049)

m_lc_4049_coefs <-
  m_lc_4049_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "40-50")

m_lc_5065 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 50],
  control = list(method = "df")
)

m_lc_5065_summary <- 
  summary(m_lc_5065)

m_lc_5065_coefs <-
  m_lc_5065_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "50-65")

m_lc <- 
  bind_rows(m_lc_coefs, m_lc_3039_coefs, m_lc_4049_coefs, m_lc_5065_coefs) 

write_csv(m_lc, file.path(outputsReturnsNcs, "m_lc_ipw_coefs.csv"))

lifecycle_end <- Sys.time()
cat("✅ Lifecycle analysis completed in", round(difftime(lifecycle_end, lifecycle_start, units = "mins"), 2), "minutes\n")
cat("   - File: m_lc_ipw_coefs.csv\n")
cat("   - Age groups analyzed: 16-65, 30-39, 40-49, 50-65\n")
cat("   - Focus: Median quantile (0.50) for lifecycle perspective\n\n")

# SECTION 9: SUPPLEMENTARY GAM ANALYSIS
cat("📈 SECTION 9: SUPPLEMENTARY GAM ANALYSIS\n")
cat("Fitting GAM model for non-parametric age effects...\n")
gam_start <- Sys.time()

library(mgcv)

gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status, data = ind_master_returns)
age_gam <- plot(gam, se = TRUE, col = "black")

gam_end <- Sys.time()
cat("✅ GAM analysis completed in", round(difftime(gam_end, gam_start, units = "secs"), 2), "seconds\n")
cat("   - Smooth age function estimated\n")
cat("   - Age effects plot generated\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS QUANTILE REGRESSION ANALYSIS COMPLETED!\n")
cat(rep("=", 80), "\n")
cat("📊 MODELS FITTED:\n")
cat("   • M1: Baseline quantile regression (5 quantiles)\n")
cat("   • M1_IPW: Baseline with Inverse Probability Weighting\n")
cat("   • M2: Education-extended quantile regression\n")
cat("   • M2_IPW: Education-extended with IPW\n")
cat("   • M6: Gender-stratified models (Male & Female)\n")
cat("   • M4: Education-stratified models (Tertiary, Vocational, Secondary)\n")
cat("   • Lifecycle: Age-group analysis (4 age groups)\n")
cat("   • GAM: Non-parametric age effects\n\n")
cat("📁 FILES EXPORTED:\n")
cat("   • m1_coefs.csv (Baseline coefficients)\n")
cat("   • m1_ipw_coefs.csv (Baseline IPW coefficients)\n")
cat("   • m2_coefs.csv (Education-extended coefficients)\n")
cat("   • m2_ipw_coefs.csv (Education-extended IPW coefficients)\n")
cat("   • m4_coefs_edu.csv (Education-stratified coefficients)\n")
cat("   • m4_coefs_edu_ipw.csv (Education-stratified IPW coefficients)\n")
cat("   • m7_ipw_gender_ncs_int.csv (Gender interaction coefficients)\n")
cat("   • m_lc_ipw_coefs.csv (Lifecycle analysis coefficients)\n\n")
cat("🔍 ANALYSIS FEATURES:\n")
cat("   • Quantile regression across 5 quantiles (10%, 25%, 50%, 75%, 90%)\n")
cat("   • Linear Quantile Mixed Models (LQMM) with random intercepts\n")
cat("   • Inverse Probability Weighting for causal inference\n")
cat("   • Gender and education heterogeneity analysis\n")
cat("   • Lifecycle perspective across age groups\n")
cat("   • Big Five personality traits as key regressors\n\n")
cat("🎯 KEY VARIABLES:\n")
cat("   • Dependent: log_wage (natural log of hourly wage)\n")
cat("   • NCS: O, C, E, A, ES (Big Five traits)\n")
cat("   • Controls: experience, area, sex, marital_status, region\n")
cat("   • Education: explicit education levels in extended models\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Run 04_outputs_returns.R to generate publication tables and plots\n")
cat("   • All coefficient files ready for output generation\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")
