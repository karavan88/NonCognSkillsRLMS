# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS - QUANTILE REGRESSION ANALYSIS (WITH IPW)
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         03.2_regression_returns_ipw.R
# Purpose:      Quantile regression analysis with Inverse Probability Weighting
# 
# Description:  This script estimates quantile regression models to analyze
#               heterogeneous returns to non-cognitive skills across the wage
#               distribution. Uses linear mixed quantile models (LQMM) to account
#               for individual heterogeneity while examining differential effects
#               at various quantiles (10%, 25%, 50%, 75%, 90%).
#               THIS VERSION INCLUDES ONLY IPW-WEIGHTED MODELS.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 16-29 years (2016-2019)
# Methodology:  Linear Quantile Mixed Models (LQMM) with random intercepts
#               Inverse Probability Weighting for causal inference
#
# Models:       M1_IPW - IPW-weighted baseline quantile regression
#               M2_IPW - IPW-weighted models with education levels
#               M4_IPW - IPW-weighted models by education stratification  
#               M6_IPW - IPW-weighted models by gender
#               M7_IPW - IPW-weighted gender interaction models
#               M5_IPW - IPW-weighted lifecycle analysis models
#
# Key Variables: log_wage (dependent variable)
#               O, C, E, A, ES (Big Five personality traits)
#               exp_imp (work experience)
#               ipw (inverse probability weights)
#               Controls: area, sex, marital_status, region
#
# Author:       Garen Avanesian  
# Institution:  Southern Federal University
# Created:      October 19, 2025
# Modified:     October 19, 2025
# Version:      1.0 (IPW regression models)
#
# Dependencies: lqmm, tidyverse, broom
# Runtime:      ~12-15 minutes (IPW quantile models are computationally intensive)
#
# Notes:        Quantile regression allows analysis of heterogeneous effects
#               LQMM accounts for individual-level random effects
#               IPW addresses selection bias and provides causal inference
#               Results saved as CSV files for output generation
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("📊 RETURNS TO NCS - QUANTILE REGRESSION ANALYSIS (WITH IPW)\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 03.2_regression_returns_ipw.R\n")
cat("🎯 Purpose: IPW-weighted quantile regression analysis of NCS returns\n")
cat("📈 Processing: Youth returns data → IPW-weighted LQMM models → Coefficient exports\n\n")

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

# SECTION 3: IPW VALIDATION
cat("⚖️  SECTION 3: IPW WEIGHTS VALIDATION\n")
cat("Validating inverse probability weights...\n")
ipw_start <- Sys.time()

# Check IPW weight distribution
cat("   - IPW weight summary:\n")
print(summary(youth_master_returns$ipw_empl))
cat("   - IPW weight range:", round(min(youth_master_returns$ipw_empl, na.rm = TRUE), 3), "to", round(max(youth_master_returns$ipw_empl, na.rm = TRUE), 3), "\n")
cat("   - Missing IPW weights:", sum(is.na(youth_master_returns$ipw_empl)), "\n")

ipw_end <- Sys.time()
cat("✅ IPW validation completed in", round(difftime(ipw_end, ipw_start, units = "secs"), 2), "seconds\n\n")

# SECTION 4: MODEL SPECIFICATION AND IPW BASELINE QUANTILE REGRESSION
cat("📊 SECTION 4: IPW-WEIGHTED BASELINE QUANTILE REGRESSION MODEL (M1_IPW)\n")
cat("Setting up general formula and fitting IPW-weighted LQMM across quantiles...\n")
cat("Quantiles: 10%, 25%, 50%, 75%, 90%\n")
model_start <- Sys.time()

# Define general regression formula
general_formula <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + sex  + marital_status + 
  region + O + C + E + A + ES 

cat("   - Formula: log_wage ~ exp_imp + I(exp_imp^2) + area + sex + marital_status + region + NCS\n")
cat("   - NCS variables: O, C, E, A, ES\n")
cat("   - Random effects: individual-level random intercepts\n")
cat("   - IPW weighting: Inverse probability weights applied\n")
cat("   - Fitting model across 5 quantiles...\n")


### Set seed for reproducibility
set.seed(12345)

# Fit IPW-weighted baseline quantile regression model
m1_ipw <- lqmm(
  general_formula,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw_empl,
  control = list(method = "df")
)

model_end <- Sys.time()
cat("✅ IPW baseline quantile regression (M1_IPW) completed in", round(difftime(model_end, model_start, units = "mins"), 2), "minutes\n")

# Generate model summary
cat("   - Generating IPW model summary...\n")
m1_ipw_summary <- summary(m1_ipw)
cat("✅ M1_IPW model summary generated\n\n")

# SECTION 5: M1_IPW COEFFICIENT EXTRACTION AND EXPORT
cat("💾 SECTION 5: M1_IPW COEFFICIENT EXTRACTION AND EXPORT\n")
cat("Extracting IPW model fit statistics and coefficients...\n")
coef_start <- Sys.time()

# Extract model fit statistics
m1_ipw_aic <- m1_ipw_summary$aic
m1_ipw_loglik <- m1_ipw_summary$logLik
cat("   - IPW AIC:", round(m1_ipw_aic, 2), "\n")
cat("   - IPW Log-likelihood:", round(m1_ipw_loglik, 2), "\n")

# Extract and process coefficients
m1_ipw_coefs <-
  m1_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%  # Remove region effects for cleaner output
  mutate_if(is.numeric, round, 3)

cat("   - IPW coefficients extracted for", nrow(m1_ipw_coefs), "variables\n")
cat("   - Regional effects excluded from export\n")

# Define column names for quantile results
new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m1_ipw_coefs) <- new_names

# Export coefficients to CSV
write_csv(m1_ipw_coefs, file.path(outputsReturnsNcs, "m1_ipw_coefs.csv"))

coef_end <- Sys.time()
cat("✅ M1_IPW coefficients exported in", round(difftime(coef_end, coef_start, units = "secs"), 2), "seconds\n")
cat("   - File: m1_ipw_coefs.csv\n")
cat("   - Contains IPW-weighted estimates, standard errors, confidence intervals, p-values\n\n")

# SECTION 6: IPW EDUCATION-EXTENDED MODELS (M2_IPW)
cat("🎓 SECTION 6: IPW EDUCATION-EXTENDED QUANTILE MODELS\n")
cat("Fitting IPW-weighted models with explicit education level controls...\n")

# Define education-extended formula
formula_edu <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2)  + sex + area + region + marital_status + 
  O + C + E + A + ES

cat("   - Extended formula includes explicit education levels\n")
cat("   - Education levels: No school, Secondary, Secondary Vocational, Tertiary\n")
cat("   - IPW weights applied for causal inference\n")

# Fit M2_IPW (education-extended with IPW)
cat("   - Fitting M2_IPW (education-extended IPW model)...\n")
m2_ipw_start <- Sys.time()

m2_ipw <- lqmm(
  formula_edu,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw_empl,
  control = list(method = "df")
)

m2_ipw_end <- Sys.time()
cat("✅ M2_IPW model completed in", round(difftime(m2_ipw_end, m2_ipw_start, units = "mins"), 2), "minutes\n")

# Extract model summaries and coefficients
cat("   - Extracting M2_IPW coefficients...\n")
coef_extract_start <- Sys.time()

m2_ipw_summary <- summary(m2_ipw)

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
cat("✅ M2_IPW coefficients extracted and exported in", round(difftime(coef_extract_end, coef_extract_start, units = "secs"), 2), "seconds\n")
cat("   - Files: m2_ipw_coefs.csv\n")
cat("   - Education effects explicitly modeled with IPW weighting\n\n")

# SECTION 7: IPW GENDER-STRATIFIED QUANTILE MODELS (M6_IPW)
cat("👫 SECTION 7: IPW GENDER-STRATIFIED QUANTILE MODELS\n")
cat("Fitting separate IPW-weighted models for males and females...\n")

# Define gender-stratified formula (excluding sex as it's stratified)
formula_sex <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + marital_status + 
  region + O + C + E + A + ES 

cat("   - Formula excludes sex variable (stratified by gender)\n")
cat("   - Sample sizes by gender:\n")
gender_table <- table(youth_master_returns$sex)
print(gender_table)

# Fit female model (WITH IPW)
cat("   - Fitting IPW model for females...\n")
female_start <- Sys.time()

m6_female_ipw <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Female", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw_empl[youth_master_returns$sex == "Female"],
       control = list(method = "df")
  )

female_end <- Sys.time()
cat("✅ Female IPW model completed in", round(difftime(female_end, female_start, units = "mins"), 2), "minutes\n")

# Extract female model results
cat("   - Extracting female IPW model coefficients...\n")
m6_female_ipw_summary <- summary(m6_female_ipw)

m6_female_ipw_coefs <-
  m6_female_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

# Reshape female results to long format for analysis
m6_female_ipw_long <- 
  m6_female_ipw_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>%
  mutate(sex = "Female")

cat("✅ Female IPW coefficients extracted and reshaped\n")

# Fit male model (WITH IPW)
cat("   - Fitting IPW model for males...\n")
male_start <- Sys.time()

m6_male_ipw <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Male", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw_empl[youth_master_returns$sex == "Male"],
       control = list(method = "df")
  )

male_end <- Sys.time()
cat("✅ Male IPW model completed in", round(difftime(male_end, male_start, units = "mins"), 2), "minutes\n")

# Extract male model results
cat("   - Extracting male IPW model coefficients...\n")
m6_male_ipw_summary <- summary(m6_male_ipw)

m6_male_ipw_coefs <-
  m6_male_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

# Reshape the dataset to long format
m6_male_ipw_long <- 
  m6_male_ipw_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>% 
  mutate(sex = "Male")

# bind data
m6_ipw_coefs <- 
  bind_rows(m6_female_ipw_long, m6_male_ipw_long)

new_names_sex_model <- c("variable", "quantile",  "estimate", "std.error", "ci.low", "ci.upp", "p.value", "sex")
names(m6_ipw_coefs) <- new_names_sex_model

m6_ipw_coefs$quantile = as.numeric(m6_ipw_coefs$quantile) * 100
m6_ipw_coefs$quantile = paste0("Q", m6_ipw_coefs$quantile)

# choose the ncs only
m6_ncs_ipw <- 
  m6_ipw_coefs %>%
  filter(str_detect(variable, "O|C|E|A|ES")) %>%
  filter(!str_detect(variable, "area")) %>%
  filter(!str_detect(variable, "marital")) %>%
  select(variable, sex, quantile, estimate, std.error, ci.low, ci.upp, p.value
  )

write_csv(m6_ncs_ipw, file.path(outputsReturnsNcs, "m6_ncs_ipw_coefs.csv"))
cat("✅ Gender-stratified IPW NCS coefficients exported to m6_ncs_ipw_coefs.csv\n\n")

# SECTION 8: IPW GENDER INTERACTION MODEL (M7_IPW)
cat("👫 SECTION 8: IPW GENDER INTERACTION MODEL\n")
cat("Fitting IPW-weighted model with gender interactions for NCS variables...\n")

formula_gend_int <- 
  log_wage ~  exp_imp + I(exp_imp^2)  + area + sex + marital_status + region + 
  O + C + E + A + ES  +
  O*sex + C*sex + E*sex + A*sex + ES*sex 

# Fit gender interaction model (WITH IPW)
m7_ipw <- lqmm(formula_gend_int,
               data = youth_master_returns,
               random = ~1,
               group = idind,
               tau = c(0.5, 0.75, 0.9),
               weights = youth_master_returns$ipw_empl,
               control = list(method = "df")
)

# extract the coefficients
m7_ipw_summary <-
  summary(m7_ipw)

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

write_csv(m7_ipw_coefs, file.path(outputsReturnsNcs, "m7_ipw_sex_ncs_int.csv"))
cat("✅ Gender interaction IPW coefficients exported to m7_ipw_sex_ncs_int.csv\n\n")

# SECTION 9: IPW EDUCATION-STRATIFIED MODELS (M4_IPW)
cat("🎓 SECTION 9: IPW EDUCATION-STRATIFIED QUANTILE MODELS\n")
cat("Fitting separate IPW-weighted models by education level...\n")

# Tertiary education model (WITH IPW)
cat("   - Fitting IPW tertiary education model...\n")
m4_tert_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw_empl[youth_master_returns$edu_lvl == "4. Tertiary"],
  control = list(method = "df")
)

# Secondary vocational model (WITH IPW)
cat("   - Fitting IPW secondary vocational model...\n")
m4_voc_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw_empl[youth_master_returns$edu_lvl == "3. Secondary Vocational"],
  control = list(method = "df")
)

# Secondary or below model (WITH IPW)
cat("   - Fitting IPW secondary or below model...\n")
m4_sec_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw_empl[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School")],
  control = list(method = "df")
)

# Extract coefficients for all IPW education models
m4_tert_summary_ipw <- summary(m4_tert_ipw)
m4_voc_summary_ipw <- summary(m4_voc_ipw)
m4_sec_summary_ipw <- summary(m4_sec_ipw)

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

# Merge IPW education stratified results
m4_coefs_ipw <-
  bind_rows(m4_tert_coefs_ipw,
            m4_voc_coefs_ipw,
            m4_sec_coefs_ipw) %>%
  rownames_to_column(var = "variable")

write_csv(m4_coefs_ipw, file.path(outputsReturnsNcs, "m4_coefs_edu_ipw.csv"))
cat("✅ Education-stratified IPW coefficients exported to m4_coefs_edu_ipw.csv\n\n")

# SECTION 10: IPW LIFECYCLE ANALYSIS (MEDIAN QUANTILE BY AGE GROUPS)
cat("👶👩👴 SECTION 10: IPW LIFECYCLE ANALYSIS\n")
cat("Fitting IPW-weighted median quantile models by age groups...\n")
cat("Age groups: 16-65 (full sample), 30-39, 40-49, 50-65\n")
lifecycle_start <- Sys.time()

# Full sample (16-65) lifecycle model (WITH IPW)
cat("   - Fitting IPW full sample model (16-65 years)...\n")
m_lc_ipw <- lqmm(
  general_formula,
  data = ind_master_returns,
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl_empl,
  control = list(method = "df")
)

m_lc_ipw_summary <- summary(m_lc_ipw)

m_lc_ipw_coefs <-
  m_lc_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "16-65")

# Age group 30-39 (WITH IPW)
m_lc_3039_ipw <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
  control = list(method = "df")
)

m_lc_3039_ipw_summary <- summary(m_lc_3039_ipw)

m_lc_3039_ipw_coefs <-
  m_lc_3039_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "30-40")

# Age group 40-49 (WITH IPW)
m_lc_4049_ipw <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
  control = list(method = "df")
)

m_lc_4049_ipw_summary <- summary(m_lc_4049_ipw)

m_lc_4049_ipw_coefs <-
  m_lc_4049_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "40-50")

# Age group 50-65 (WITH IPW)  
m_lc_5065_ipw <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl_empl[ind_master_returns$age >= 50],
  control = list(method = "df")
)

m_lc_5065_ipw_summary <- summary(m_lc_5065_ipw)

m_lc_5065_ipw_coefs <-
  m_lc_5065_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "50-65")

# Combine IPW lifecycle results
m_lc_ipw_combined <- 
  bind_rows(m_lc_ipw_coefs, m_lc_3039_ipw_coefs, m_lc_4049_ipw_coefs, m_lc_5065_ipw_coefs) 

write_csv(m_lc_ipw_combined, file.path(outputsReturnsNcs, "m_lc_ipw_coefs.csv"))

lifecycle_end <- Sys.time()
cat("✅ IPW lifecycle analysis completed in", round(difftime(lifecycle_end, lifecycle_start, units = "mins"), 2), "minutes\n")
cat("   - File: m_lc_ipw_coefs.csv\n")
cat("   - Age groups analyzed: 16-65, 30-39, 40-49, 50-65\n")
cat("   - Focus: IPW-weighted median quantile (0.50) for lifecycle perspective\n\n")

# SECTION 11: SUPPLEMENTARY IPW GAM ANALYSIS
cat("📈 SECTION 11: SUPPLEMENTARY IPW GAM ANALYSIS\n")
cat("Fitting IPW-weighted GAM model for non-parametric age effects...\n")
gam_start <- Sys.time()

library(mgcv)

# Note: GAM with weights for IPW adjustment
gam_ipw <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status, 
               data = ind_master_returns, 
               weights = ind_master_returns$ipw_empl)
age_gam_ipw <- plot(gam_ipw, se = TRUE, col = "blue")

gam_end <- Sys.time()
cat("✅ IPW GAM analysis completed in", round(difftime(gam_end, gam_start, units = "secs"), 2), "seconds\n")
cat("   - IPW-weighted smooth age function estimated\n")
cat("   - IPW age effects plot generated\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS QUANTILE REGRESSION ANALYSIS (WITH IPW) COMPLETED!\n")
cat(rep("=", 80), "\n")
cat("📊 MODELS FITTED:\n")
cat("   • M1_IPW: IPW baseline quantile regression (5 quantiles)\n")
cat("   • M2_IPW: IPW education-extended quantile regression\n")
cat("   • M6_IPW: IPW gender-stratified models (Male & Female)\n")
cat("   • M7_IPW: IPW gender interaction model (3 quantiles)\n")
cat("   • M4_IPW: IPW education-stratified models (Tertiary, Vocational, Secondary)\n")
cat("   • Lifecycle_IPW: IPW age-group analysis (4 age groups)\n")
cat("   • GAM_IPW: IPW non-parametric age effects\n\n")
cat("📁 FILES EXPORTED:\n")
cat("   • m1_ipw_coefs.csv (IPW baseline coefficients)\n")
cat("   • m2_ipw_coefs.csv (IPW education-extended coefficients)\n")
cat("   • m6_ncs_ipw_coefs.csv (IPW gender-stratified NCS coefficients)\n")
cat("   • m7_ipw_sex_ncs_int.csv (IPW gender interaction coefficients)\n")
cat("   • m4_coefs_edu_ipw.csv (IPW education-stratified coefficients)\n")
cat("   • m_lc_ipw_coefs.csv (IPW lifecycle analysis coefficients)\n\n")
cat("🔍 ANALYSIS FEATURES:\n")
cat("   • Quantile regression across 5 quantiles (10%, 25%, 50%, 75%, 90%)\n")
cat("   • Linear Quantile Mixed Models (LQMM) with random intercepts\n")
cat("   • Inverse Probability Weighting for causal inference\n")
cat("   • Selection bias correction through IPW methodology\n")
cat("   • Gender and education heterogeneity analysis\n")
cat("   • Lifecycle perspective across age groups\n")
cat("   • Big Five personality traits as key regressors\n\n")
cat("🎯 KEY VARIABLES:\n")
cat("   • Dependent: log_wage (natural log of hourly wage)\n")
cat("   • NCS: O, C, E, A, ES (Big Five traits)\n")
cat("   • Controls: experience, area, sex, marital_status, region\n")
cat("   • Education: explicit education levels in extended models\n")
cat("   • Weights: IPW for youth sample, ipw_empl for full sample\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Compare results with 03.1_regression_returns.R (non-IPW)\n")
cat("   • Assess robustness of findings across IPW and non-IPW specifications\n")
cat("   • Run 04_outputs_returns.R to generate publication tables and plots\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")