# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS TO EDUCATION - DATA PREPARATION
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         01_data_prep_returns.R
# Purpose:      Prepare individual-level data for returns to NCS analysis
# 
# Description:  This script prepares RLMS individual data for analyzing returns
#               to non-cognitive skills. Processes wage data, applies inflation
#               adjustments, calculates hourly wages, and creates analysis-ready
#               datasets for youth (16-29) and working-age (16-65) populations.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Working-age population (16-65 years, 2016-2019)
# Methodology:  CPI adjustment, hourly wage calculation, missing data handling
#
# Key Variables: wages_adj (inflation-adjusted wages)
#               hourly_wage (hourly wage rate)
#               log_wage (natural log of hourly wage)
#               working_hrs_per_month (monthly working hours)
#
# Outputs:      youth_master_returns.rds (Youth analysis dataset)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 1, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: tidyverse, project configuration
# Runtime:      ~1-2 minutes
#
# Notes:        Applies CPI adjustment from 2016 to 2019 rubles
#               Handles missing working hours with 40-hour default
#               Focuses on youth employment returns analysis
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("💰 RETURNS TO NCS - DATA PREPARATION\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 01_data_prep_returns.R\n")
cat("🎯 Purpose: Prepare wage and NCS data for returns analysis\n")
cat("📈 Processing: Individual employment data → Returns analysis dataset\n\n")

# SECTION 1: INFLATION ADJUSTMENT SETUP
cat("💹 SECTION 1: INFLATION ADJUSTMENT SETUP\n")
cat("Setting up Consumer Price Index (CPI) adjustments...\n")
cat("Source: World Bank CPI data for Russian Federation\n")
cat("URL: https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=RU\n")

# CPI values for inflation adjustment
cpi_2016 <- 162.2
cpi_2019 <- 180.8
adj_factor <- cpi_2019/cpi_2016

cat("   - CPI 2016:", cpi_2016, "\n")
cat("   - CPI 2019:", cpi_2019, "\n")
cat("   - Adjustment factor (2016→2019):", round(adj_factor, 4), "\n")
cat("   - Inflation rate:", round((adj_factor - 1) * 100, 2), "%\n")
cat("✅ Inflation adjustment parameters configured\n\n")

# SECTION 2: LOAD AND PROCESS MASTER EMPLOYMENT DATA
cat("📊 SECTION 2: LOADING MASTER EMPLOYMENT DATA\n")
cat("Loading individual employment data for returns analysis...\n")
data_load_start <- Sys.time()

ind_master_returns <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 16 & age < 66) %>%
  mutate(wages_adj = ifelse(id_w == 25, wages * adj_factor, wages)) %>%
  mutate(work_hrs_per_week = ifelse(is.na(j6_2), 40, j6_2)) %>%
  mutate(working_hrs_per_month = work_hrs_per_week * 4) %>%
  mutate(hourly_wage = wages_adj/working_hrs_per_month) %>%
  mutate(log_wage = log(hourly_wage)) %>%
  drop_na(wages, O, C, E, A, ES)

data_load_end <- Sys.time()
cat("✅ Data processing completed in", round(difftime(data_load_end, data_load_start, units = "secs"), 2), "seconds\n")
cat("   - Dataset dimensions:", nrow(ind_master_returns), "rows x", ncol(ind_master_returns), "columns\n")
cat("   - Age range:", min(ind_master_returns$age, na.rm = TRUE), "to", max(ind_master_returns$age, na.rm = TRUE), "years\n")
cat("   - Unique individuals:", length(unique(ind_master_returns$idind)), "\n")
cat("   - Observations with wage data:", sum(!is.na(ind_master_returns$wages_adj)), "\n")
cat("   - Mean hourly wage (2019 RUB):", round(mean(ind_master_returns$hourly_wage, na.rm = TRUE), 2), "\n")
cat("   - Median hourly wage (2019 RUB):", round(median(ind_master_returns$hourly_wage, na.rm = TRUE), 2), "\n\n")

# SECTION 3: CREATE YOUTH SUBSAMPLE
cat("👥 SECTION 3: CREATING YOUTH SUBSAMPLE\n")
cat("Filtering data for youth population (16-29 years)...\n")
youth_filter_start <- Sys.time()

youth_master_returns <-
  ind_master_returns %>%
  filter(age >= 16 & age < 30) 

youth_filter_end <- Sys.time()
cat("✅ Youth filtering completed in", round(difftime(youth_filter_end, youth_filter_start, units = "secs"), 2), "seconds\n")
cat("   - Youth dataset dimensions:", nrow(youth_master_returns), "rows x", ncol(youth_master_returns), "columns\n")
cat("   - Youth age range:", min(youth_master_returns$age, na.rm = TRUE), "to", max(youth_master_returns$age, na.rm = TRUE), "years\n")
cat("   - Unique youth individuals:", length(unique(youth_master_returns$idind)), "\n")
cat("   - Youth with wage data:", sum(!is.na(youth_master_returns$wages_adj)), "\n")
cat("   - Youth mean hourly wage (2019 RUB):", round(mean(youth_master_returns$hourly_wage, na.rm = TRUE), 2), "\n\n")

# SECTION 4: SAVE PROCESSED DATASET
cat("💾 SECTION 4: SAVING PROCESSED DATASET\n")
cat("Saving youth returns analysis dataset...\n")
save_start <- Sys.time()

saveRDS(youth_master_returns, file.path(outputsReturnsNcs, "youth_master_returns.rds"))

save_end <- Sys.time()
cat("✅ Dataset saved successfully in", round(difftime(save_end, save_start, units = "secs"), 2), "seconds\n")
cat("   - File path: 02_ReturnsNCS/outputs/youth_master_returns.rds\n")
cat("   - File size:", round(file.size(file.path(rCodes, "02_ReturnsNCS/outputs/youth_master_returns.rds")) / 1024^2, 2), "MB\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS DATA PREPARATION COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 DATA PROCESSING SUMMARY:\n")
cat("   • Inflation adjustment: 2016 wages → 2019 rubles\n")
cat("   • Hourly wage calculation with working hours imputation\n")
cat("   • Working-age population filtering (16-65 years)\n")
cat("   • Youth subsample creation (16-29 years)\n")
cat("   • Missing data handling for wages and NCS variables\n\n")
cat("📈 FINAL DATASET CHARACTERISTICS:\n")
cat("   • Total observations:", nrow(youth_master_returns), "\n")
cat("   • Unique individuals:", length(unique(youth_master_returns$idind)), "\n")
cat("   • Variables included:", ncol(youth_master_returns), "\n")
cat("   • Years covered: 2016, 2019\n")
cat("   • Complete wage data:", sum(!is.na(youth_master_returns$wages_adj)), "observations\n\n")
cat("💰 WAGE STATISTICS (2019 RUB):\n")
cat("   • Mean hourly wage:", round(mean(youth_master_returns$hourly_wage, na.rm = TRUE), 2), "\n")
cat("   • Median hourly wage:", round(median(youth_master_returns$hourly_wage, na.rm = TRUE), 2), "\n")
cat("   • Min hourly wage:", round(min(youth_master_returns$hourly_wage, na.rm = TRUE), 2), "\n")
cat("   • Max hourly wage:", round(max(youth_master_returns$hourly_wage, na.rm = TRUE), 2), "\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Run 02_descr_returns.R for descriptive analysis\n")
cat("   • Dataset ready for returns to NCS regression analysis\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")
# END OF SCRIPT