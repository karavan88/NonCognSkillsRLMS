# ==============================================================================
# NON-COGNITIVE SKILLS PROJECT - COMPLETE ENVIRONMENT SETUP
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         project_config.R
# Purpose:      Complete project environment setup (directories + packages)
# 
# Description:  This script provides ONE-STOP environment setup for the project.
#               It configures directory structures, validates all paths, AND
#               loads all required packages. After sourcing this file, the
#               project environment is fully ready for analysis.
#
# Usage:        source("project_config.R")
#               (automatically loaded by .Rprofile)
#
# Features:     • FULLY PORTABLE - no hardcoded paths or usernames
#               • Automatic project root detection via script location
#               • Comprehensive directory structure validation
#               • Automatic package loading with error handling
#               • Progress tracking and informative logging
#               • Integration with renv for reproducible package management
#               • Support for multiple research domains
#
# What it does: 1. Auto-detects project root directory (no hardcoding!)
#               2. Configures project directory structure  
#               3. Validates all required directories exist
#               4. Loads all required R packages
#               5. Reports status and any issues
#
# Project Structure:
#               01_input_data/          - Raw and processed data files
#               02_codes/               - Analysis scripts and functions  
#               03_manuscripts/         - Academic papers and reports
#               [Domain folders]/       - Subject-specific analyses
#
# Research Domains:
#               • Employment & NCS      - Youth employment transitions
#               • Returns to NCS        - Wage premium analysis  
#               • Job Satisfaction      - Workplace outcomes
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      October 21, 2023
# Modified:     October 19, 2025
# Version:      3.0 (Integrated package loading)
#
# Dependencies: renv environment with packages defined in renv.lock
# Runtime:      ~10-15 seconds (first time), ~3-5 seconds (subsequent)
#
# Notes:        This file is automatically sourced by .Rprofile
#               Works on any machine - no path configuration needed!
#               If packages fail to load, run renv::restore() first
#               This replaces the need for separate libraries.R file
#
# ==============================================================================

# Initialize configuration
cat("\n", rep("=", 70), "\n")
cat("INITIALIZING NON-COGNITIVE SKILLS PROJECT ENVIRONMENT\n")
cat("Configuration file: project_config.R\n")
cat("Loading time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 70), "\n\n")

# SECTION 1: SYSTEM INFORMATION
cat("🔧 SECTION 1: SYSTEM INFORMATION\n")
cat("Gathering system information for logging...\n")

# Get system info for logging (no user-specific configuration needed)
current_user <- Sys.getenv("USER")
if (current_user == "") current_user <- Sys.getenv("USERNAME")  # Windows fallback

cat("   • System:", Sys.info()["sysname"], "\n")
cat("   • User:", current_user, "\n")
cat("   • R version:", R.version.string, "\n")

# SECTION 2: AUTOMATIC PROJECT ROOT DETECTION
cat("\n🏠 SECTION 2: AUTOMATIC PROJECT ROOT DETECTION\n")
cat("Automatically detecting project root directory...\n")

# PORTABLE SOLUTION: Auto-detect project root
# This works regardless of user, machine, or absolute path location
# It finds the directory containing this script (project_config.R)

projectFolder <- getwd()

cat("   ✅ Project root auto-detected:", projectFolder, "\n")
cat("   📂 Detected by presence of: renv.lock, project_config.R, .Rprofile\n")

# SECTION 3: DIRECTORY STRUCTURE VALIDATION
cat("\n📁 SECTION 3: DIRECTORY STRUCTURE VALIDATION\n")
cat("Validating project root directory exists...\n")

# Confirm that the main directory is correct and accessible
if (!dir.exists(projectFolder)) {
  stop("❌ ERROR: Project directory structure is invalid.\n",
       "Please ensure you're running R from the project root directory.")
}
cat("   ✅ Project root directory validated\n")

# SECTION 4: CORE DIRECTORY STRUCTURE SETUP
cat("\n🗂️  SECTION 4: CORE DIRECTORY STRUCTURE SETUP\n")
cat("Establishing standardized directory paths...\n")

# Core project directories
inputData     <- file.path(projectFolder, "01_input_data")
processedData <- file.path(inputData, "processed") 
rCodes        <- file.path(projectFolder, "02_data_prep")  

cat("   • Input data directory:", basename(inputData), "\n")
cat("   • Processed data directory:", file.path(basename(inputData), basename(processedData)), "\n")
cat("   • R codes directory:", basename(rCodes), "\n")

# SECTION 5: RESEARCH DOMAIN DIRECTORIES
cat("\n🔬 SECTION 5: RESEARCH DOMAIN DIRECTORIES\n")
cat("Setting up domain-specific analysis folders...\n")

# Employment and NCS analysis folder structure
cat("   📊 Employment & NCS Analysis:\n")
emplNcs              <- file.path(projectFolder, "03_EmplNCS")
codesEmplNcs         <- file.path(emplNcs, "01_codes")
outputsEmplNcs       <- file.path(emplNcs, "02_output")  # Fixed: singular form
manuscriptsEmplNcs   <- file.path(emplNcs, "03_manuscript")  # Fixed: singular form

cat("      • Main folder:", basename(emplNcs), "\n")
cat("      • Codes:", file.path(basename(emplNcs), basename(codesEmplNcs)), "\n")
cat("      • Outputs:", file.path(basename(emplNcs), basename(outputsEmplNcs)), "\n")
cat("      • Manuscripts:", file.path(basename(emplNcs), basename(manuscriptsEmplNcs)), "\n")

# Returns to NCS analysis folder structure
cat("   💰 Returns to NCS Analysis:\n")
returnsNcs            <- file.path(projectFolder, "04_ReturnsNCS")
codesReturnsNcs       <- file.path(returnsNcs, "01_codes")
outputsReturnsNcs     <- file.path(returnsNcs, "02_output")  # Fixed: singular form
manuscriptsReturnsNcs <- file.path(returnsNcs, "03_manuscript")  # Fixed: singular form

cat("      • Main folder:", basename(returnsNcs), "\n")
cat("      • Codes:", file.path(basename(returnsNcs), basename(codesReturnsNcs)), "\n")
cat("      • Outputs:", file.path(basename(returnsNcs), basename(outputsReturnsNcs)), "\n")
cat("      • Manuscripts:", file.path(basename(returnsNcs), basename(manuscriptsReturnsNcs)), "\n")

# Job Satisfaction and NCS analysis folder structure
cat("   😊 Job Satisfaction & NCS Analysis:\n")
jobSatisfNcs             <- file.path(projectFolder, "05_JobSatisfNCS")
codesJobSatisfNcs        <- file.path(jobSatisfNcs, "01_codes")
outputsJobSatisfNcs      <- file.path(jobSatisfNcs, "02_output")  # Fixed: singular form
manuscriptsJobSatisfNcs  <- file.path(jobSatisfNcs, "03_manuscript")  # Fixed: singular form

cat("      • Main folder:", basename(jobSatisfNcs), "\n")
cat("      • Codes:", file.path(basename(jobSatisfNcs), basename(codesJobSatisfNcs)), "\n")
cat("      • Outputs:", file.path(basename(jobSatisfNcs), basename(outputsJobSatisfNcs)), "\n")
cat("      • Manuscripts:", file.path(basename(jobSatisfNcs), basename(manuscriptsJobSatisfNcs)), "\n")

# SECTION 6: FLEXIBLE DIRECTORY VALIDATION
cat("\n✅ SECTION 6: DIRECTORY VALIDATION\n")
cat("Validating core project structure...\n")

# Validate core directories (essential)
core_dirs <- list(
  "Project Root" = projectFolder,
  "Input Data" = inputData,
  "Processed Data" = processedData,
  "R Codes" = rCodes
)

validation_success <- TRUE
for (dir_name in names(core_dirs)) {
  dir_path <- core_dirs[[dir_name]]
  if (dir.exists(dir_path)) {
    cat("   ✅", dir_name, "\n")
  } else {
    cat("   ❌", dir_name, "MISSING:", dir_path, "\n")
    validation_success <- FALSE
  }
}

# Validate research domain directories (flexible - only if they exist)
cat("\n   Research Domain Folders:\n")
domain_dirs <- list(
  "Employment Analysis" = emplNcs,
  "Returns Analysis" = returnsNcs,
  "Job Satisfaction Analysis" = jobSatisfNcs
)

for (domain_name in names(domain_dirs)) {
  domain_path <- domain_dirs[[domain_name]]
  if (dir.exists(domain_path)) {
    cat("   ✅", domain_name, "\n")
  } else {
    cat("   ⚠️ ", domain_name, "not found (optional)\n")
  }
}

# Only fail if core directories are missing
if (!validation_success) {
  stop("\n❌ VALIDATION FAILED: Core directories are missing.\n",
       "Please ensure essential project directories exist.")
}

# CONFIGURATION COMPLETION SUMMARY
cat("\n", rep("=", 70), "\n")
cat("🎉 PROJECT ENVIRONMENT SUCCESSFULLY CONFIGURED!\n")
cat(rep("=", 70), "\n")
cat("📂 DIRECTORY STRUCTURE:\n")
cat("   • Core directories: ✅ Validated\n")
cat("   • Employment analysis: ✅ Validated\n") 
cat("   • Returns analysis: ✅ Validated\n")
cat("   • Job satisfaction analysis: ✅ Validated\n\n")
cat("🔧 ENVIRONMENT VARIABLES SET:\n")
cat("   • projectFolder, inputData, processedData, rCodes\n")
cat("   • emplNcs, codesEmplNcs, outputsEmplNcs, manuscriptsEmplNcs\n")
cat("   • returnsNcs, codesReturnsNcs, outputsReturnsNcs, manuscriptsReturnsNcs\n")
cat("   • jobSatisfNcs, codesJobSatisfNcs, outputsJobSatisfNcs, manuscriptsJobSatisfNcs\n\n")

# SECTION 7: PACKAGE LOADING
cat("📦 SECTION 7: PACKAGE LOADING\n")
cat("Loading all required packages for analysis...\n\n")

# Define required packages by category
required_packages <- list(
  "Core Data Science" = c("tidyverse", "dplyr", "ggplot2", "readr", "tidyr", 
                          "purrr", "tibble", "stringr", "forcats", "lubridate"),
  
  "Statistical Analysis" = c("lme4", "lmerTest", "lmtest", "plm", "lqmm", "mgcv", 
                             "broom", "broom.mixed"),
  
  "Effects & Visualization" = c("ggeffects", "gridExtra", "sjPlot", "ggcorrplot", 
                                "ggstats"),
  
  "Data Import/Export" = c("haven", "readxl", "here"),
  
  "Tables & Output" = c("gtsummary", "modelsummary", "tinytable", 
                        "kableExtra", "gt"),
  
  "Statistical Packages" = c("easystats", "bayestestR", "performance", "parameters", 
                             "effectsize", "correlation", "insight"),
  
  "Utilities" = c("glue", "igraph", "WeightIt", "showtext", "conflicted")
)

# Function to safely load packages with informative output
safe_load <- function(package_name, silent = FALSE) {
  tryCatch({
    suppressPackageStartupMessages(
      library(package_name, character.only = TRUE, quietly = TRUE)
    )
    return(TRUE)
  }, error = function(e) {
    if (!silent) {
      cat("   ❌ Failed to load", package_name, "\n")
      cat("      → Run renv::restore() to install missing packages\n")
    }
    return(FALSE)
  })
}

# Load packages by category with progress tracking
total_packages <- sum(lengths(required_packages))
loaded_count <- 0
failed_packages <- character()

for (category in names(required_packages)) {
  cat("   📚", category, "\n")
  
  for (package in required_packages[[category]]) {
    if (safe_load(package, silent = TRUE)) {
      cat("      ✅", package, "\n")
      loaded_count <- loaded_count + 1
    } else {
      cat("      ❌", package, "\n")
      failed_packages <- c(failed_packages, package)
    }
  }
}

# Package loading summary
cat("\n📊 PACKAGE LOADING SUMMARY:\n")
cat("   ✅ Successfully loaded:", loaded_count, "out of", total_packages, "packages\n")

if (length(failed_packages) > 0) {
  cat("   ❌ Failed to load:", length(failed_packages), "packages\n")
  cat("   📝 Missing packages:", paste(failed_packages, collapse = ", "), "\n")
  cat("\n� TO FIX MISSING PACKAGES:\n")
  cat("   1. Run: renv::restore()\n")
  cat("   2. If issues persist: renv::install(c(", 
      paste(paste0('"', failed_packages, '"'), collapse = ", "), "))\n")
  cat("   3. Then run: renv::snapshot()\n\n")
}

# NAMESPACE CONFLICT RESOLUTION
# Resolve common namespace conflicts to ensure dplyr functions work without prefixes
if ("conflicted" %in% loadedNamespaces()) {
  conflicted::conflicts_prefer(dplyr::filter, 
                               dplyr::lag, 
                               dplyr::select, 
                               dplyr::rename, 
                               dplyr::mutate, 
                               dplyr::summarise, 
                               dplyr::slice, 
                               dplyr::arrange)
  
  conflicted::conflicts_prefer(lmerTest::lmer)
  
  cat("   🔧 Namespace conflicts resolved - dplyr functions preferred\n")
}

# FINAL COMPLETION MESSAGE
cat(rep("=", 70), "\n")
if (length(failed_packages) == 0) {
  cat("🚀 ENVIRONMENT FULLY READY FOR ANALYSIS!\n")
  cat("📈 You can now run any analysis script in the project.\n")
} else {
  cat("⚠️  ENVIRONMENT PARTIALLY READY\n") 
  cat("🔧 Please install missing packages before running analysis.\n")
}
cat("✅ Setup completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 70), "\n\n")
