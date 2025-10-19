# ==============================================================================
# RENV SETUP SCRIPT FOR NON-COGNITIVE SKILLS PROJECT
# ==============================================================================
#
# Purpose: Install all required packages and create reproducible environment
# Author:  Garen Avanesian
# Date:    October 19, 2025
#
# ==============================================================================

# Initialize renv if not already done
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Activate renv for this project
renv::activate()

# Define all required packages with specific versions to avoid conflicts
required_packages <- list(
  # Core tidyverse
  "tidyverse"       = "2.0.0",
  "dplyr"           = "1.1.4",
  "ggplot2"         = "3.5.1",
  "readr"           = "2.1.5",
  "tidyr"           = "1.3.1",
  "purrr"           = "1.0.2",
  "tibble"          = "3.2.1",
  "stringr"         = "1.5.1",
  "forcats"         = "1.0.1",
  "lubridate"       = "1.9.4",
  
  # Statistical analysis
  "lme4"            = NULL,  # Let renv pick compatible version
  "lmerTest"        = NULL,
  "lmtest"          = NULL,
  "plm"             = NULL,
  "lqmm"            = NULL,
  "mgcv"            = NULL,
  "broom"           = NULL,
  "broom.mixed"     = NULL,
  
  # Visualization and effects
  "ggeffects"       = "2.0.0",
  "gridExtra"       = NULL,
  "sjPlot"          = NULL,
  "ggcorrplot"      = NULL,
  "ggstats"         = NULL,
  
  # Data handling
  "haven"           = NULL,
  "readxl"          = NULL,
  "here"            = NULL,  # Project structure management
  
  # Tables and output
  "gtsummary"       = NULL,
  "modelsummary"    = NULL,
  "tinytable"       = NULL,
  "kableExtra"      = NULL,
  "gt"              = NULL,
  
  # Utility
  "glue"            = NULL,
  "igraph"          = NULL,
  "WeightIt"        = NULL,
  "surveytoolbox"   = NULL,
  "showtext"        = NULL
)

# Function to safely install packages
safe_install <- function(package_name, version = NULL) {
  cat("Installing", package_name, "...\n")
  
  tryCatch({
    if (is.null(version)) {
      renv::install(package_name)
    } else {
      renv::install(paste0(package_name, "@", version))
    }
    cat("✅", package_name, "installed successfully\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Failed to install", package_name, ":", e$message, "\n")
    
    # Special handling for packages with known system dependencies
    if (package_name %in% c("gdtools", "officer", "systemfonts", "textshaping")) {
      cat("💡 This package requires system dependencies. See troubleshooting below.\n")
    }
    
    return(FALSE)
  })
}

# Function to check and install system dependencies (macOS)
check_system_deps <- function() {
  if (Sys.info()["sysname"] == "Darwin") {  # macOS
    cat("🔧 Checking for system dependencies on macOS...\n")
    
    # Check if Homebrew is installed
    homebrew_installed <- system("which brew", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    
    if (!homebrew_installed) {
      cat("⚠️  Homebrew not found. Some packages may require system dependencies.\n")
      cat("💡 To install Homebrew: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"\n")
    } else {
      cat("✅ Homebrew found\n")
      cat("💡 If you encounter issues with gdtools, run:\n")
      cat("   brew install cairo freetype2 fribidi harfbuzz\n")
    }
  } else if (Sys.info()["sysname"] == "Linux") {
    cat("🔧 On Linux, you may need: sudo apt-get install libcairo2-dev libfreetype6-dev\n")
  }
}

# Install packages in logical order to avoid conflicts
cat("🚀 Starting package installation...\n\n")

# Check system dependencies first
check_system_deps()

# 1. Core packages first
core_packages <- c("tidyverse", "dplyr", "ggplot2", "readr", "tidyr", "purrr", 
                   "tibble", "stringr", "forcats", "lubridate", "here")

cat("📦 Installing core packages...\n")
for (pkg in core_packages) {
  if (pkg %in% names(required_packages)) {
    safe_install(pkg, required_packages[[pkg]])
  }
}

# 2. Statistical packages
stat_packages <- c("lme4", "lmerTest", "lmtest", "plm", "lqmm", "mgcv", 
                   "broom", "broom.mixed")

cat("\n📊 Installing statistical packages...\n")
for (pkg in stat_packages) {
  safe_install(pkg)
}

# 3. Visualization packages
viz_packages <- c("ggeffects", "gridExtra", "sjPlot", "ggcorrplot", "ggstats")

cat("\n📈 Installing visualization packages...\n")
for (pkg in viz_packages) {
  if (pkg == "ggeffects" && !is.null(required_packages[[pkg]])) {
    safe_install(pkg, required_packages[[pkg]])
  } else {
    safe_install(pkg)
  }
}

# 4. Data handling packages
data_packages <- c("haven", "readxl")

cat("\n📁 Installing data handling packages...\n")
for (pkg in data_packages) {
  safe_install(pkg)
}

# 5. Table and output packages (these may require gdtools)
table_packages <- c("gtsummary", "modelsummary", "tinytable", "gt")
problematic_packages <- c("kableExtra")  # Separate these due to gdtools dependency

cat("\n📋 Installing table packages...\n")
for (pkg in table_packages) {
  safe_install(pkg)
}

cat("\n⚠️  Installing packages with system dependencies...\n")
cat("💡 If these fail, install system dependencies first (see above)\n")
for (pkg in problematic_packages) {
  safe_install(pkg)
}

# 6. Utility packages
util_packages <- c("glue", "igraph", "WeightIt",  "showtext")

cat("\n🔧 Installing utility packages...\n")
for (pkg in util_packages) {
  safe_install(pkg)
}

# Install easystats carefully last
cat("\n🎯 Installing easystats package suite...\n")
easystats_packages <- c("insight", "bayestestR", "performance", "parameters", 
                        "effectsize", "correlation", "modelbased", "see", "report")

for (pkg in easystats_packages) {
  safe_install(pkg)
}

# Finally install easystats meta-package
safe_install("easystats")

# Create snapshot
cat("\n📸 Creating renv snapshot...\n")
renv::snapshot()

cat("\n🎉 Setup complete! All packages installed and lockfile created.\n")
cat("📋 To reproduce this environment on another machine:\n")
cat("   1. Clone the repository\n")
cat("   2. Open R in the project directory\n") 
cat("   3. Run: renv::restore()\n")
cat("   4. All packages will be installed automatically\n\n")
cat("🚀 Happy coding in the Non-Cognitive Skills Project!\n")