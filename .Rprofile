# ==============================================================================
# R PROFILE FOR NON-COGNITIVE SKILLS PROJECT
# ==============================================================================
#
# This file automatically activates the renv environment and loads the
# project configuration when R is started in this directory.
#
# ==============================================================================

# Activate renv for this project
cat("🔧 Activating renv environment...\n")
source("renv/activate.R")

# Check if packages are in sync
tryCatch({
  status <- renv::status(quiet = TRUE)
  if (length(status) > 0) {
    cat("⚠️  Project packages are out of sync. Run renv::restore() to update.\n")
  } else {
    cat("✅ Project environment is up to date.\n")
  }
}, error = function(e) {
  cat("ℹ️  Run renv::restore() to set up the project environment.\n")
})

# Load project configuration
if (file.exists("project_config.R")) {
  cat("📁 Loading project configuration...\n")
  source("project_config.R")
}

# Set project-specific options
options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  warn = 1,
  scipen = 999,
  stringsAsFactors = FALSE,
  width = 120
)

cat("\n🎯 Non-Cognitive Skills Project Environment Ready!\n")
cat("� All packages loaded automatically - ready for analysis!\n\n")
