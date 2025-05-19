#-------------------------------------------------------------------
# Project: PhD Thesis
# Script: Upload Libraries
# Author: Garen Avanesian
# Date: 1 December 2024
#-------------------------------------------------------------------

required_packages <- c("tidyverse",
                       "easystats",
                       "ggeffects",
                       "gridExtra",
                       "sjPlot",
                       "haven",
                       "readxl",
                       "lme4",
                       "lmerTest",
                       "lmtest",
                       "plm",
                       "lqmm",
                       "mgcv",
                       "ggcorrplot",
                       "gtsummary",
                       "broom",
                       "broom.mixed",
                       "glue",
                       "modelsummary",
                       "flextable",
                       "igraph",
                       "WeightIt",
                       "surveytoolbox",
                       "kableExtra",
                       "ggstats",
                       "showtext",
                       "gt")

# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Call the function with the list of required packages
check_and_install_packages(required_packages)