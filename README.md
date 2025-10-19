# Non-Cognitive Skills and Labor Market Outcomes

[![R](https://img.shields.io/badge/R-4.4.1+-blue.svg)](https://www.r-project.org/)
[![renv](https://img.shields.io/badge/renv-1.0.11+-green.svg)](https://rstudio.github.io/renv/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## 📊 Project Overview

This repository contains the complete analytical framework for studying the relationship between non-cognitive skills (Big Five personality traits) and labor market outcomes among Russian youth, using data from the Russia Longitudinal Monitoring Survey (RLMS-HSE).

## 🎯 Research Domains

- **Employment Transitions**: Analysis of successful employment transitions among youth aged 15-29
- **Returns to NCS**: Wage premium analysis for non-cognitive skills
- **Job Satisfaction**: Workplace outcome analysis
- **NEET Analysis**: Not in Education, Employment, or Training status

## 🚀 Quick Start

### Prerequisites

- R version 4.4.1 or higher
- RStudio (recommended)
- Git

### 1. Clone the Repository

```bash
git clone https://github.com/karavan88/NonCognSkillsRLMS.git
cd NonCognSkillsRLMS
```

### 2. Set Up Reproducible Environment

**Option A: Automatic Setup (Recommended)**
```r
# Open R in the project directory
source("setup_renv.R")
```

**Option B: Manual Setup**
```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restore the project environment
renv::restore()
```

### 3. Ready to Analyze! 

That's it! The environment automatically configures itself:
- ✅ **renv activates** (package environment)
- ✅ **Directories configured** (project paths)  
- ✅ **Packages loaded** (all analysis tools ready)

```r
# Everything loads automatically when you start R!
# No need to run additional setup commands

# Optional: verify everything works
source("test_environment.R")
```

### 4. Run Analysis

```r
# Run specific analysis (example: employment analysis)
source("02_codes/01_EmplNCS/03_regression_empl.R")
```

## 📁 Project Structure

```
NonCognSkillsRLMS/
├── 📊 01_input_data/           # Raw and processed datasets
│   └── processed/              # Cleaned and prepared data
├── 💻 02_codes/                # Analysis scripts
│   ├── 00_data_prep/          # Data preparation scripts
│   ├── 01_EmplNCS/            # Employment analysis
│   ├── 02_ReturnsNCS/         # Returns analysis
│   ├── 03_JobSatisfNCS/       # Job satisfaction analysis
│   └── 04_NEET-NCS/           # NEET analysis
├── 📖 03_manuscripts/          # Academic papers and reports
├── 🔧 renv/                   # Package environment (auto-generated)
├── 📋 renv.lock               # Package lockfile
├── ⚙️ user_profile.R          # Project configuration
├── 📦 setup_renv.R            # Environment setup script
└── 📚 README.md               # This file
```

## 🔄 Reproducibility Features

### Package Management with renv

This project uses [`renv`](https://rstudio.github.io/renv/) to ensure reproducible package environments:

- **Isolated**: Project-specific package library
- **Reproducible**: Exact package versions locked in `renv.lock`
- **Portable**: Works across different machines and operating systems

### Key Files for Reproducibility

- `renv.lock`: Lockfile containing exact package versions
- `project_config.R`: Project paths and complete environment setup
- `setup_renv.R`: Automated environment setup script

## 📊 Analysis Scripts

### Data Preparation
- `02_codes/00_data_prep/01_data_perp_hh.R` - Household data preparation
- `02_codes/00_data_prep/02_data_perp_ncs.R` - Non-cognitive skills data
- `02_codes/00_data_prep/03_data_prep_ind.R` - Individual data preparation

### Employment Analysis
- `02_codes/01_EmplNCS/01_data_prep_empl.R` - Employment data preparation
- `02_codes/01_EmplNCS/02_descr_empl.R` - Descriptive analysis
- `02_codes/01_EmplNCS/03_regression_empl.R` - Main regression analysis
- `02_codes/01_EmplNCS/03_regression_final.R` - Publication tables

### Other Analyses
- Returns analysis: `02_codes/02_ReturnsNCS/`
- Job satisfaction: `02_codes/03_JobSatisfNCS/`
- NEET analysis: `02_codes/04_NEET-NCS/`

## 🔬 Methodology

### Statistical Approach
- **Mixed-effects models** with random intercepts and slopes
- **Multilevel modeling** accounting for individual, regional, and temporal effects
- **Big Five personality traits** as non-cognitive skill measures
- **Longitudinal panel data** analysis (2016-2023)

### Sample
- **Population**: Russian youth aged 15-29
- **Data source**: RLMS-HSE (Russia Longitudinal Monitoring Survey)
- **Time period**: 2016-2023
- **Sample size**: ~16,000 observations

## 📦 Key Dependencies

### Core Packages
- `tidyverse` (2.0.0+) - Data manipulation and visualization
- `lme4` - Mixed-effects models
- `ggeffects` - Marginal effects and predictions

### Statistical Analysis
- `easystats` - Comprehensive statistical toolkit
- `broom` / `broom.mixed` - Model tidying
- `modelsummary` - Publication-ready tables

### Visualization
- `ggplot2` - Graphics
- `sjPlot` - Model visualization
- `tinytable` - Publication tables

## 🔧 System Requirements

### Minimum Requirements
- **R**: 4.4.1+
- **RAM**: 8GB (16GB recommended for large datasets)
- **Storage**: 2GB free space
- **OS**: Windows 10+, macOS 10.15+, or Linux

### Recommended Setup
- **IDE**: RStudio 2023.12.0+
- **Git**: Latest version for version control
- **Processors**: Multi-core processor for faster model fitting

## 🤝 Contributing

### For Collaborators

1. **Clone the repository**
2. **Set up environment**: Run `source("setup_renv.R")` or `renv::restore()`
3. **Start working**: Open R in project directory - everything loads automatically!
4. **Create feature branch**: `git checkout -b feature/your-feature`
5. **Make changes and commit**
6. **Update lockfile**: Run `renv::snapshot()` if you add packages
7. **Submit pull request**

### No Configuration Needed! 🎉

The project now auto-detects its location - no manual path setup required!
Works on any machine, any user, any operating system.

## 📈 Performance Notes

### Model Fitting Times
- Simple models (M1-M3): ~30 seconds each
- Complex random slopes models (M4-M6): ~2-5 minutes each
- Full analysis pipeline: ~10-15 minutes

### Memory Usage
- Peak memory usage: ~4-6GB during large model fitting
- Recommended: 16GB RAM for smooth operation

## 🐛 Troubleshooting

### Common Issues

**Package Installation Fails**
```r
# Clean renv cache and reinstall
renv::purge()
source("setup_renv.R")
```

**Path Issues**
```r
# Check and update user profile
source("user_profile.R")
# Verify all paths exist
```

**Model Convergence Issues**
- Increase iterations: Add `control = lmerControl(optCtrl = list(maxfun = 2e5))`
- Check data quality and missing values
- Consider model simplification

### Getting Help

1. Check the [Issues](https://github.com/karavan88/NonCognSkillsRLMS/issues) page
2. Review script documentation and logging output
3. Contact the project maintainer

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📚 Citation

If you use this code or findings in your research, please cite:

```bibtex
@misc{avanesian2025ncs,
  title={Non-Cognitive Skills and Labor Market Outcomes: Evidence from Russian Youth},
  author={Avanesian, Garen},
  year={2025},
  url={https://github.com/karavan88/NonCognSkillsRLMS}
}
```

## 👨‍🔬 Author

**Garen Avanesian**
- Institution: [Your Institution]
- Email: [Your Email]
- GitHub: [@karavan88](https://github.com/karavan88)

## 🔄 Version History

- **v2.1** (2025-10-19): Added renv for reproducibility, enhanced logging
- **v2.0** (2024-12-17): Complete rewrite with professional documentation
- **v1.0** (2023-10-21): Initial project setup

---

<div align="center">

**🎓 Research • 📊 Analytics • 🔬 Reproducible Science**

*Making labor market research reproducible and accessible*

</div>