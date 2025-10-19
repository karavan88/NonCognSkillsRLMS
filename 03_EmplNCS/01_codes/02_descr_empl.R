# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT ANALYSIS - DESCRIPTIVE STATISTICS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         02_descr_empl.R
# Purpose:      Descriptive statistics and visualizations for employment analysis
# 
# Description:  This script generates comprehensive descriptive statistics,
#               summary tables, and visualizations for the employment transition
#               analysis. Creates tables showing employment patterns by demographics,
#               Big Five personality trait distributions, and publication-ready
#               visualizations for employment outcomes.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 15-29 years (Waves 25-28: 2016-2019)
# Analysis:     - Employment status descriptive tables
#               - Big Five traits summary statistics
#               - Boxplots by employment status, SES, education, sex
#               - Occupation classification tables (ISCO-08)
#               - Publication-ready visualizations
#
# Key Outputs:  - summary_stats: Employment status by year
#               - ncs_descr: Big Five traits descriptive statistics
#               - Boxplot visualizations (bp1, bp2, bp3)
#               - Density plots by demographic groups
#               - Occupation classification tables
#
# Dependencies: ind_master_empl.rds (from 01_data_prep_empl.R)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 19, 2025
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("EMPLOYMENT DESCRIPTIVE ANALYSIS - RLMS-HSE STUDY\n")
cat(rep("=", 80), "\n")
cat("Script:", "02_descr_empl.R\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

cat("📊 PHASE 1: DATA LOADING AND PREPARATION\n")
cat(rep("-", 50), "\n")

cat("Loading master employment dataset...")
start_time <- Sys.time()


cat("Loading master employment dataset...")
start_time <- Sys.time()

youth_empl <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, edu_lvl, region, sex, employed, year,
         in_education, hh_inc_quintile, area_binary, area, occupation, 
         # empl_offic, 
         employed_officially,
         self_employed, satisf_job, j1_1_1, 
         O, C, E, A, ES) %>%
  # if NA in edu_lvv that assign No School - this will fill 4 observations
  mutate(edu_lvl = ifelse(is.na(edu_lvl), "1. No school", edu_lvl),
         employed_officially = ifelse(is.na(employed_officially), 0, employed_officially)) %>%
  # fix NAs in self-employed
  mutate(self_employed = ifelse(is.na(self_employed), 0, self_employed)) %>%
  # create transition_successful variable
  # should be OFFICIALLy self-employed and satisfied with the job
  mutate(self_empl_offic_and_saisf = case_when(self_employed == 1 & j1_1_1 == 1 & employed_officially == 1 ~ 1,
                                               TRUE ~ 0),
         # exclude self-employed from those employed officially as successfully transitioned
         transition_successful1 = case_when(self_employed == 1 ~ 0,
                                            TRUE ~ employed_officially)) %>%
  # create a final transition successful variable
  mutate(transition_successful = case_when(transition_successful1 == 1 |
                                             self_empl_offic_and_saisf == 1 ~ 1,
                                           TRUE                             ~ 0)) %>%
  rename(ses5 = hh_inc_quintile) %>%
  #create age groups based on 15-29
  mutate(age_group = case_when(age >= 15 & age < 20 ~ "1. 15-20",
                               #age >= 18 & age < 20 ~ "1. 18-19",
                               age >= 20 & age < 25 ~ "2. 20-24",
                               age >= 25 & age < 30 ~ "3. 25-29")) 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Youth employment dataset prepared:\n")
cat("  • Age range: 15-29 years\n")
cat("  • Total observations:", nrow(youth_empl), "\n")
cat("  • Unique individuals:", length(unique(youth_empl$idind)), "\n")
cat("  • Variables created: transition_successful, age_group, ses5\n")
cat("  • Missing data handling: education and employment status\n\n")

cat("📋 PHASE 2: SUMMARY STATISTICS TABLES\n")
cat(rep("-", 50), "\n")

cat("Creating comprehensive employment summary table...")
start_time <- Sys.time() 


cat("Creating comprehensive employment summary table...")
start_time <- Sys.time()

summary_stats <-
  youth_empl %>%
  mutate(satisfied_with_job = case_when(j1_1_1 == 1 ~ 1, TRUE ~ 0)) %>%
  select(age, sex, year,
         employed, employed_officially, self_employed, satisfied_with_job, transition_successful,
         in_education, edu_lvl, 
         year, area, ses5) %>%
  tbl_summary(by = year, 
              type = list( age ~ 'continuous2',
                           c(employed, employed_officially, self_employed, transition_successful,
                             in_education, satisfied_with_job) ~ 'dichotomous',
                           c( ses5, area, edu_lvl) ~ 'categorical'),
              label = list(age ~ "Age",
                           employed ~ "Employed",
                           employed_officially ~ "Officially Employed",
                           self_employed ~ "Self-Employed",
                           satisfied_with_job ~ "Satisfied with Job",
                           transition_successful ~ "Transition Successful",
                           in_education ~ "Attending Education",
                           area ~ "Area",
                           sex ~ "Sex",
                           ses5 ~ "HH Income Per Cap Quintile",
                           edu_lvl ~ "Highest Level of Education") , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data")) 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Main summary statistics table created:\n")
cat("  • Variables included: employment status, demographics, education\n")
cat("  • Grouped by: survey year (2016 vs 2019)\n")
cat("  • Format: GT table with source note\n\n")

cat("Creating Russian-language data chapter table...")
start_time <- Sys.time() 

cat("Creating Russian-language data chapter table...")
start_time <- Sys.time()

summary_stats_data_chapter <-
  youth_empl %>%
  mutate(satisfied_with_job = case_when(j1_1_1 == 1 ~ 1, TRUE ~ 0)) %>%
  select(employed, employed_officially, 
         self_employed, satisfied_with_job, transition_successful,
         year) %>%
  tbl_summary(by = year, 
              type = list( # age ~ 'continuous2',
                           c(employed, employed_officially, self_employed, transition_successful,
                             satisfied_with_job) ~ 'dichotomous' #,
                          # c( ses5, area, edu_lvl) ~ 'categorical'
                           ),
              label = list(# age ~ "Age",
                           employed ~ "Трудоустроен",
                           employed_officially ~ "Формальная занятость",
                           self_employed ~ "Самозанятый",
                           satisfied_with_job ~ "Удовлетворен работой",
                           transition_successful ~ "Переход завершен" #,
                           ) , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels() 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Russian-language table created:\n")
cat("  • Employment variables in Russian\n")
cat("  • Format: FlexTable for Russian publications\n\n")

cat("📊 PHASE 3: BIG FIVE PERSONALITY TRAITS ANALYSIS\n")
cat(rep("-", 50), "\n")

cat("Creating Big Five descriptive statistics...")
start_time <- Sys.time()

ncs_descr <-
  youth_empl %>%
  select(O, C, E, A, ES) %>%
  rename(Openness = O,
         Conscientiousness = C,
         Extraversion = E,
         Agreeableness = A,
         `Emotional Stability` = ES) %>%
  datasummary_skim()

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Big Five descriptive statistics created:\n")
cat("  • Traits: Openness, Conscientiousness, Extraversion, Agreeableness, Emotional Stability\n")
cat("  • Statistics: Mean, SD, Min, Max, Histogram\n")
cat("  • Format: Datasummary table\n\n")

cat("📈 PHASE 4: BOXPLOT VISUALIZATIONS\n")
cat(rep("-", 50), "\n")

cat("Preparing data for boxplot visualizations...")
start_time <- Sys.time()



cat("Preparing data for boxplot visualizations...")
start_time <- Sys.time()

boxplot_data <-
  youth_empl %>%
  select(O, C, E, A, ES, 
         transition_successful, ses5, edu_lvl, sex) %>%
  mutate(transition_successful = ifelse(transition_successful == 1, "Employed", "Unemployed")) %>%
  pivot_longer(cols = c(O, C, E, A, ES),
               names_to = "non_cogn_skills",
               values_to = "ncs_values") %>%
  mutate(non_cogn_skills = case_when(non_cogn_skills == "O" ~ "Openness",
                                     non_cogn_skills == "C" ~ "Conscientiousness",
                                     non_cogn_skills == "E" ~ "Extraversion",
                                     non_cogn_skills == "A" ~ "Agreeableness",
                                     non_cogn_skills == "ES" ~ "Emotional Stability"))

ncs_levels = c("Openness", "Conscientiousness", "Extraversion", 
               "Agreeableness", "Emotional Stability")

boxplot_data$non_cogn_skills = 
  factor(boxplot_data$non_cogn_skills, levels = ncs_levels)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Boxplot data prepared:\n")
cat("  • Long format transformation completed\n")
cat("  • Employment status recoded (Employed/Unemployed)\n")
cat("  • NCS levels factorized in correct order\n\n")

cat("Creating employment transition boxplot...")
start_time <- Sys.time()

bp1 <-
  boxplot_data %>%
  mutate(Employment = "Transition: Successful") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = transition_successful)) +
  scale_fill_manual(values = c("Unemployed" = "#f8766b", "Employed" = "#00bfc4")) +  
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(Employment ~ non_cogn_skills)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")

cat("Creating SES boxplot...")
start_time <- Sys.time()

bp2 <-
  boxplot_data %>%
  mutate(`Income Per Capita Percentile` = "HH Income Per Capita Quintile") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = factor(ses5))) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(`Income Per Capita Percentile` ~ non_cogn_skills)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")

cat("Creating education level boxplot...")
start_time <- Sys.time()

bp3 <-
  boxplot_data %>%
  mutate(`Level of Education` = "Level of Education") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = edu_lvl)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(`Level of Education` ~ non_cogn_skills)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ All boxplots created:\n")
cat("  • bp1: Employment transition status\n")
cat("  • bp2: Household income quintiles\n")
cat("  • bp3: Education levels\n\n")

# bp_final <- grid.arrange(bp1, bp2, bp3, ncol = 1)

cat("🎨 PHASE 5: ADVANCED DENSITY VISUALIZATIONS\n")
cat(rep("-", 50), "\n")

cat("Preparing data for density plots...")
start_time <- Sys.time()

cat("Preparing data for density plots...")
start_time <- Sys.time()

# Prepare data for density plots
density_data <- youth_empl %>%
  select(sex, ses5, O, C, E, A, ES) %>%
  pivot_longer(cols = c(O, C, E, A, ES),
               names_to = "skill",
               values_to = "value") %>%
  mutate(skill = case_when(
    skill == "O" ~ "Openness",
    skill == "C" ~ "Conscientiousness", 
    skill == "E" ~ "Extraversion",
    skill == "A" ~ "Agreeableness",
    skill == "ES" ~ "Emotional Stability"
  )) %>%
  mutate(skill = factor(skill, levels = c("Openness", "Conscientiousness", 
                                         "Extraversion", "Agreeableness", 
                                         "Emotional Stability")))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")

cat("Creating NCS by sex boxplot visualization...")
start_time <- Sys.time()

# Create beautiful violin plot with boxplots inside
bp_sex_ncs <- 
  ggplot(density_data, aes(x = sex, y = value, fill = sex)) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.shape = 21, 
               outlier.fill = "white", outlier.size = 1.5) +
  scale_fill_manual(values = c("Female" = "#FF6B6B", "Male" = "#4ECDC4"),
                    name = "Sex") +
  facet_wrap(~ skill, nrow = 1) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, size = 0.5)
  ) +
  labs(x = "",
       y = "") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")

cat("Creating NCS by SES boxplot visualization...")
start_time <- Sys.time()

bp_ses5_ncs <- 
  ggplot(density_data, aes(x = ses5, y = value, fill = ses5)) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.shape = 21, 
               outlier.fill = "white", outlier.size = 1.5) +
  facet_wrap(~ skill, nrow = 1) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, size = 0.5)
  ) +
  labs(x = "",
       y = "") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Advanced visualizations created:\n")
cat("  • bp_sex_ncs: Big Five by gender (publication-ready)\n")
cat("  • bp_ses5_ncs: Big Five by SES quintiles\n")
cat("  • Professional styling with custom colors and themes\n\n")

cat("📊 PHASE 6: OCCUPATION CLASSIFICATION TABLE\n")
cat(rep("-", 50), "\n")

cat("Creating ISCO-08 occupation classification table...")
start_time <- Sys.time()

occup_table <-
  youth_empl %>%
  select(year, occupation) %>%
  # recode in accordance with isco-08 in russin МСКЗ-08
  mutate(occupation = case_when(occupation == 0 ~ "0. Военные",
                                occupation == 1 ~ "1. Руководители", 
                                occupation == 2 ~ "2. Специалисты-профессионалы",
                                occupation == 3 ~ "3. Специалисты-техники и иной средний специальный персонал",
                                occupation == 4 ~ "4. Служащие, занятые подготовкой и оформлением документации",
                                occupation == 5 ~ "5. Работники сферы обслуживания и торговли",
                                occupation == 6 ~ "6. Квалифицированные работники сельского хозяйств",
                                occupation == 7 ~ "7. Квалифицированные рабочие промышленности ",
                                occupation == 8 ~ "8. Операторы и сборщики промышленных установок и машин",
                                occupation == 9 ~ "9. Неквалифицированные работники",
                                TRUE ~ as.character(NA)
                                )) %>%
  tbl_summary(by = year, 
              type = list( 
                c(occupation) ~ 'categorical'
              ),
              label = list(# age ~ "Age",
                occupation ~ "Классификация занятости"
              ) ) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels() 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Occupation classification table created:\n")
cat("  • Standard: ISCO-08 (МСКЗ-08 in Russian)\n")
cat("  • Categories: 10 major occupation groups\n")
cat("  • Language: Russian labels for Russian publications\n")
cat("  • Format: FlexTable by survey year\n\n")

cat(rep("=", 80), "\n")
cat("🎉 DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 OUTPUTS CREATED:\n")
cat("  • summary_stats: Main employment statistics table (GT)\n")
cat("  • summary_stats_data_chapter: Russian employment table (FlexTable)\n")
cat("  • ncs_descr: Big Five descriptive statistics\n")
cat("  • bp1, bp2, bp3: Employment/SES/Education boxplots\n")
cat("  • bp_sex_ncs: Publication-ready gender differences plot\n")
cat("  • bp_ses5_ncs: SES differences visualization\n")
cat("  • occup_table: ISCO-08 occupation classification (Russian)\n")
cat("  • density_data, boxplot_data: Prepared datasets for further analysis\n")
cat("\n📈 VISUALIZATION OBJECTS:\n")
cat("  • Ready for export to publications\n")
cat("  • Professional styling applied\n")
cat("  • Multi-language support (English/Russian)\n")
cat(rep("=", 80), "\n")
cat("Script completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n") 
  


