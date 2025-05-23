#-------------------------------------------------------------------
# Project: Non-Cognitive Skills and Employment
# Script:  Descriptive Statistics of Job Satisfaction by NCS
# Author:  Garen Avanesian
# Date:    17 December 2024
#-------------------------------------------------------------------


youth_js <- 
  youth_job_satisf %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, edu_lvl, region, sex, year, wages_imp,
        area,
        work_hrs_per_week, 
        occupation, industry,
        starts_with("satisf"), 
        j1_1_1, j1_1_2, j1_1_3, j1_1_4,
        O, C, E, A, ES) %>%
  # if NA in edu_lvv that assign No School - this will fill 4 observations
  mutate(edu_lvl = ifelse(is.na(edu_lvl), "1. No school", edu_lvl)) 

# View(youth_js)

summary_stats_js <-
  youth_js %>%
  select(age, sex, edu_lvl, area, year, occupation, 
         O, C, E, A, ES,
         satisf_job, satisf_labor_cond, satisf_wage, satisf_career) %>%
  tbl_summary(by = year, 
              type = list( c(age, O, C, E, A, ES) ~ 'continuous2',
                           # c(employed, employed_officially, self_employed, transition_successful,
                           #   in_education, satisfied_with_job) ~ 'dichotomous',
                           c(area, edu_lvl, occupation) ~ 'categorical'),
              label = list(age ~ "Age",
                           area ~ "Area",
                           sex ~ "Sex",
                           edu_lvl ~ "Highest Level of Education",
                           occupation ~ "Occupation",
                           O ~ "Openness",
                           C ~ "Conscientiousness",
                           E ~ "Extraversion",
                           A ~ "Agreeableness",
                           ES ~ "Emotional Stability",
                           satisf_job ~ "Satisf: Job Overall", 
                           satisf_labor_cond ~ "Satisf: Labor Conditions",
                           satisf_wage ~ "Satisf: Pay", 
                           satisf_career ~ "Satisf: Career Opport") , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data")) 

ncs_descr_js <-
  youth_js %>%
  select(O, C, E, A, ES) %>%
  rename(Openness = O,
         Conscientiousness = C,
         Extraversion = E,
         Agreeableness = A,
         `Emotional Stability` = ES) %>%
  datasummary_skim()

### create a ggcorrplot of job satisfaction domains with pearson rank correlations

youth_js_corr_dat <-
  youth_js %>%
  # select(starts_with("satisf")) %>%
  select(starts_with("j1")) %>%
  # select(-satisf_wbl) %>%
  rename(
  #.       `Job Satisfaction` = satisf_job, 
  #        `Labor Conditions` = satisf_labor_cond,
  #        `Pay` = satisf_wage, 
  #        `Career Opportunities` = satisf_career
         `Job Satisfaction` = j1_1_1, 
         `Labor Conditions` = j1_1_2,
         `Pay` = j1_1_3, 
         `Career Opportunities` = j1_1_4)

youth_js_corr <-
  cor(youth_js_corr_dat, use = "pairwise.complete.obs", method = "spearman") %>%
  round(2) %>%
  as.data.frame() 

# Create a correlation matrix plot
js_corr_matrix <-
  ggcorrplot(
  youth_js_corr |> as.matrix(),   # remove 'satisf' column, keep only matrix
  hc.order = TRUE,                      # hierarchical clustering of variables
  type = "lower",                       # show only lower part
  lab = TRUE,                           # add correlation values
  lab_size = 3,                         # label size
  colors = c("red", "white", "blue"),   # color scheme for negative/positive
  ggtheme = ggplot2::theme_minimal()
)



