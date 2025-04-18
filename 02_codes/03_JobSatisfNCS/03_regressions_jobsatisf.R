#-------------------------------------------------------------------
# Project: NCS and Job Satisfaction
# Script:  Regression to predict job satisfaction via NCS
# Author:  Garen Avanesian
# Date:    12 January 2025
#-------------------------------------------------------------------

### We start with GAM model of wages on job satisfaction

gam_job_satisf <- gam(satisf_job ~ s(log(hourly_wage)) ,
                      data = youth_job_satisf, family = binomial)

summary(factor(youth_job_satisf$edu_lvl))

table(youth_job_satisf$edu_lvl, youth_job_satisf$age)

# plot(gam_job_satisf)

### Mixed-Effects Models of Overall Job Satisfaction ####

# Draw a baseline model
m0_satisf <- 
  lmer(satisf_job ~ 1 + 
         (1 | idind) + (1 | region) + (1 | occupation) + 
         (1 | hourly_wage_quintile), 
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
       data = youth_job_satisf)

summary(m00_satisf_base)
icc(m0_satisf, by_group = TRUE)

edu_lvl_levels <- c("4. Tertiary", "1. No school", "2. Secondary School", "3. Secondary Vocational" )

youth_job_satisf$edu_lvl <- factor(youth_job_satisf$edu_lvl, levels = edu_lvl_levels)

# Reference Model
m1_satisf <- 
  lmer(satisf_job ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         (1 | idind) + (1 | region) + (1 | occupation),  
       weights = ipw_empl,
       data = youth_job_satisf)

modelsummary(m1_satisf)

m1.5_satisf <- 
  lmer(satisf_job ~ 1 + 
         # age + I(age^2) + sex + edu_lvl + area + 
         # log(wages_imp) + 
         # work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

m = modelsummary(m1.5_satisf)


# Reference Model
m2_satisf <- 
  lmer(satisf_job ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

# Model with random slopes of NCS by wage quintile
m3_satisf <- 
  lmer(satisf_job ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
        # log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation) + 
         (O + C + E + A + ES | hourly_wage_quintile), 
       weights = ipw_empl,
       data = youth_job_satisf)

m3_satisf_coefs =
  coef(m3_satisf)$hourly_wage_quintile %>%
  as.data.frame() %>%
  rownames_to_column(var = "hourly_wage_quintile") %>%
  select(hourly_wage_quintile, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -hourly_wage_quintile) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

plot_wages_ncs =
  ggplot(m3_satisf_coefs, aes(Estimate, hourly_wage_quintile)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")

### Create a table of regressions

models_job_satisf <- list("Model with Socio-Economic Charachteristics" = m1_satisf,
                          "Model supplemented with NCS"  = m2_satisf,
                          "Model with Random Slopes of NCS by Wage Quintile" = m3_satisf)

rename_vector <- c(`(Intercept)` = "Intercept",
                   age = "Age",
                   `I(age^2)` = "Age Squared",
                   sexMale = "Sex: Male",
                   `edu_lvl2. Secondary School` = "Education: Secondary",
                   `edu_lvl3. Secondary Vocational` = "Education: Vocational",
                   `edu_lvl4. Tertiary` = "Education: Tertiary",
                   `areaUrban-Type Settlement` = "Area: Urban-Type Settlement",
                   areaCity = "Area: City",
                   `areaRegional Center` = "Area: Regional Center",
                   `log(wages_imp)` = "Hourly Wage (Log)",
                   work_hrs_per_week = "Working Hours Per Week",
                   O = "Openness",
                   C = "Conscientiousness",
                   E = "Extraversion",
                   A = "Agreeableness",
                   ES = "Emotional Stability")


models_job_satisf <- 
  modelsummary(models_job_satisf,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               output = "gt") %>%
  tab_source_note(
    source_note = "Source: Calculations of the author based on the RLMS data.")



### Models for other domains of job satisfaction ####

m4_satisf <- 
  lmer(satisf_career ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

m5_satisf <- 
  lmer(satisf_labor_cond ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)


m6_satisf <- 
  lmer(satisf_wage ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)


m7_satisf <- 
  lmer(satisf_wbl ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

models_satisf <- list("Career" = m4_satisf,
                      "Working Conditions"  = m5_satisf,
                      "Wages" = m6_satisf)

models_satisf2 <- 
  modelsummary(models_satisf,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               output = "gt") %>%
  tab_source_note(
    source_note = "Source: Calculations of the author based on the RLMS data.")
