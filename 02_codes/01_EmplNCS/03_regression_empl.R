#-------------------------------------------------------------------
# Project: Non-Cognitive Skills and Employment
# Script:  Regressions of Employment by NCS
# Author:  Garen Avanesian
# Date:    17 December 2024
#-------------------------------------------------------------------

youth_empl <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, 
         edu_lvl, region, sex, employed, year, wave,
         in_education, hh_inc_quintile, area_binary, area,
         #empl_offic, 
         employed_officially,
         self_employed, satisf_job, j1_1_1, 
         O, C, E, A, ES) %>%
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
  mutate(age_group = case_when(age >= 15 & age < 20 ~ "1. 15-19",
                               age >= 20 & age < 25 ~ "2. 20-24",
                               age >= 25 & age < 30 ~ "3. 25-29")) %>%
  mutate(age_factor = factor(age)) %>%
  mutate(in_education = factor(in_education))
 

dim(youth_empl)
length(unique(youth_empl$idind))

summary(factor(youth_empl$employed))
summary(factor(youth_empl$employed_officially))
summary(factor(youth_empl$self_employed))
summary(factor(youth_empl$transition_successful1))
summary(factor(youth_empl$self_empl_offic_and_saisf))
summary(factor(youth_empl$transition_successful))
# summary(factor(youth_empl$hh_inc_quintile))
summary(factor(youth_empl$sex))
summary(factor(youth_empl$age))
summary(factor(youth_empl$ses5))

table(youth_empl$self_employed, youth_empl$j1_1_1)

# View(youth_empl)


m1_empl <- lmer(transition_successful ~ 1 + 
                      (1|region)  + (1|idind) + (1|age_factor) + (1|edu_lvl),
                    REML = T, data = youth_empl )

icc(m1_empl, by_group = TRUE)



m2_empl <- lmer(transition_successful ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + edu_lvl + age + area + in_education + ses5 + # controls
                 (1|region)  + (1|idind) + (1|age_factor), # random intercepts
               REML = T, data = youth_empl)

summary(m2_empl)


### Predict the effect of age
age_effect_females <- 
  ggpredict(m2_empl, 
            terms = "age [all]", 
            type = "fixed", 
            condition = c(sex = "Female", 
                          ses5 = "Q3", 
                          edu_lvl = "4. Tertiary", 
                          area = "City"))

age_effect_males <- 
  ggpredict(m2_empl, 
            terms = "age [all]", 
            type = "fixed", 
            condition = c(sex = "Male", 
                          ses5 = "Q3", 
                          edu_lvl = "4. Tertiary", 
                          area = "City"))

Age = age_effect_females$x
Female = age_effect_females$predicted
Male = age_effect_males$predicted

age_effect_data <-
  data.frame(Age, Female, Male) %>%
  pivot_longer(cols = c("Male", "Female"),
               names_to = "Sex",
               values_to = "Value") %>%
  mutate(Value = Value * 100) %>%
  filter(Sex == "Female")
  

age_effect_sel <-
  ggplot(age_effect_data, aes(Age, Value)) + # , color = Sex
  geom_line(linewidth = 1) +
  xlim(20, 30) +
  scale_x_continuous(breaks = c(21, 22, 23, 24, 25, 26, 27, 28, 29),
                     limits = c(21, 29.5)) +
  # plot x axis only between 20 and 30
  
  theme_bw() +
  ylim(45, 100) +
  ylab("Probability of Completed Transition (%)") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

m3_empl <- lmer(transition_successful ~ 1 + 
                  sex + edu_lvl + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age), # random effects
                REML = T, data = youth_empl)

summary(m3_empl)
modelsummary(m3_empl)

### Create a table of regressions

models_empl1 <- list("Baseline Model" = m1_empl,
                     "Model with NCS"  = m2_empl,
                     "Model with NCS and controls" = m3_empl)

rename_vector_empl <- 
  c(`(Intercept)` = "Intercept",
    sexMale = "Sex: Male",
    `edu_lvl2. Secondary School` = "Education: Secondary",
    `edu_lvl3. Secondary Vocational` = "Education: Vocational",
    `edu_lvl4. Tertiary` = "Education: Tertiary",
    `areaUrban-Type Settlement` = "Area: Urban-Type Settlement",
    areaCity = "Area: City",
    `areaRegional Center` = "Area: Regional Center",
    `in_education1` = "Currently studying: Yes",
    # ses5Q2 = "SES: Q2",
    # ses5Q3 = "SES: Q3",
    # ses5Q4 = "SES: Q4",
    # ses5Q5 = "SES: Q5",
    O = "Openness",
    C = "Conscientiousness",
    E = "Extraversion",
    A = "Agreeableness",
    ES = "Emotional Stability")


reg_tables_mem_fixed <- 
  modelsummary(models_empl1,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               output = "gt") %>%
  tab_source_note(
    source_note = "Source: Calculations of the author based on the RLMS data.")







### SES ####

m4_empl <- lmer(transition_successful ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + edu_lvl + in_education + area + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | ses5), # random slopes
                REML = T, data = youth_empl)

summary(m4_empl)

m4_empl_coefs =
  coef(m4_empl)$`ses5` %>%
  as.data.frame() %>%
  rownames_to_column(var = "ses5") %>%
  select(ses5, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -ses5) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

plot_ses =
  ggplot(m4_empl_coefs, aes(Estimate, ses5)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")


### EDU ####

m5_empl <- lmer(transition_successful ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | edu_lvl), # random slopes
                REML = T, data = youth_empl)

summary(m5_empl)

m5_empl_coefs =
  coef(m5_empl)$`edu_lvl` %>%
  as.data.frame() %>%
  rownames_to_column(var = "edu_lvl") %>%
  select(edu_lvl, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -edu_lvl) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

plot_edu =
  ggplot(m5_empl_coefs, aes(Estimate, edu_lvl)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")

### SEX ####

m6_empl <- lmer(transition_successful ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  edu_lvl + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | sex), # random slopes
                REML = T, data = youth_empl)

m6_empl_coefs =
  coef(m6_empl)$`sex` %>%
  as.data.frame() %>%
  rownames_to_column(var = "sex") %>%
  select(sex, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -sex) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

plot_sex =
  ggplot(m6_empl_coefs, aes(Estimate, Skill)) +
  geom_point(size = 9, aes(color = sex)) +
  geom_text(aes(label = round(Estimate, 2)), 
            size = 2, color = "black",
            check_overlap = TRUE) +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  ylab("") +
  xlab("") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


models_empl2 <-
  list("Model with SES" = m4_empl,
       "Model with Education" = m5_empl,
       "Model with Sex" = m6_empl)

reg_tables_mem_random <- 
  modelsummary(models_empl2,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               output = "gt") %>%
  tab_source_note(
    source_note = "Source: Calculations of the author based on the RLMS data.")

