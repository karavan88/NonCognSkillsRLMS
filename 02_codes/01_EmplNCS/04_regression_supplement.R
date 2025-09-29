#-------------------------------------------------------------------
# Project: Non-Cognitive Skills and Employment
# Script:  Regressions of Employment Outcomes by NCS
# Author:  Garen Avanesian
# Date:    27 April 2025
#-------------------------------------------------------------------

youth_empl_suppl <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, 
         edu_lvl, region, sex, employed, year, wave, occupation, 
         in_education, hh_inc_quintile, area_binary, area, ipw_empl,
         #empl_offic, 
         employed_officially,
         self_employed, satisf_job, j1_1_1, j5_2,
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
  # create age groups based on 15-29
  mutate(age_group = case_when(age >= 15 & age < 20 ~ "1. 15-19",
                               age >= 20 & age < 25 ~ "2. 20-24",
                               age >= 25 & age < 30 ~ "3. 25-29")) %>%
  mutate(in_education = factor(in_education)) %>%
  mutate(white_collar_hs = ifelse(occupation %in% c(1, 2, 3), 1, 0)) %>%
  mutate(white_collar_ls = ifelse(occupation %in% c(4, 5), 1, 0)) %>%
  mutate(blue_collar_hs = ifelse(occupation %in% c(6, 7), 1, 0)) %>%
  mutate(blue_collar_ls = ifelse(occupation %in% c(8, 9), 1, 0)) %>%
  mutate(white_collar = ifelse(occupation %in% c(1, 2, 3, 4, 5), 1, 0)) %>%
  mutate(age_factor = factor(age)) %>%
  mutate(skill_mismatch_overeduc = ifelse(edu_lvl == "4. Tertiary" & white_collar_hs !=1, 1, 0)) 

summary(youth_empl_suppl$white_collar)
summary(factor(youth_empl_suppl$edu_lvl))
summary(youth_empl_suppl$j5_2)
summary(youth_empl_suppl$skill_mismatch_overeduc)

table(youth_empl_suppl$edu_lvl, youth_empl_suppl$white_collar_hs)

# ### White collar overall
# m_occup_wc <- lmer(white_collar ~ 1 + 
#                      age + I(age^2) +
#                      sex + edu_lvl + area + ses5 + in_education + # controls
#                      O + C + E + A + ES + # NCS
#                      (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
#                    weights = ipw_empl,
#                    REML = T, data = youth_empl_suppl)
# 
# summary(m_occup_wc)
# 
# m_occup_wc_ses <- lmer(white_collar ~ 1 + 
#                             age + I(age^2) +
#                             sex + edu_lvl + area  + in_education + # controls
#                             O + C + E + A + ES + # NCS
#                             (1|region)  + (1|idind) + (1|edu_lvl) + # random intercepts
#                             (1 + O + C + E + A + ES | ses5), # random effects
#                           weights = ipw_empl,
#                           REML = T, 
#                           data = youth_empl_suppl)
# 
# summary(m_occup_wc_ses)

### White collar high-skilled

m_occup_wc_hs <- lmer(white_collar_hs ~ 1 + 
                        age + I(age^2) +
                        sex + edu_lvl + area + ses5 + in_education + # controls
                        O + C + E + A + ES + # NCS
                        (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
                      weights = ipw_empl,
                      REML = T, data = youth_empl_suppl)

summary(m_occup_wc_hs)
# icc(m_occup_wc_hs, by_group = T)

# Random slope by SES
m_occup_wc_hs_ses <- lmer(white_collar_hs ~ 1 + 
                            age + I(age^2) +
                            sex + edu_lvl + area + in_education + # controls
                            O + C + E + A + ES + # NCS
                            (1|idind) + (1|edu_lvl) + # random intercepts
                            (1 + O + C + E + A + ES | ses5), # random effects
                          weights = ipw_empl,
                          REML = T, 
                          data = youth_empl_suppl)
  
summary(m_occup_wc_hs_ses)

m_ocup_ses_coefs =
  coef(m_occup_wc_hs_ses)$`ses5` %>%
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

plot_ses_occup =
  ggplot(m_ocup_ses_coefs, aes(Estimate, ses5)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")


models_occup <-
  list("White Collar (High-Skilled)" = m_occup_wc_hs,
       "White Collar (High-Skilled) by SES"  = m_occup_wc_hs_ses)

reg_tables_occup <-
  modelsummary(models_occup,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               notes   = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output = "tinytable") 
  



## Other occupations

# ### White collar low-skilled
# m_occup_wc_ls <- lmer(white_collar_ls ~ 1 + 
#                         age + I(age^2) +
#                         sex + edu_lvl + area + ses5 + in_education + # controls
#                         O + C + E + A + ES + # NCS
#                         (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
#                       weights = ipw_empl,
#                       REML = T, data = youth_empl_suppl)
# 
# summary(m_occup_wc_ls)
# 
# ### Blue collar high-skilled
# m_occup_bc_hs <- lmer(blue_collar_hs ~ 1 + 
#                         age + I(age^2) +
#                         sex + edu_lvl + area + ses5 + in_education + # controls
#                         O + C + E + A + ES + # NCS
#                         (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
#                       weights = ipw_empl,
#                       REML = T, data = youth_empl_suppl)
# 
# summary(m_occup_bc_hs)

# ### Blue collar low-skilled
# m_occup_bc_ls <- lmer(blue_collar_ls ~ 1 + 
#                         age + I(age^2) +
#                         sex + edu_lvl + area + ses5 + in_education + # controls
#                         O + C + E + A + ES + # NCS
#                         (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
#                       weights = ipw_empl,
#                       REML = T, data = youth_empl_suppl)
# 
# summary(m_occup_bc_ls)
# 
# 
# models_occup <-
#   list("White Collar (High-Skilled)" = m_occup_wc_hs,
#        "White Collar (Low-Skilled)"  = m_occup_wc_ls,
#        "Blue Collar (High-Skilled)"  = m_occup_bc_hs,
#        "Blue Collar (Low-Skilled)"   = m_occup_bc_ls)
# 
# reg_tables_occup <- 
#   modelsummary(models_occup,
#                statistic = "({std.error}) {stars}",
#                gof_omit = "ICC|RMSE|cond|AIC|BIC",
#                coef_omit = "SD|Cor",
#                coef_rename = rename_vector_empl,
#                output = "gt") %>%
#   tab_source_note(
#     source_note = "Source: Calculations of the author based on the RLMS data.")


# m_overeduc_ipw <- lmer(skill_mismatch_overeduc ~ 1 + 
#                        age + I(age^2) +
#                        sex + area + ses5 + # in_education + # controls
#                        O + C + E + A + ES + # NCS
#                        (1|region)  + (1|idind), # random intercepts
#                      weights = ipw_empl,
#                      REML = T, 
#                   data = youth_empl_suppl[youth_empl_suppl$edu_lvl == "4. Tertiary", ])

# summary(m_overeduc_ipw)


m_overeduc <- lmer(skill_mismatch_overeduc ~ 1 + 
                         age + I(age^2) +
                         sex + area + ses5 + # in_education + # controls
                         O + C + E + A + ES + # NCS
                         (1|region)  + (1|idind), # random intercepts
                       REML = T, 
                       data = youth_empl_suppl[youth_empl_suppl$edu_lvl == "4. Tertiary", ])

# reg_tab_overeduc <-
#   modelsummary(m_overeduc,
#                statistic = "({std.error}) {stars}",
#                gof_omit = "ICC|RMSE|cond|AIC|BIC",
#                coef_omit = "SD|Cor",
#                coef_rename = rename_vector_empl,
#                output = "gt") %>%
#   tab_source_note(
#     source_note = "Source: Calculations of the author based on the RLMS data.")

reg_tab_overeduc <-
  modelsummary(m_overeduc,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               notes   = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output = "tinytable") 


