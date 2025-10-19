#-------------------------------------------------------------------
# Project: Non-Cognitive Skills and Employment
# Script:  Regressions of Employment by NCS
# Author:  Garen Avanesian
# Date:    17 December 2024
#-------------------------------------------------------------------

m1_baseline <- lmer(working ~ 1 + (1|region)  + (1|idind) + (1|age),
                          REML = T, data = ind_master_empl)

icc(m1_baseline, by_group = TRUE)

m2_ncs <- lmer(working ~ 1 + sex + edu_lvl + area_binary + 
                 O + C + E + A + ES + (1|region)  + (1|idind) + (1|age),
                          REML = T, data = ind_master_empl)

summary(m2_ncs)

# random slope of NCS by hh income
m3_ses_ncs <- lmer(working ~ sex + edu_lvl + area_binary + 
                     O + C + E + A + ES + 
                     (1|idind) + (1|age) +
                     (1 + O + C + E + A + ES | hh_inc_quintile ),
                   REML = T, data = ind_master_empl)

summary(m3_ses_ncs) 


m3_ses =
  coef(m3_ses_ncs)$`hh_inc_quintile` %>%
  as.data.frame() %>%
  rownames_to_column(var = "hh_inc_quintile") %>%
  select(hh_inc_quintile, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -hh_inc_quintile) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))
