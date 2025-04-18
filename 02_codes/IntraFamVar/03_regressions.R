# Baseline model is lme4 that basically estimates variation in each 5 traits (5 models) due to id_h ind indid
# Then we can add covariates to the model
m_opns_base <- lmer(O  ~ 1 + (1|family_id) + (1|idind), data = ind_fam_full)
m_cons_base <- lmer(C  ~ 1 + (1|family_id) + (1|idind), data = ind_fam_full)
m_extr_base <- lmer(E  ~ 1 + (1|family_id) + (1|idind), data = ind_fam_full)
m_aggr_base <- lmer(A  ~ 1 + (1|family_id) + (1|idind), data = ind_fam_full)
m_emst_base <- lmer(ES ~ 1 + (1|family_id) + (1|idind), data = ind_fam_full)

icc_opns <- as.data.frame(performance::icc(m_opns_base, by_group = TRUE)) %>% mutate(trait = "Openness")
icc_cons <- as.data.frame(performance::icc(m_cons_base, by_group = TRUE)) %>% mutate(trait = "Conscientiousness")
icc_extr <- as.data.frame(performance::icc(m_extr_base, by_group = TRUE)) %>% mutate(trait = "Extraversion")
icc_aggr <- as.data.frame(performance::icc(m_aggr_base, by_group = TRUE)) %>% mutate(trait = "Agreeableness")
icc_emst <- as.data.frame(performance::icc(m_emst_base, by_group = TRUE)) %>% mutate(trait = "Emotional Stability")

icc_baseline <- 
  rbind(icc_opns, icc_cons, icc_extr, icc_aggr, icc_emst) %>%
  rename(Model = trait) %>%
  mutate(ICC = round(ICC*100, 1),
         Group = ifelse(Group == "family_id", "Family", "Individual"))

### Let's estimate between-family variance in NCS by lookoing at the resudial variance of separate models by wave
m_opns_base_2016 <- lmer(O  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 25,])
m_opns_base_2019 <- lmer(O  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 28,])

m_cons_base_2016 <- lmer(C  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 25,])
m_cons_base_2019 <- lmer(C  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 28,])

m_extr_base_2016 <- lmer(E  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 25,])
m_extr_base_2019 <- lmer(E  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 28,])

m_aggr_base_2016 <- lmer(A  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 25,])
m_aggr_base_2019 <- lmer(A  ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 28,])

m_emst_base_2016 <- lmer(ES ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 25,])
m_emst_base_2019 <- lmer(ES ~ 1 + (1|family_id), data = ind_fam_full[ind_fam_full$id_w == 28,])

icc_opns_2016 <- 
  as.data.frame(performance::icc(m_opns_base_2016, by_group = TRUE)) %>% 
  mutate(trait = "Openness",
         year = 2016) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_opns_2019 <-
  as.data.frame(performance::icc(m_opns_base_2019, by_group = TRUE)) %>% 
  mutate(trait = "Openness",
         year = 2019) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_cons_2016 <- 
  as.data.frame(performance::icc(m_cons_base_2016, by_group = TRUE)) %>% 
  mutate(trait = "Conscientiousness",
         year = 2016) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_cons_2019 <-
  as.data.frame(performance::icc(m_cons_base_2019, by_group = TRUE)) %>% 
  mutate(trait = "Conscientiousness",
         year = 2019) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)


icc_extr_2016 <-
  as.data.frame(performance::icc(m_extr_base_2016, by_group = TRUE)) %>% 
  mutate(trait = "Extraversion",
         year = 2016) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_extr_2019 <-
  as.data.frame(performance::icc(m_extr_base_2019, by_group = TRUE)) %>% 
  mutate(trait = "Extraversion",
         year = 2019) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_aggr_2016 <-
  as.data.frame(performance::icc(m_aggr_base_2016, by_group = TRUE)) %>% 
  mutate(trait = "Agreeableness",
         year = 2016) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_aggr_2019 <-
  as.data.frame(performance::icc(m_aggr_base_2019, by_group = TRUE)) %>% 
  mutate(trait = "Agreeableness",
         year = 2019) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_emst_2016 <-
  as.data.frame(performance::icc(m_emst_base_2016, by_group = TRUE)) %>% 
  mutate(trait = "Emotional Stability",
         year = 2016) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)

icc_emst_2019 <-
  as.data.frame(performance::icc(m_emst_base_2019, by_group = TRUE)) %>% 
  mutate(trait = "Emotional Stability",
         year = 2019) %>%
  rename(`ICC (Between Families)` = ICC) %>%
  mutate(`Residual Variance (Within Families)` = 1 - `ICC (Between Families)`) %>%
  select(trait, year, `ICC (Between Families)`, `Residual Variance (Within Families)`)


within_table <-
  bind_rows(icc_opns_2016, icc_opns_2019,
            icc_cons_2016, icc_cons_2019,
            icc_extr_2016, icc_extr_2019,
            icc_aggr_2016, icc_aggr_2019,
            icc_emst_2016, icc_emst_2019) %>%
  mutate(`ICC (Between Families)` = round(`ICC (Between Families)`*100, 1),
         `Residual Variance (Within Families)` = round(`Residual Variance (Within Families)`*100, 1))

# View(within_table)


# Socio-Demographic Covariates
common_formula <- 
  O ~ 
  age + I(age^2) + sex + first_born + male_siblings_perc +
  relations_with_father + disability +
  obtaining_higher_edu + employed + 
  (1 | family_id) + (1 | idind)

m_opns_socdem <- lmer(update(common_formula, O ~ .),  data = ind_fam_full)
m_cons_socdem <- lmer(update(common_formula, C ~ .),  data = ind_fam_full)
m_extr_socdem <- lmer(update(common_formula, E ~ .),  data = ind_fam_full)
m_aggr_socdem <- lmer(update(common_formula, A ~ .),  data = ind_fam_full)
m_emst_socdem <- lmer(update(common_formula, ES ~ .), data = ind_fam_full)

summary(m_opns_socdem)
summary(m_cons_socdem)
summary(m_extr_socdem)
summary(m_aggr_socdem)
summary(m_emst_socdem)

soc_dem_models <- list("Openness" = m_opns_socdem, 
                       "Conscientiousness" = m_cons_socdem, 
                       "Extraversion" = m_extr_socdem, 
                       "Agreeableness" = m_aggr_socdem, 
                       "Emotional Stability" = m_emst_socdem)

# modelsummary(soc_dem_models,
#              estimate = "{estimate}{stars}",
#              gof_omit = "ICC|RMSE")

### add prediction plot

#### Socio-Economic Outcomes Model

socecon_formula <- 
  O ~ age + I(age^2) + sex + 
  obtaining_higher_edu + employed + marital_status + having_kids +
  (1 | family_id) + (1 | idind)

m_opns_socecon <- lmer(update(socecon_formula, O ~ .), data = ind_fam_full)
m_cons_socecon <- lmer(update(socecon_formula, C ~ .), data = ind_fam_full)
m_extr_socecon <- lmer(update(socecon_formula, E ~ .), data = ind_fam_full)
m_aggr_socecon <- lmer(update(socecon_formula, A ~ .), data = ind_fam_full)
m_emst_socecon <- lmer(update(socecon_formula, ES ~ .), data = ind_fam_full)

summary(m_opns_socecon)
summary(m_cons_socecon)
summary(m_extr_socecon)
summary(m_aggr_socecon)
summary(m_emst_socecon)

# report(m_opns_socecon)
# report(m_cons_socecon)
# report(m_extr_socecon)
# report(m_aggr_socecon)
# report(m_emst_socecon)

# ind_fam_full$income_percentile <- percent_rank(ind_fam_full$hh_income_per_cap)
# 
# ind_fam_full$income_quartile <- as.factor(ifelse(ind_fam_full$income_percentile < 0.25, 1, 
#                                                    ifelse(ind_fam_full$income_percentile < 0.5, 2, 
#                                                           ifelse(ind_fam_full$income_percentile < 0.75, 3, 4))))
# 
# ind_fam_full$region <- as_factor(ind_fam_full$region)
# 
# ### household level factors model
# hh_formula <- 
#   O ~ age + I(age^2) + sex + region + area + income_percentile + nfam + 
#   (1 | family_id) + (1 | idind)
# 
# 
# m_opns_hh <- lmer(update(hh_formula, O ~ .), data = ind_fam_full)
# m_cons_hh <- lmer(update(hh_formula, C ~ .), data = ind_fam_full)
# m_extr_hh <- lmer(update(hh_formula, E ~ .), data = ind_fam_full)
# m_aggr_hh <- lmer(update(hh_formula, A ~ .), data = ind_fam_full)
# m_emst_hh <- lmer(update(hh_formula, ES ~ .), data = ind_fam_full)
# 
# summary(m_opns_hh)
# summary(m_cons_hh)
# summary(m_extr_hh)
# summary(m_aggr_hh)
# summary(m_emst_hh)
# 
# 
