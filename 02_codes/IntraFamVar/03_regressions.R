
# ind_fam_full <-
#   ind_fam_full %>%
#   filter(age >= 15)


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
         Group = ifelse(Group == "family_id", "Family", "Individual")) %>%
  pivot_wider(names_from = Group, 
              values_from = ICC) 

icc_long <-
  icc_baseline %>%
  mutate(Error = round(100 - (Family + Individual))) %>%
  pivot_longer(cols = c(Individual, Family, Error),
               names_to = "Source",
               values_to = "Variance")

# Plot stacked bar chart
ggplot(icc_long, aes(x = Model, y = Variance, fill = Source)) +
  geom_bar(stat = "identity", position = "fill", col = "black") +  # fill ensures 100% height
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Variance Decomposition of Non-Cognitive Skills",
    x = "",
    y = "Proportion of Total Variance",
    fill = "Variance Source"
  ) +
  theme_minimal() +
  # put legend at the bottom
  theme(legend.position = "bottom",
        #legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


# Extract variances
varcomp <- as.data.frame(VarCorr(m_opns_base))[, c("grp", "vcov")]
names(varcomp) <- c("Source", "Variance")

# Add residual manually if not included
total_var <- sum(varcomp$Variance)
varcomp$Proportion <- varcomp$Variance / total_var

# Make names more readable
varcomp$Source <- recode(varcomp$Source,
                         "family_id" = "Between Families",
                         "idind" = "Between Individuals",
                         "Residual" = "Within Individual")

ggplot(varcomp, aes(x = reorder(Source, -Proportion), y = Proportion, fill = Source)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Variance Decomposition of Openness",
       x = NULL, y = "Proportion of Total Variance") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Socio-Demographic Covariates
common_formula <- 
  O ~ 
  age + I(age^2) + sex + first_born + male_siblings_perc +
  relations_with_father + disability + 
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

modelsummary(soc_dem_models,
             estimate = "{estimate}{stars}",
             gof_omit = "ICC|RMSE")

### add prediction plot





# ind_fam_full$income_percentile <- percent_rank(ind_fam_full$hh_income_per_cap)
# 
# ind_fam_full$income_quartile <- as.factor(ifelse(ind_fam_full$income_percentile < 0.25, 1, 
#                                                    ifelse(ind_fam_full$income_percentile < 0.5, 2, 
#                                                           ifelse(ind_fam_full$income_percentile < 0.75, 3, 4))))
# 
# ind_fam_full$region <- as_factor(ind_fam_full$region)
# 
### household level factors model
# hh_formula <-
#   O ~ age + I(age^2) + sex + region + area + income_percentile + nfam +
#   (1 | family_id) + (1 | idind)
# # 
# # 
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


### Model of employment
employed_data <-
  ind_fam_full %>%
  filter(age >= 15) %>%
  mutate(officially_employed_binary = ifelse(employed == "2. Officially Employed", 1, 0))

empl_reg <- lmer(officially_employed_binary ~ 
                   age + I(age^2) + sex + h_edu + 
                   O + C + E + A + ES +
                   (1 | family_id) + (1 | idind),
                 data = employed_data)

summary(empl_reg)

empl_reg_sex <- lmer(officially_employed_binary ~ 
                   age + I(age^2) + h_edu + sex +
                   O + C + E + A + ES +
                   (1 | family_id) + (1 | idind) +
                     (O + C + E + A + ES | first_born),
                 data = employed_data)

summary(empl_reg_sex)

# same model but each NCS has an interaction term wiht first_born

### increase age for higher edu
table(ind_fam_full$obtaining_higher_edu, ind_fam_full$age, useNA = "always")

h_edu_dta <-
  ind_fam_full %>%
  filter(age >= 18) %>%
  mutate(obtaining_hedu = ifelse(obtaining_higher_edu == "2. Obtaining or obtained", 1, 0))

hedu_reg <- lmer(obtaining_hedu ~ 
                   age + I(age^2) + sex  + 
                   O + C + E + A + ES +
                   (1 | family_id) + (1 | idind),
                 data = h_edu_dta)

summary(hedu_reg)
icc(hedu_reg, by_group = T)

coef(hedu_reg)$idind %>%
  as.data.frame() %>%
  select(O, C, E, A, ES) %>%
  distinct() %>%
  mutate(model = "Higher Education") %>%
  pivot_longer(cols = c(O, C, E, A, ES), 
               names_to = "Variable", 
               values_to = "Estimate") %>%
  mutate(Variable = recode(Variable,
                           `O` = "Openness",
                           `C` = "Conscientiousness",
                           `E` = "Extraversion",
                           `A` = "Agreeableness",
                           `ES` = "Emotional Stability"))

hedu_reg1 <- lmer(obtaining_hedu ~ 
                   age + I(age^2) + sex  + 
                   O + C + E + A + ES +
                   (1 | family_id) + (1 | idind) +
                    (O + C + E + A + ES |hh_inc_quintile),
                 data = h_edu_dta)

summary(hedu_reg)

# export regression coefficient 

coef(hedu_reg1)$hh_inc_quintile %>%
  as.data.frame() %>%
  mutate(Variable = rownames(.)) %>%
  rename(Estimate = `(Intercept)`) %>%
  select(Variable, Estimate) %>%
  mutate(Variable = recode(Variable,
                           `O` = "Openness",
                           `C` = "Conscientiousness",
                           `E` = "Extraversion",
                           `A` = "Agreeableness",
                           `ES` = "Emotional Stability")) 


hedu_empl_models <- list("Higher Education" = hedu_reg, 
                         "Employment" = empl_reg)

modelsummary(hedu_empl_models,
             estimate = "{estimate}{stars}",
             gof_omit = "ICC|RMSE")

