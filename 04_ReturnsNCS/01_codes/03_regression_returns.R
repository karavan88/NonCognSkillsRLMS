#-------------------------------------------------------------------
# Project: Returns to NCS
# Script:  RLMS 2016-2019 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    1 December 2024
#-------------------------------------------------------------------

main_folder = "~/Documents/GitHub/Thesis"


youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Female", "Male"))
youth_master_returns$edu_lvl <- factor(youth_master_returns$edu_lvl, levels = c("1. No school", 
                                                                "2. Secondary School",
                                                                "3. Secondary Vocational",
                                                                "4. Tertiary"))

summary(youth_master_returns$sex)
summary(youth_master_returns$edu_lvl)

# generate a var area_urban 
youth_master_returns$area1 = ifelse( youth_master_returns$area %in% c("Областной центр", "Город"), "urban", "rural")
# set factor levels with rural as base
youth_master_returns$area1 = factor(youth_master_returns$area1, levels = c("rural", "urban"))




youthOutput <- "~/Documents/GitHub/Thesis/02_codes/02_ReturnsNCS/outputs"

# summary(data_merged$area)
# summary(data_merged$area_binary)

########################################
### ------------ Regressions -----------
########################################

general_formula <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + sex  + marital_status + 
  region + O + C + E + A + ES 

### ------ BASELINE REGRESSIONS ------- ###
m1 <- lqmm(
  general_formula,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

m1_summary <-
  summary(m1)

m1_aic <- m1_summary$aic
m1_loglik <- m1_summary$logLik

m1_coefs <-
  m1_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

# dim(m1_coefs)

new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m1_coefs) <- new_names

# View(m1_coefs)

# write a csv file with the regression results
write_csv(m1_coefs, file.path(youthOutput, "m1_coefs.csv"))

m1_ipw <- lqmm(
  general_formula,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw,
  control = list(method = "df")
)

m1_ipw_summary <-
  summary(m1_ipw)

m1_ipw_aic <- m1_ipw_summary$aic
m1_ipw_loglik <- m1_ipw_summary$logLik

m1_ipw_coefs <-
  m1_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m1_ipw_coefs) <- new_names

# View(m1_ipw_coefs)

# write a csv file with the regression results
write_csv(m1_ipw_coefs, file.path(youthOutput, "m1_ipw_coefs.csv"))


### ------ EXTENDED REGRESSIONS (WITH EDU) ------- ###

formula_edu <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2)  + sex + area + region + marital_status + 
  O + C + E + A + ES

m2 <- lqmm(
  formula_edu,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

m2_ipw <- lqmm(
  formula_edu,
  data = youth_master_returns,
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw,
  control = list(method = "df")
)

m2_summary <-
  summary(m2)

m2_ipw_summary <-
  summary(m2_ipw)

m2_coefs <-
  m2_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m2_coefs) <- new_names

write_csv(m2_coefs, file.path(youthOutput, "m2_coefs.csv"))

m2_ipw_coefs <-
  m2_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

names(m2_ipw_coefs) <- new_names

# View(m2_ipw_coefs)

write_csv(m2_ipw_coefs, file.path(youthOutput, "m2_ipw_coefs.csv"))


### ------ sex  ------- ###


###########################################################################################
#####################       create a lqmm model  by sex           ########################
###########################################################################################

formula_sex <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + marital_status + 
  region + O + C + E + A + ES 

m6_female <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Female", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw[youth_master_returns$sex == "Female"],
       control = list(method = "df")
  )

m6_female_summary <- 
  summary(m6_female)

m6_female_coefs <-
  m6_female_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

names(m6_female_coefs)

# Reshape the dataset to long format
m6_female_long <- 
  m6_female_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>%
  mutate(sex = "Female")

# View(m6_female_long)

m6_male <-
  lqmm(formula_sex,
       data = youth_master_returns[youth_master_returns$sex == "Male", ],
       random = ~1,
       group = idind,
       tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
       weights = youth_master_returns$ipw[youth_master_returns$sex == "Male"],
       control = list(method = "df")
  )

m6_male_summary <- 
  summary(m6_male)

m6_male_coefs <-
  m6_male_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) 

# Reshape the dataset to long format
m6_male_long <- 
  m6_male_coefs %>%
  pivot_longer(
    cols = -variable,
    names_to = c("model", ".value"),
    names_pattern = "X(0\\.1|0\\.25|0\\.5|0\\.75|0\\.9)\\.(.*)"
  ) %>% 
  mutate(sex = "Male")

View(m6_male_long)

# bind data
m6_coefs <- 
  bind_rows(m6_female_long, m6_male_long)

new_names_sex_model <- c("variable", "quantile",  "estimate", "std.error", "ci.low", "ci.upp", "p.value", "sex")
names(m6_coefs) <- new_names_sex_model

m6_coefs$quantile = as.numeric(m6_coefs$quantile) * 100
m6_coefs$quantile = paste0("Q", m6_coefs$quantile)

# choose the ncs only

m6_ncs <- 
  m6_coefs %>%
  filter(str_detect(variable, "O|C|E|A|ES")) %>%
  filter(!str_detect(variable, "area")) %>%
  filter(!str_detect(variable, "marital")) %>%
  select(variable, sex, quantile, estimate, std.error, ci.low, ci.upp, p.value
  )

# View(m6_ncs)

###-----------------------------------------------------------------------------
### -------------- Interaction 
###-----------------------------------------------------------------------------

# youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Male", "Female"))


formula_gend_int <- 
  log_wage ~  exp_imp + I(exp_imp^2)  + area + sex + marital_status + region + 
  O + C + E + A + ES  +
  O*sex + C*sex + E*sex + A*sex + ES*sex 

# we need to make female as a base level in sex variable
# data_merged$sex <- relevel(data_merged$sex, ref = "female")
# levels(data_merged$sex)

m7 <- lqmm(formula_gend_int,
           data = youth_master_returns,
           random = ~1,
           group = idind,
           tau = 0.50,
           control = list(method = "df")
)

m7_ipw <- lqmm(formula_gend_int,
               data = youth_master_returns,
               random = ~1,
               group = idind,
               tau = c(0.5, 0.75, 0.9),
               weights = youth_master_returns$ipw,
               control = list(method = "df")
)

# extract the coefficients
m7_summary <- 
  summary(m7)

m7_ipw_summary <-
  summary(m7_ipw)

m7_coefs <-
  m7_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

m7_ipw_coefs <-
  m7_ipw_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3)

new_names_upp <- c("variable", 
                   "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
                   "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
                   "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m7_ipw_coefs) <- new_names_upp

# View(m7_ipw_coefs)

write.csv(m7_ipw_coefs, file.path(youthOutput, "m7_ipw_sex_ncs_int.csv"))



###########################################################################################
#####################   create a lqmm model  by education level        ####################
###########################################################################################

### Tertiary
m4_tert <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.5, 0.75, 0.9),
  control = list(method = "df")
)

### secondary vocational
m4_voc <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)


### secondary or below
m4_sec <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  control = list(method = "df")
)

### extract the coefs

# tertiary
m4_tert_summary <- 
  summary(m4_tert)

m4_voc_summary <- 
  summary(m4_voc)

m4_sec_summary <- 
  summary(m4_sec)

m4_tert_coefs <- 
  m4_tert_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Tertiary") 

m4_voc_coefs <- 
  m4_voc_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary Vocational") 

m4_sec_coefs <- 
  m4_sec_summary$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary or below")


# merge the data
m4_coefs <- 
  bind_rows(m4_tert_coefs, 
            m4_voc_coefs,  
            m4_sec_coefs) %>%
  rownames_to_column(var = "variable") 

# View(m4_coefs)


write_csv(m4_coefs, file.path(youthOutput, "m4_coefs_edu.csv"))

### now do absolutely te same models but with ipw weights 
m4_tert_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl == "4. Tertiary"],
  control = list(method = "df")
)

m4_voc_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl == "3. Secondary Vocational"],
  control = list(method = "df")
)

m4_sec_ipw <- lqmm(
  general_formula,
  data = youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ],
  random = ~1,
  group = idind,
  tau = c(0.10, 0.25, 0.50, 0.75, 0.90),
  weights = youth_master_returns$ipw[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School")],
  control = list(method = "df")
)

m4_tert_summary_ipw <- 
  summary(m4_tert_ipw)

m4_voc_summary_ipw <-
  summary(m4_voc_ipw)

m4_sec_summary_ipw <-
  summary(m4_sec_ipw)

m4_tert_coefs_ipw <-
  m4_tert_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Tertiary")

m4_voc_coefs_ipw <-
  m4_voc_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary Vocational")

m4_sec_coefs_ipw <-
  m4_sec_summary_ipw$tTable %>%
  as.data.frame() %>%
  mutate(model = "Secondary or below")

m4_coefs_ipw <-
  bind_rows(m4_tert_coefs_ipw,
            m4_voc_coefs_ipw,
            m4_sec_coefs_ipw) %>%
  rownames_to_column(var = "variable")

write_csv(m4_coefs_ipw, file.path(youthOutput, "m4_coefs_edu_ipw.csv"))


### Life Course Perspective ####


m_lc <- lqmm(
  general_formula,
  data = ind_master_returns,
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl,
  control = list(method = "df")
)

m_lc_summary <- 
  summary(m_lc)

m_lc_coefs <-
  m_lc_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "16-65")

m_lc_3039 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
  control = list(method = "df")
)

m_lc_3039_summary <- 
  summary(m_lc_3039)

m_lc_3039_coefs <-
  m_lc_3039_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "30-40")

m_lc_4049 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
  control = list(method = "df")
)

m_lc_4049_summary <- 
  summary(m_lc_4049)

m_lc_4049_coefs <-
  m_lc_4049_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "40-50")

m_lc_5065 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 50],
  control = list(method = "df")
)

m_lc_5065_summary <- 
  summary(m_lc_5065)

m_lc_5065_coefs <-
  m_lc_5065_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "50-65")

m_lc <- 
  bind_rows(m_lc_coefs, m_lc_3039_coefs, m_lc_4049_coefs, m_lc_5065_coefs) 

write_csv(m_lc, file.path(youthOutput, "m_lc_ipw_coefs.csv"))

### do the gam model for age and log wage
library(mgcv)

gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status, data = ind_master_returns)

age_gam <- plot(gam, se = TRUE, col = "black")
