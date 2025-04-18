#-------------------------------------------------------------------
# Project: Returns to Non-Cognitive Skills 
# Organization: SFedU Future Skills Research Lab
# Objective: Prepare tables for the manuscript
# Author:  Garen Avanesian
# Date: 31 October 2024
#-------------------------------------------------------------------

youthOutput <- "~/Documents/GitHub/Thesis/02_codes/02_ReturnsNCS/outputs"

nrow_base <- nrow(youth_master_returns)
ngrp_base <- as.character(length(unique(youth_master_returns$idind)))

new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")



### Table 1 and 2: Baseline model ####

base_reg <- 
  read_csv(file.path(youthOutput, "m1_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)"  ~ "Intercept",
                              variable == "exp_imp"      ~ "Experience",
                              variable == "I(exp_imp^2)" ~ "Experience Sqd.",
                              variable == "areaГород"    ~ "Area: Urban",
                              variable == "areaПГТ"      ~ "Area: Settlement",
                              variable == "areaСело"     ~ "Area: Rural",
                              variable == "gendermale"   ~ "gender: Male",
                              variable == "O"            ~ "Openness",
                              variable == "C"            ~ "Conscientiousness",
                              variable == "E"            ~ "Extraversion",
                              variable == "A"            ~ "Agreeableness",
                              variable == "ES"           ~ "Emotional Stability")) %>%
  # we wanna add one row where variable = "region" and values for other columns are "controlled"
  add_row(variable = "Region", Q10 = "Controlled", Q25 = "Controlled", Q50 = "Controlled", Q75 = "Controlled", Q90 = "Controlled") %>%
  # we need to add model fit rows with aic and then next loglik
  #add_row(variable = "AIC", Q10 = as.character(round(m1_aic[1], 2)), Q25 = as.character(round(m1_aic[2], 2)), Q50 = as.character(round(m1_aic[3], 2)), Q75 = as.character(round(m1_aic[4], 2)), Q90 = as.character(round(m1_aic[5], 2))) %>%
  #add_row(variable = "Log Likelihood", Q10 = as.character(round(m1_loglik[1], 2)), Q25 = as.character(round(m1_loglik[2], 2)), Q50 = as.character(round(m1_loglik[3], 2)), Q75 = as.character(round(m1_loglik[4], 2)), Q90 = as.character(round(m1_loglik[5], 2))) %>%
  add_row(variable = "No. Groups", Q10 = ngrp_base, Q25 = ngrp_base, Q50 = ngrp_base, Q75 = ngrp_base, Q90 = ngrp_base) %>%
  add_row(variable = "No. Obs", Q10 = as.character(nrow_base), Q25 = as.character(nrow_base), Q50 = as.character(nrow_base), Q75 = as.character(nrow_base), Q90 = as.character(nrow_base)) 

### Model into the paper
base_reg_ipw <-
  read_csv(file.path(youthOutput, "m1_ipw_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)"  ~ "Intercept",
                              variable == "exp_imp"      ~ "Experience",
                              variable == "I(exp_imp^2)" ~ "Experience Sqd.",
                              variable == "areaCity"     ~ "Area: City",
                              variable == "areaUrban-Type Settlement"      ~ "Area: Settlement",
                              variable == "areaRegional Center"            ~ "Area: Reg Center",
                              variable == "sexMale"      ~ "Sex: Male",
                              variable == "marital_status2. Married/Civil partnership" ~ "Family: Married",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Family: Divorced",
                              variable == "O"           ~ "Openness",
                              variable == "C"           ~ "Conscientiousness",
                              variable == "E"           ~ "Extraversion",
                              variable == "A"           ~ "Agreeableness",
                              variable == "ES"          ~ "Emotional Stability",
                              TRUE ~ variable)) %>%
  # we wanna add one row where variable = "region" and values for other columns are "controlled"
  add_row(variable = "Region", Q10 = "Controlled", Q25 = "Controlled", Q50 = "Controlled", Q75 = "Controlled", Q90 = "Controlled") %>%
  # we need to add model fit rows with aic and then next loglik
  #add_row(variable = "AIC", Q10 = as.character(round(m1_ipw_aic[1], 2)), Q25 = as.character(round(m1_ipw_aic[2], 2)), Q50 = as.character(round(m1_ipw_aic[3], 2)), Q75 = as.character(round(m1_ipw_aic[4], 2)), Q90 = as.character(round(m1_ipw_aic[5], 2))) %>%
  #add_row(variable = "Log Likelihood", Q10 = as.character(round(m1_ipw_loglik[1], 2)), Q25 = as.character(round(m1_ipw_loglik[2], 2)), Q50 = as.character(round(m1_ipw_loglik[3], 2)), Q75 = as.character(round(m1_ipw_loglik[4], 2)), Q90 = as.character(round(m1_ipw_loglik[5], 2))) %>%
  add_row(variable = "No. Groups", Q10 = ngrp_base, Q25 = ngrp_base, Q50 = ngrp_base, Q75 = ngrp_base, Q90 = ngrp_base) %>%
  add_row(variable = "No. Obs", Q10 = as.character(nrow_base), Q25 = as.character(nrow_base), Q50 = as.character(nrow_base), Q75 = as.character(nrow_base), Q90 = as.character(nrow_base)) 

# View(base_reg_ipw)

### Table 3 and 4: Extended model ####

# write_csv(m3_coefs, file.path(youthOutput, "m3_coefs.csv"))
# write_csv(m3_ipw_coefs, file.path(youthOutput, "m3_ipw_coefs.csv"))


extd_reg <- 
  read_csv(file.path(youthOutput, "m2_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)" ~ "Intercept",
                              variable == "exp"         ~ "Experience",
                              variable == "I(exp^2)"    ~ "Experience Sqd.",
                              variable == "areaurban"   ~ "Area: Urban",
                              variable == "gendermale"     ~ "gender: Male",
                              variable == "edu_lvl2. Secondary School"  ~ "Education: Secondary",
                              variable == "edu_lvl3. Secondary Vocational"  ~ "Education: Secondary Vocational",
                              variable == "edu_lvl4. Tertiary"  ~ "Education: Tertiary",
                              variable == "O"           ~ "Openness",
                              variable == "C"           ~ "Conscientiousness",
                              variable == "E"           ~ "Extraversion",
                              variable == "A"           ~ "Agreeableness",
                              variable == "ES"          ~ "Emotional Stability")) %>%
  mutate(variable = factor(variable, levels = c("Intercept", "Experience", "Experience Sqd.",
                                                "Area: Urban", "gender: Male",
                                                "Education: Secondary", "Education: Secondary Vocational",
                                                "Education: Tertiary", "Openness", "Conscientiousness",
                                                "Extraversion", "Agreeableness", "Emotional Stability")))

extd_reg_ipw <-
  read_csv(file.path(youthOutput, "m2_ipw_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)"                     ~ "Intercept",
                              variable == "exp_imp"                         ~ "Experience",
                              variable == "I(exp_imp^2)"                    ~ "Experience Sqd.",
                              variable == "areaCity"     ~ "Area: City",
                              variable == "areaUrban-Type Settlement"      ~ "Area: Settlement",
                              variable == "areaRegional Center"            ~ "Area: Reg Center",
                              variable == "sexMale"      ~ "Sex: Male",
                              variable == "marital_status2. Married/Civil partnership" ~ "Family: Married",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Family: Divorced",
                              variable == "edu_lvl2. Secondary School"      ~ "Edu: Second",
                              variable == "edu_lvl3. Secondary Vocational"  ~ "Edu: Vocat",
                              variable == "edu_lvl4. Tertiary"              ~ "Edu: Tert",
                              variable == "O"           ~ "Openness",
                              variable == "C"           ~ "Conscientiousness",
                              variable == "E"           ~ "Extraversion",
                              variable == "A"           ~ "Agreeableness",
                              variable == "ES"          ~ "Emotional Stability")) %>%
  mutate(variable = factor(variable, levels = c("Intercept", "Experience", "Experience Sqd.",
                                                "Area: Settlement", "Area: City", "Area: Reg Center",
                                                "Sex: Male", "Family: Married", "Family: Divorced",
                                                "Edu: Second", "Edu: Vocat", "Edu: Tert", 
                                                "Openness", "Conscientiousness",
                                                "Extraversion", "Agreeableness", "Emotional Stability"))) %>%
  # we wanna add one row where variable = "region" and values for other columns are "controlled"
  add_row(variable = "Region", Q10 = "Controlled", Q25 = "Controlled", Q50 = "Controlled", Q75 = "Controlled", Q90 = "Controlled") %>%
  # we need to add model fit rows with aic and then next loglik
  #add_row(variable = "AIC", Q10 = as.character(round(m1_ipw_aic[1], 2)), Q25 = as.character(round(m1_ipw_aic[2], 2)), Q50 = as.character(round(m1_ipw_aic[3], 2)), Q75 = as.character(round(m1_ipw_aic[4], 2)), Q90 = as.character(round(m1_ipw_aic[5], 2))) %>%
  #add_row(variable = "Log Likelihood", Q10 = as.character(round(m1_ipw_loglik[1], 2)), Q25 = as.character(round(m1_ipw_loglik[2], 2)), Q50 = as.character(round(m1_ipw_loglik[3], 2)), Q75 = as.character(round(m1_ipw_loglik[4], 2)), Q90 = as.character(round(m1_ipw_loglik[5], 2))) %>%
  add_row(variable = "No. Groups", Q10 = ngrp_base, Q25 = ngrp_base, Q50 = ngrp_base, Q75 = ngrp_base, Q90 = ngrp_base) %>%
  add_row(variable = "No. Obs", Q10 = as.character(nrow_base), Q25 = as.character(nrow_base), Q50 = as.character(nrow_base), Q75 = as.character(nrow_base), Q90 = as.character(nrow_base)) 



# View(extd_reg_ipw)

### Figure 1 NCS by Edu Level ####


# returns to NCS by education level
extd_reg_ncs_edu <- 
  read_csv(file.path(youthOutput, "m4_coefs_edu_ipw.csv")) %>%
  # filter only NCS in variable
  filter(!str_detect(variable, "area")) %>%
  filter(str_detect(variable, "O|C|E|A|ES")) %>%
  # remove from the variable everything after capital letters
  mutate(variable = str_extract(variable, "[A-Z]+")) %>%
  # mutate by changing the NCS values in variable
  mutate(variable = case_when(variable == "O" ~ "Openness",
                              variable == "C" ~ "Conscientiousness",
                              variable == "E" ~ "Extraversion",
                              variable == "A" ~ "Agreeableness",
                              variable == "ES" ~ "Emotional Stability")) %>%
  drop_na() 

names(extd_reg_ncs_edu) <- c(new_names, "model")

# View(extd_reg_ncs_edu)

# Assuming your data is stored in a dataframe called `data`
edu_models_long <- 
  extd_reg_ncs_edu %>%
  pivot_longer(
    cols = starts_with("q"),       # Select all columns starting with "q"
    names_to = c("quantile", ".value"), # Split column names into 'model' and actual variable names
    names_pattern = "q(\\d+)_(.+)"  # Regex to extract quantile (q10, q25, etc.) and variable names
  ) 

# add the values to plot for the chart if the p is <0.1
edu_models_long$text = ifelse(edu_models_long$p.value < 0.1, 
                              round(edu_models_long$estimate*100, 1), as.numeric(NA))

# View(edu_models_long)

t <-
  edu_models_long %>%
  select(variable, model, quantile, estimate, p.value) %>%
  mutate_if(is.numeric, round, 3)

# View(t)

write_csv(t, file.path(youthOutput, "edu_ncs_fig.csv"))

# View(t)

fig_edu_ncs <-
  ggplot(edu_models_long, aes(x = estimate * 100, y = variable)) +
  geom_errorbarh(aes(xmin = ci.low * 100, xmax = ci.upp * 100),
                 height = 0.3) +                                        # Smaller whiskers on error bars
  geom_point(aes(fill = p.value < 0.1), shape = 21, color = "black", size = 4.5) +  # Use fill with black outline
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"),    # Use TRUE/FALSE for manual fill
                    labels = c("Insig", "Sig")) +                       # Set legend labels
  # add geom text for the variable text
  geom_text(aes(label = text), color = "white", size = 2.2) +            # Add text for significant variables
  #geom_vline(xintercept = 0, linetype = "dashed") +                     # Add vertical dashed line at 0
  facet_grid(quantile ~ model) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_text(color = "black"),                    # Set y-axis text to black
        legend.position = "bottom",                                     # Position legend at the bottom
        legend.title = element_blank())                                 # Remove legend title


### Figure 2 Returns to NCs by Gender ####

# gender_regs <-
#   read_csv(file.path(youthOutput, "m6_gender_coefs.csv")) %>%
#   mutate(text = ifelse(p.value < 0.1, estimate*100, as.numeric(NA))) %>%
#   # filter only NCS in variable
#   filter(str_detect(variable, "O|C|E|A|ES")) %>%
#   # mutate by changing the NCS values in variable
#   mutate(variable = case_when(variable == "O" ~ "Openness",
#                               variable == "C" ~ "Conscientiousness",
#                               variable == "E" ~ "Extraversion",
#                               variable == "A" ~ "Agreeableness",
#                               variable == "ES" ~ "Emotional Stability"))
# 
# 
# fig_gender_ncs <-
#   ggplot(gender_regs, aes(x = estimate * 100, y = variable)) +
#   geom_errorbarh(aes(xmin = ci.low * 100, xmax = ci.upp * 100),
#                  height = 0.3) +                                        # Smaller whiskers on error bars
#   geom_point(aes(fill = p.value < 0.1), shape = 21, color = "black", size = 4.5) +  # Use fill with black outline
#   scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"),    # Use TRUE/FALSE for manual fill
#                     labels = c("Insig", "Sig")) +                       # Set legend labels
#   # add geom text for the variable text
#   geom_text(aes(label = text), color = "white", size = 2.2) +            # Add text for significant variables
#   #geom_vline(xintercept = 0, linetype = "dashed") +                     # Add vertical dashed line at 0
#   facet_grid(quantile ~ gender) +
#   theme_bw() +
#   xlab("") +
#   ylab("") +
#   theme(axis.text.y = element_text(color = "black"),                    # Set y-axis text to black
#         legend.position = "bottom",                                     # Position legend at the bottom
#         legend.title = element_blank())                                 # Remove legend title

### Table 5 Interaction between NCS and Gender

gend_int_tab <-
  read_csv(file.path(youthOutput, "m7_ipw_gender_ncs_int.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)" ~ "Intercept",
                              variable == "exp_imp"         ~ "Experience",
                              variable == "I(exp_imp^2)"    ~ "Experience Sqd.",
                              variable == "areaCity"     ~ "Area: City",
                              variable == "areaUrban-Type Settlement"      ~ "Area: Settlement",
                              variable == "areaRegional Center"            ~ "Area: Reg Center",
                              variable == "genderMale"      ~ "Sex: Male",
                              variable == "marital_status2. Married/Civil partnership" ~ "Family: Married",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Family: Divorced",
                              variable == "O"           ~ "Openness",
                              variable == "C"           ~ "Conscientiousness",
                              variable == "E"           ~ "Extraversion",
                              variable == "A"           ~ "Agreeableness",
                              variable == "ES"          ~ "Emotional Stability",
                              variable == "genderMale:O"   ~ "Male * Openness",
                              variable == "genderMale:C"   ~ "Male * Conscientiousness",
                              variable == "genderMale:E"   ~ "Male * Extraversion",
                              variable == "genderMale:A"   ~ "Male * Agreeableness",
                              variable == "genderMale:ES"  ~ "Male * Emotional Stability")) %>%
  # we wanna add one row where variable = "region" and values for other columns are "controlled"
  add_row(variable = "Region", Q50 = "Controlled", Q75 = "Controlled", Q90 = "Controlled") %>%
  # we need to add model fit rows with aic and then next loglik
  #add_row(variable = "AIC", Q50 = as.character(round(m1_ipw_aic[3], 2)), Q75 = as.character(round(m1_ipw_aic[4], 2)), Q90 = as.character(round(m1_ipw_aic[5], 2))) %>%
  #add_row(variable = "Log Likelihood", Q50 = as.character(round(m1_ipw_loglik[3], 2)), Q75 = as.character(round(m1_ipw_loglik[4], 2)), Q90 = as.character(round(m1_ipw_loglik[5], 2))) %>%
  add_row(variable = "No. Groups", Q50 = ngrp_base, Q75 = ngrp_base, Q90 = ngrp_base) %>%
  add_row(variable = "No. Obs", Q50 = as.character(nrow_base), Q75 = as.character(nrow_base), Q90 = as.character(nrow_base)) 

# View(gend_int_tab)

### Life course models ####

# we need to generate the n obs per each age group
nrow_1665 <- as.character(nrow(ind_master_returns))
ngrp_1665 <- as.character(length(unique(ind_master_returns$idind)))

nrow_3039 <- as.character(nrow(ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ]))
ngrp_3039 <- ind_master_returns %>% filter(age >= 30 & age < 40) %>% select(idind) %>% distinct() %>% nrow() %>% as.character()

nrow_4049 <- as.character(nrow(ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ]))
ngrp_4049 <- ind_master_returns %>% filter(age >= 40 & age < 50) %>% select(idind) %>% distinct() %>% nrow() %>% as.character()

nrow_5065 <- as.character(nrow(ind_master_returns[ind_master_returns$age >= 50 & ind_master_returns$age <= 65, ]))
ngrp_5065 <- ind_master_returns %>% filter(age >= 50 & age <= 65) %>% select(idind) %>% distinct() %>% nrow() %>% as.character()

lc_models <-
  read_csv(file.path(youthOutput, "m_lc_ipw_coefs.csv")) %>%
  rename(estimate = Value,
         std.error = `Std. Error`,
         p.value = `Pr(>|t|)`) %>%
  select(variable, estimate, std.error, p.value, age_group) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q50 = paste0(round(estimate, 3), " (", round(std.error, 2), ")", p.value)) %>%
  select(variable, Q50, age_group) %>%
  mutate(variable = case_when(variable == "(Intercept)" ~ "Intercept",
                              variable == "exp_imp"         ~ "Experience",
                              variable == "I(exp_imp^2)"    ~ "Experience Sqd.",
                              variable == "areaCity"     ~ "Area: City",
                              variable == "areaUrban-Type Settlement"      ~ "Area: Settlement",
                              variable == "areaRegional Center"            ~ "Area: Reg Center",
                              variable == "sexMale"      ~ "Sex: Male",
                              variable == "marital_status2. Married/Civil partnership" ~ "Family: Married",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Family: Divorced",
                              variable == "O"           ~ "Openness",
                              variable == "C"           ~ "Conscientiousness",
                              variable == "E"           ~ "Extraversion",
                              variable == "A"           ~ "Agreeableness",
                              variable == "ES"          ~ "Emotional Stability")) %>%
  mutate(age_group = case_when(age_group == "30-40" ~ "30-39",
                               age_group == "40-50" ~ "40-49",
                               TRUE                 ~ age_group)) %>%
  pivot_wider(names_from = age_group, values_from = Q50) %>%
  # we wanna add one row where variable = "region" and values for other columns are "controlled"
  add_row(variable = "Region", `16-65` = "Controlled", `30-39` = "Controlled", `40-49` = "Controlled", `50-65` = "Controlled") %>%
  add_row(variable = "No. Groups", `16-65` = ngrp_1665, `30-39` = ngrp_3039, `40-49` = ngrp_4049, `50-65` = ngrp_5065) %>%
  add_row(variable = "No. Obs", `16-65` = nrow_1665, `30-39` = nrow_3039, `40-49` = nrow_4049, `50-65` = nrow_5065)


View(lc_models)




gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl, data = ind_master_returns)

