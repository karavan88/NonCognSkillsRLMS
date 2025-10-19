#-------------------------------------------------------------------
# Project: Returns to NCS
# Script:  RLMS 2016-2019 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    1 December 2024
#-------------------------------------------------------------------

# We also need to adjust the wages for 2016 to the 2019 level
# https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=RU
cpi_2016 <- 162.2
cpi_2019 <- 180.8

adj_factor <- cpi_2019/cpi_2016

# saveRDS(ind_master_empl, file.path(processedData, "ind_master_empl.rds"))
ind_master_returns <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 16 & age < 66) %>%
  mutate(wages_adj = ifelse(id_w == 25, wages * adj_factor, wages)) %>%
  mutate(work_hrs_per_week = ifelse(is.na(j6_2), 40, j6_2)) %>%
  mutate(working_hrs_per_month = work_hrs_per_week * 4) %>%
  mutate(hourly_wage = wages_adj/working_hrs_per_month) %>%
  mutate(log_wage = log(hourly_wage)) %>%
  drop_na(wages, O, C, E, A, ES)

youth_master_returns <-
  ind_master_returns %>%
  filter(age >= 16 & age < 30) 

saveRDS(youth_master_returns, file.path(rCodes, "02_ReturnsNCS/outputs/youth_master_returns.rds"))


  

# 
# 
# 
# 
# 
# 
# 
# 
# missing <- 99999996
# missing_vars <- c("wages", "j6_2", "j10", "j13_2")
# 
# # Variables for non-cognitive skills
# ncs_vars <- 
#   c("j445_3", "j445_11", "j445_14",
#     "j445_2", "j445_12", "j445_17",
#     "j445_1", "j445_4",  "j445_20",
#     "j445_9", "j445_16", "j445_19",
#     "j445_5", "j445_10", "j445_18")
# 
# openness <- c("o1", "o2", "o3") 
# con <- c("c1", "c2", "c3")
# ex <- c("e1", "e2", "e3")
# ag <- c("a1", "a2", "a3")
# em_st <- c("es1", "es2", "es3") 
# grit <- c("g1", "g2", "g3")
# 
# ind_2016_2019 <- 
#   readRDS("01_input_data/processed/rlms_ind_sel_2007_2023.rds") %>%
#   filter(id_w %in% c(25, 28)) %>%
#   filter(age >= 16 & age < 66) %>%
#   select(# id vars
#     id_w, idind, redid_i, id_i, id_h, int_y, h3, 
#     # sample design
#     inwgt, psu, site, status, popul, region,
#     # interview dates
#     h7_1, h7_2, 
#     # personal info
#     age, h5, h6, marst,
#     # education 
#     educ, diplom,
#     # employment, occupation, and wages
#     j1, occup08,  j2cod08, j4_1, j11_1, 
#     # wages 
#     j13_2, j10,  
#     # years/months of experience
#     experience, 
#     # labor amount (to derive an hourly wage)
#     j6_1a, j6_1b, j6_2,
#     ncs_vars) %>%
#   mutate(year = as.numeric(int_y), 
#          area = as_factor(status),
#          idind = as.numeric(idind),
#          occupation = as_factor(occup08),
#          region = as_factor(region),
#          gender = ifelse(h5 == 1, "Male", "Female"),
#          edu_lvl = case_when(diplom %in% c(1,2,3) ~ "1. No school",
#                              diplom == 4          ~ "2. Secondary School",
#                              diplom == 5          ~ "3. Secondary Vocational",
#                              diplom == 6          ~ "4. Tertiary"),
#          working  = ifelse(j1 %in% c(1:4), 1, 0), 
#          family_id = h3, 
#          wages = j10) %>%
#   mutate(across(all_of(ncs_vars), ~ ifelse(. >= 88888888, NA, .))) %>%
#   # if any of the variables in the dataset is more than or equal to the value in missing vector, replace with NA
#   mutate(across(all_of(missing_vars), ~ ifelse(. >= missing, NA, .))) %>%
#   # if wave is 25, then we need to adjust the wages
#   mutate(wages = ifelse(id_w == 25, wages * adj_factor, wages)) %>%
#   # derive hourly wage
#   # if working ==1 and j6_2 (hours oer week) is NA, then insert 40 hours as a standard working week
#   mutate(j6_2 = ifelse(working == 1 & is.na(j6_2), 40, j6_2)) %>%
#   mutate(working_per_month = j6_2 * 4, # 4 weeks in a month multiplied by number of working hours per week
#          hourly_wage = wages/working_per_month) %>%
#   # we also need to adjust NAs in edu_lvl making sure that kids are not assigned NAs
#   mutate(edu_lvl = case_when(is.na(edu_lvl) ~ "1. No school", 
#                              TRUE ~ edu_lvl)) %>%
#   # produce NCS measures
#   mutate(o1 = 5 - j445_3,
#          o2 = 5 - j445_11,
#          o3 = 5 - j445_14,
#          c1 = 5 - j445_2,
#          c2 = j445_12,
#          c3 = 5 - j445_17,
#          e1 = 5 - j445_1,
#          e2 = j445_4, 
#          e3 = 5 - j445_20,
#          a1 = 5 - j445_9,
#          a2 = 5 - j445_16,
#          a3 = 5 - j445_19,
#          es1 = 5 - j445_5,
#          es2 = j445_10,
#          es3 = j445_18) %>%
#   mutate(O = scale(rowMeans(select(., all_of(openness)), na.rm = T),center = TRUE, scale = TRUE)[,1],
#          C = scale(rowMeans(select(., all_of(con)), na.rm = T),center = TRUE, scale = TRUE)[,1],
#          E = scale(rowMeans(select(., all_of(ex)), na.rm = T),center = TRUE, scale = TRUE)[,1],
#          A = scale(rowMeans(select(., all_of(ag)), na.rm = T),center = TRUE, scale = TRUE)[,1],
#          ES = scale(rowMeans(select(., all_of(em_st)), na.rm = T),center = TRUE, scale = TRUE)[,1]) %>%
#   select(-all_of(ncs_vars), -all_of(openness), -all_of(con), -all_of(ex), -all_of(ag), -all_of(em_st)) %>%
#   group_by(idind) %>%
#   arrange(id_w) %>%
#   fill(O, C, E, A, ES, .direction = "downup") %>%
#   ungroup() 
# 
# # View(ind_2016_2019)
# 
# # explore wages
# summary(ind_2016_2019$wages)
# summary(ind_2016_2019$j10)
# summary(ind_2016_2019$j13_2)
# 
# # create a scatter plot
# ggplot(ind_2016_2019, aes(x = j10, y = j13_2)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") +
#   labs(title = "Wages vs Hourly Wage",
#        x = "Wages1",
#        y = "Wages2") +
#   theme_minimal()
# 
# # Split the dataset by `id_w`
# data_split <- 
#   split(ind_2016_2019, ind_2016_2019$id_w)
# 
# # Apply the loess model and extract predicted values
# predicted_data <- 
#   lapply(data_split, function(subset) {
# 
#     # Fit loess model for each subset
#     exp_model <- loess(experience ~ age, data = subset)
#     
#     # Calculate inverse probability weight to control for the self-selection into employment
#     ipw_model <- weightit(working ~ age + gender + edu_lvl + region + area ,
#                           data = subset,
#                           method = "ps",  # Propensity score (logit model)
#                           estimand = "ATE")  # Average treatment effect
#     
#     # Add predicted values to the subset
#     subset$exp_pred <- predict(exp_model, newdata = subset)
#     subset$ipw <- ipw_model$weights
#     
#     return(subset)
# })
# 
# # Combine the data back into a single dataset
# ind_2016_2019 <- bind_rows(predicted_data)
# 
# summary(ind_2016_2019$ipw)
# summary(ind_2016_2019$exp_pred)
# 
# # check the correlation between observed and predicted via plot
# ggplot(ind_2016_2019, aes(x = experience, y = exp_pred)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") +
#   labs(title = "Experience vs Predicted Experience",
#        x = "Experience",
#        y = "Predicted Experience") +
#   facet_wrap(~id_w) +
#   theme_minimal()
# 
# 
# # experience cannot be below 0, so all the negative values should be 0
# ind_2016_2019$exp_pred <- ifelse(ind_2016_2019$exp_pred < 0, 0, ind_2016_2019$exp_pred)
# 
# # replace NAs of exp with the predicted values from loess
# ind_2016_2019$exp_imp <- 
#   ifelse(is.na(ind_2016_2019$experience), ind_2016_2019$exp_pred, 
#          ind_2016_2019$experience)
# 
# # just to check to which quantile does the value of 10 as a threshold belongs
# percentile <- quantile(ind_2016_2019$ipw, 0.99)
# ind_2016_2019$ipw <- ifelse(ind_2016_2019$ipw > percentile, percentile, ind_2016_2019$ipw)
# 
# # View(ind_2016_2019)
# 
# 
# 
# # merge the NCS data with the individual data
# master <- 
#   ind_2016_2019 %>%
#   select(# id vars
#     id_w, idind, redid_i, id_i, id_h, year, family_id, 
#     # sample design
#     inwgt, psu, site, area, popul, region,
#     # interview dates
#     h7_1, h7_2, 
#     # personal info
#     age, gender, h6, marst,
#     # education 
#     educ, diplom, edu_lvl, 
#     # occupation
#     occupation, working, j1, wages, working_per_month, hourly_wage, exp_imp,
#     O, C, E, A, ES, ipw) %>%
#   mutate(edu_lvl = factor(edu_lvl),
#          gender = factor(gender)) %>%
#   drop_na(wages, O, C, E, A, ES) %>%
#   mutate(log_wage = log(hourly_wage + 1)) %>%
#   mutate(marital_status = case_when(marst %in% c(2, 3) ~ "2. Married/Civil partnership",
#                                     marst %in% c(4, 5, 6) ~ "3. Divorced/Separated/Widowed",
#                                     # let's treat some refusals etc as not married/base
#                                     TRUE ~ "1. Single")) 
# 
# master$area <- factor(master$area, levels = c("Село", "ПГТ", "Город", "Областной центр"))
# master$marital_status <- factor(master$marital_status, levels = c("1. Single", 
#                                                                   "2. Married/Civil partnership", 
#                                                                   "3. Divorced/Separated/Widowed"))
# # recode area variable
# master$area <- recode(master$area, 
#                       "Село" = "Rural", 
#                       "ПГТ" = "Urban-Type Settlement", 
#                       "Город" = "City", 
#                       "Областной центр" = "Regional Center")
# summary(master$area)
# summary(master$age) # min is 16 max is 65.5
# summary(factor(master$marst))
# 
# # View(master)
#     
# summary(master)
# dim(master)
# 
# table(master$occupation, master$edu_lvl)
# 
# youth_master <- 
#   master %>%
#   filter(age >= 16 & age < 30) %>%
#   mutate(log_wage = log(hourly_wage)) 
# 
# dim(youth_master)
# 
# 
# inds_ncs <- unique(youth_master$idind)
# length(inds_ncs)
# 
