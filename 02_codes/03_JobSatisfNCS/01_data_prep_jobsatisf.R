#-------------------------------------------------------------------
# Project: NCS and Job Satisfaction
# Script:  RLMS 2016-2019 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    15 December 2024
#-------------------------------------------------------------------

# We also need to adjust the wages for 2016 to the 2019 level
# https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=RU
cpi_2016 <- 162.2
cpi_2019 <- 180.8

adj_factor <- cpi_2019/cpi_2016

youth_job_satisf <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  filter(employed == 1) %>%
  select(idind, id_w, year, ipw_empl, wave,
         age, sex, region, area, edu_lvl, 
         exp_imp, wages,
         # tenure year and month
         j5a, j5b,
         # employees/subordinates
         j6, j6_0, 
         # working over time or over the weekend
         working_overtime, working_weekends,
         # company ownership
         j23, j26,
         # labor amount (to derive an hourly wage)
         j6_1, j6_1a, j6_1b, j6_2,
         occupation, industry,
         starts_with("satisf"), 
         # original job satisfaction measures
         j1_1_1:j1_1_10,
         O, C, E, A, ES) %>%
  # if wave is 25, then we need to adjust the wages
  #mutate(wages = ifelse(id_w == "25", wages * adj_factor, wages)) %>%
  drop_na(O, C, E, A, ES, satisf_job) %>%
  mutate(work_hrs_per_week = ifelse(is.na(j6_2), 40, j6_2)) %>%
  mutate(work_over_40hrs = ifelse(work_hrs_per_week > 40, 1, 0)) %>%
  mutate(occupation = case_when(occupation == "0" ~ "0. Military",
                                occupation == "1" ~ "1. Managers",
                                occupation == "2" ~ "2. Professionals",
                                occupation == "3" ~ "3. Associate Professionals",
                                occupation == "4" ~ "4. Clerical Workers",
                                occupation == "5" ~ "5. Service/Sales Workers",
                                occupation == "6" ~ "6. Skilled Agricult Workers",
                                occupation == "7" ~ "7. Craft/Trades Workers",
                                occupation == "8" ~ "8. Plant/Machine Operators",
                                occupation == "9" ~ "9. Elementary Occupations"
                                )) 

summary(youth_job_satisf$age)


# we need to carry some imputation for the wages

# Split the dataset by `id_w`
data_split_js <- 
  split(youth_job_satisf, youth_job_satisf$wave)

# Apply the loess model and extract predicted values
predicted_data_job_satisf <- 
  lapply(data_split_js, function(subset) {
    
    # Fit loess model for each subset
    wages_model <- lm(log(wages) ~ exp_imp + I(exp_imp^2) + edu_lvl + 
                        sex + region + area +
                        O + C + E + A + ES, data = subset)
    
    # Add predicted values to the subset
    subset$wages_pred <- exp(predict(wages_model, newdata = subset))
    
    return(subset)
  })

youth_job_satisf <- bind_rows(predicted_data_job_satisf)

# check the correlation between observed and predicted via plot
wages_corr <-
  ggplot(youth_job_satisf, aes(x = wages, y = wages_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Wages vs Wages Experience",
       x = "Wages",
       y = "Predicted Wages") +
  xlim(0, 60000) +
  facet_wrap(~id_w) +
  theme_minimal()

summary(youth_job_satisf$wages_pred)

# replace NAs of exp with the predicted values from loess
youth_job_satisf$wages_imp <- 
  ifelse(is.na(youth_job_satisf$wages), youth_job_satisf$wages_pred, 
         youth_job_satisf$wages)

# derive hourly wage and hourly wage quantile
youth_job_satisf <- 
  youth_job_satisf %>%
  mutate(hourly_wage = wages_imp/work_hrs_per_week,
         hourly_wage_quantile = percent_rank(hourly_wage),
         hourly_wage_quintile = case_when(hourly_wage_quantile <= 0.2 ~ "Q1",
                                          hourly_wage_quantile <= 0.4 ~ "Q2",
                                          hourly_wage_quantile <= 0.6 ~ "Q3",
                                          hourly_wage_quantile <= 0.8 ~ "Q4",
                                          TRUE                        ~ "Q5" )) %>%
  mutate(satisf_wbl = ifelse(id_w == "28", satisf_wbl, as.numeric(NA))) 

# View(youth_job_satisf)

dim(youth_job_satisf)
summary(factor(youth_job_satisf$satisf_job))
summary(factor(youth_job_satisf$satisf_wbl))
summary(youth_job_satisf$age)
summary(factor(youth_job_satisf$edu_lvl))
summary(youth_job_satisf$ipw_empl)
summary(youth_job_satisf$occupation) # 11 NAs
summary(youth_job_satisf$industry) # 15 NAs
summary(youth_job_satisf$work_hrs_per_week)

summary(factor(youth_job_satisf$j1_1_1))
summary(factor(youth_job_satisf$j1_1_2))
summary(factor(youth_job_satisf$j1_1_3))
summary(factor(youth_job_satisf$j1_1_4))

# view_df(youth_job_satisf)

# histogram of working per week j6_2
hist_work_per_week <-
  ggplot(youth_job_satisf, aes(x = j6_2)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Working Hours per Week",
       x = "Working Hours",
       y = "Frequency") +
  ylim(0, 7) +
  xlim(35, 170) +
  theme_minimal()

overworkers <-
  youth_job_satisf %>%
  filter(work_hrs_per_week > 40) 



