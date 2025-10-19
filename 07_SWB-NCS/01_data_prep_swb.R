### Merge Household and Individual Data 

hh_swb <- 
  readRDS("~/Documents/GitHub/Thesis/01_input_data/processed/hh_1994_2023_selected.rds") %>%
  mutate(year = as_factor(id_w)) %>%
  filter(id_w >= 16 & id_w <= 30) %>%
  # clean up the income data - if NA or <0, take total consumption
  mutate(hh_income = case_when(tincm_r <= 0 | is.na(tincm_r) ~ totexpr,
                               TRUE                          ~ tincm_r)) %>%
  mutate(hh_income_per_cap = hh_income/nfam) %>%
  select(1:12, nfam, hh_income_per_cap, totexpr)

# View(hh_swb)

ind_swb <-
  readRDS("~/Documents/GitHub/Thesis/01_input_data/processed/rlms_ind_sel_2007_2023.rds") %>%
  filter(id_w >= 16 & id_w <= 30) %>%
  filter(age >= 18 & age < 60) %>%
  mutate(swb = ifelse(j65 >= 99999997, NA, j65)) %>%
  # create binary variable
  mutate(swb_binary1 = ifelse(swb %in% c(1,2), 1, 0)) %>%
  mutate(swb_binary2 = case_when(swb %in% c(1,2) ~ 1, TRUE ~ 0)) %>%
  mutate(swb = 6 - swb) %>%
  mutate(sex = as_factor(h5)) %>%
  mutate(area = as_factor(status))

summary(ind_swb$swb)
summary(ind_swb$id_w)
dim(ind_swb)
 
# now we need to calculate the mean and SD of the 16th wave 
#to see all the changes in SWB in relation to the baseline of 2007

swb_u_2007  <- mean(ind_swb$swb[ind_swb$id_w == 16], na.rm = T)
swb_sd_2007 <- sd(ind_swb$swb[ind_swb$id_w == 16], na.rm = T)

swb1 <-
  ind_swb %>%
  mutate(swb_std = (swb - swb_u_2007)/swb_sd_2007) %>%
  mutate(h_edu = case_when(diplom %in% c(1,2,3)  ~ "1. Below Secondary",
                           diplom == 4           ~ "2. Secondary",
                           diplom == 5           ~ "3. Vocational",
                           diplom == 6           ~ "4. Higher",
                           TRUE                  ~ as.factor(NA))) %>%
  left_join(hh_swb) %>%
  mutate(idind = as.factor(idind),
         wave = as.factor(id_w)) %>%
  mutate(region = as_factor(region)) %>%
  select(idind, id_w, wave, psu,
         region, area, m3, marst, j1, j11_1,
         age, sex, h_edu, 
         swb, swb_std, 
         swb_binary1, swb_binary2,
         hh_income_per_cap, 
         totexpr, nfam) %>%
  drop_na(hh_income_per_cap) %>%
  filter(hh_income_per_cap > 0) %>%
  mutate(time = id_w - 16) %>%
  mutate(poor_health = case_when(m3 %in% c(4,5) ~ "2. Poor or Very Poor Health",
                                 TRUE           ~ "1. Normal or Good Health")) %>%
  mutate(employed = case_when(j1 %in% c(1,2,3,4) & j11_1 == 1 ~ "2. Officially Employed",
                              TRUE                             ~ "1. Unemployed")) %>%
  mutate(marital_status = case_when(marst %in% c(2, 3) ~ "2. Married",
                                    marst == 4         ~ "3. Divorced",
                                    TRUE               ~ "1. Not married")) %>%
  # let's calculate median income per year
  group_by(wave) %>%
  mutate(median_inc = median(hh_income_per_cap, na.rm = T)) %>%
  ungroup() 
  
# View(swb1)5
  
### create a model of reference income 
ols_ref_inc <- lm(log(hh_income_per_cap) ~ h_edu + age + I(age) + 
                    sex  + area + factor(time)-1, data = swb1)
# summary(ols_ref_inc)

# predict the expected income for each individual and assign the exp of it to dataset
swb1$hh_inc_expected <- exp(predict(ols_ref_inc, newdata = swb1))

# View(swb1)

summary(swb1$hh_inc_expected)

swb <- 
  swb1 %>%
  mutate(ref_inc_ratio = hh_income_per_cap/hh_inc_expected, 
         income_above = ifelse(ref_inc_ratio > 1, 1, 0)) %>%
  # we need to account for poverty and richness effects
  group_by(idind) %>%
  # calculate a dummy if the hh_income_per_cap is below 60% of the median
  mutate(poor = ifelse(hh_income_per_cap < 0.6 * median_inc, 1, 0)) %>%
  # calculate a dummy if the hh_income_per_cap is above 160% of the median
  mutate(rich = ifelse(hh_income_per_cap > 1.6 * median_inc, 1, 0)) %>%
  # calculate duration of being in both, ie sum of being rich and sum of being poor separately
  mutate(poor_dur = cumsum(poor), 
         rich_dur = cumsum(rich)) %>%
  # now lets create a categorical 
  mutate(poor_1 = ifelse(poor_dur == 1, 1, 0),
         poor_2 = ifelse(poor_dur == 2, 1, 0),
         poor_3 = ifelse(poor_dur == 3, 1, 0),
         poor_4 = ifelse(poor_dur == 4, 1, 0),
         poor_5 = ifelse(poor_dur == 5, 1, 0),
         poor_6 = ifelse(poor_dur == 6, 1, 0),
         poor_7 = ifelse(poor_dur == 7, 1, 0),
         poor_8 = ifelse(poor_dur == 8, 1, 0),
         poor_9 = ifelse(poor_dur == 9, 1, 0),
         # in a way to account for 10 or more years of being poor
         poor_10 = ifelse(poor_dur >= 10, 1, 0),
         # poor_11 = ifelse(poor_dur == 11, 1, 0),
         # poor_12 = ifelse(poor_dur == 12, 1, 0),
         # poor_13 = ifelse(poor_dur == 13, 1, 0),
         # poor_14 = ifelse(poor_dur == 14, 1, 0),
         # poor_15 = ifelse(poor_dur == 15, 1, 0),
         # poor_16 = ifelse(poor_dur == 16, 1, 0),
         # poor_17 = ifelse(poor_dur == 17, 1, 0),
         rich_1 = ifelse(rich_dur == 1, 1, 0),
         rich_2 = ifelse(rich_dur == 2, 1, 0),
         rich_3 = ifelse(rich_dur == 3, 1, 0),
         rich_4 = ifelse(rich_dur == 4, 1, 0),
         rich_5 = ifelse(rich_dur == 5, 1, 0),
         rich_6 = ifelse(rich_dur == 6, 1, 0),
         rich_7 = ifelse(rich_dur == 7, 1, 0),
         rich_8 = ifelse(rich_dur == 8, 1, 0),
         rich_9 = ifelse(rich_dur == 9, 1, 0),
         # in a way to account for 10 or more years of being rich
         rich_10 = ifelse(rich_dur >= 10, 1, 0)#,
         # rich_11 = ifelse(rich_dur == 11, 1, 0),
         # rich_12 = ifelse(rich_dur == 12, 1, 0),
         # rich_13 = ifelse(rich_dur == 13, 1, 0),
         # rich_14 = ifelse(rich_dur == 14, 1, 0),
         # rich_15 = ifelse(rich_dur == 15, 1, 0),
         # rich_16 = ifelse(rich_dur == 16, 1, 0),
         # rich_17 = ifelse(rich_dur == 17, 1, 0)
         ) %>%
  ungroup() %>%
  group_by(psu) %>%
  mutate(ref_inc_psu = mean(hh_income_per_cap, na.rm = T)) %>%
  ungroup()

View(swb)

# We wanna create a descriptive table of N of each variable that starts with "poor" or "rich"
panel_rich_poor <-
  swb %>%
  select(#idind, id_w, 
         starts_with("poor"), starts_with("rich")) %>%
  select(-c(poor, poor_health, poor_dur, rich, rich_dur)) %>%
  summarise_all(sum) %>%
  # pivot longer the whole data
  pivot_longer(cols = contains("_"),
               names_to = "status",
               values_to = "n_obs") %>%
  # create a variable for the number of years by extracting the string after "_" from the status
  mutate(years = str_extract(status, "(?<=_)[0-9]+"),
         # remove from the status the numeric symbol and _
         status = str_remove(status, "_[0-9]+")) %>%
  pivot_wider(names_from = status, values_from = n_obs) %>%
  mutate(years = ifelse(years == "10", "10 or more", years)) 

summary(swb$ref_inc_psu)
View(panel_rich_poor)

# create a table of swb by year
descr <-
  swb %>%
  mutate(year = as_factor(id_w)) %>%
  group_by(year) %>%
  summarise(min = min(swb_std, na.rm = T),
            mean_swb = mean(swb_std, na.rm = T),
            median = median(swb_std, na.rm = T),
            sd_swb = sd(swb_std, na.rm = T),
            max = max(swb_std, na.rm = T),
            n = n()) %>%
  mutate_if(is.numeric, round, 3) %>%
  ungroup()

gt(descr)

### create the same lme4 model
mlm_swb_poor <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area + region +
                  log(hh_income_per_cap) +
                  poor_1 + poor_2 + poor_3 + poor_4 + poor_5 +
                  poor_6 + poor_7 + poor_8 + poor_9 + poor_10 +
                  #poor_11 + poor_12 + poor_13 + poor_14 + poor_15 + 
                  (1 + time|idind), 
                control = lmerControl(optimizer = "bobyqa"),
                data = swb)

summary(mlm_swb_poor)
report(mlm_swb_poor)

tab_model(mlm_swb_poor, rm.terms = "region")

mlm_swb_poor_binary <- 
  lmer(swb_binary1 ~ age + I(age^2) + sex + h_edu  + area + region +
                       log(hh_income_per_cap) +
                       poor_1 + poor_2 + poor_3 + poor_4 + poor_5 +
                       poor_6 + poor_7 + poor_8 + poor_9 + poor_10 +
                       #poor_11 + poor_12 + poor_13 + poor_14 + poor_15 + 
                       (1 + time|idind), 
                     control = lmerControl(optimizer = "bobyqa"),
                     data = swb)

summary(mlm_swb_poor_binary)
tab_model(mlm_swb_poor_binary, rm.terms = "region")

mlm_swb_rich <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area + region +
                       log(hh_income_per_cap) +
                    rich_1 + rich_2 + rich_3 + rich_4 + rich_5 +
                    rich_6 + rich_7 + rich_8 + rich_9 + rich_10 +
                  (1 + time|idind), 
                control = lmerControl(optimizer = "bobyqa"),
                data = swb)

summary(mlm_swb_rich)
report(mlm_swb_rich)

tab_model(mlm_swb_rich, rm.terms = "region")

mlm_swb_rich_binary <- 
  lmer(swb_binary1 ~ age + I(age^2) + sex + h_edu  + area + region +
                       log(hh_income_per_cap) +
                       rich_1 + rich_2 + rich_3 + rich_4 + rich_5 +
                       rich_6 + rich_7 + rich_8 + rich_9 + rich_10 +
                       (1 + time|idind), 
                     control = lmerControl(optimizer = "bobyqa"),
                     data = swb)

summary(mlm_swb_rich_binary)
tab_model(mlm_swb_rich_binary, rm.terms = "region")

# random slope of income by year
mlm_swb_income_time <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                       log(hh_income_per_cap) +
                       (1 | idind) + (1 + log(hh_income_per_cap) | time), 
                     control = lmerControl(optimizer = "bobyqa"),
                     data = swb)

summary(mlm_swb_income_time)

summary(factor(swb$rich_15))

inc_time_coefs <-
  coef(mlm_swb_income_time)$time %>%
  as.data.frame() %>%
  rownames_to_column("time") %>%
  select(time, `(Intercept)`, `log(hh_income_per_cap)`) 

### test the effect of income and check satiation

swb$idind <- as.numeric(swb$idind)
### create the same lme4 model
mlm_swb <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area + 
                  log(hh_income_per_cap) + 
                  (1 + time|idind), 
                control = lmerControl(optimizer = "bobyqa"),
                data = swb)

swb$predicted_swb <- predict(mlm_swb, newdata = swb)

ggplot(swb, aes(x = log(hh_income_per_cap), y = predicted_swb)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Log of Household Income per Capita",
    y = "Predicted Subjective Well-Being (SWB)",
    title = "Effect of Log Income on Predicted SWB"
  ) +
  theme_minimal()

ggpredict(mlm_swb, terms = c("hh_income_per_cap [all]")) +
  plot()

visualize(mlm_swb, plot = "model")

sjPlot::plot_model(mlm_swb, type = "pred", terms = c("hh_income_per_cap [all]"))

summary(mlm_swb_poor)
report(mlm_swb_poor)

### lets do a hybrid model

swb$log_inc <- log(swb$hh_income_per_cap)

# compute mean of "x_tv" for each subject (ID) and
# then "de-mean" x_tv
d <- cbind(
  swb,
  parameters::demean(swb, select = c("log_inc", "ref_inc_ratio"), by = "idind") # from package "parameters"
)

mlm_swb_rewb1 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                              log_inc_between + log_inc_within +
                              (1 + time | idind), 
                            data = d)

summary(mlm_swb_rewb1)


mlm_swb_rewb2 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                        log_inc_between + log_inc_within + ref_inc_ratio_between + ref_inc_ratio_within + 
                        (1 + time + log_inc_within + ref_inc_ratio_within | idind), 
                      control = lmerControl(optimizer = "bobyqa"),
                      data = d)

summary(mlm_swb_rewb2)


ggpredict(mlm_swb_rewb2, terms = c("log_inc_within [all]")) +
  plot()

visualize(mlm_swb_rewb2, pred = log_inc_within, modx = log_inc_within, modx.values = c(-1, 0, 1))


#### Estimate social comparison effects
mlm_swb_soc_comp1 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                       ref_inc_ratio +
                       (1 + time|idind), 
                     control = lmerControl(optimizer = "bobyqa"),
                     data = swb)

summary(mlm_swb_soc_comp1)

mlm_swb_soc_comp2 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area + log(hh_income_per_cap) +
                           ref_inc_ratio_between + ref_inc_ratio_within +
                           (1 + time|idind), 
                         control = lmerControl(optimizer = "bobyqa"),
                         data = d)

summary(mlm_swb_soc_comp2)

### add relative income through the difference between logs of observed and exprected

tab_model(mlm_swb_rich)


### Now do the model with relative and absolute income
mlm_swb_soc_comp3 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                            log(hh_inc_expected) +
                            (1 + time|idind), 
                          control = lmerControl(optimizer = "bobyqa"),
                          data = swb)

summary(mlm_swb_soc_comp3)

mlm_swb_soc_comp4 <- lmer(swb_std ~ age + I(age^2) + sex + h_edu  + area +
                            log(hh_income_per_cap) + log(hh_inc_expected) +
                            (1 + time|idind), 
                          control = lmerControl(optimizer = "bobyqa"),
                          data = swb)

summary(mlm_swb_soc_comp4)
