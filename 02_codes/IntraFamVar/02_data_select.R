# Prepare household data

hh_fam <- 
  readRDS("~/Documents/GitHub/Thesis/01_input_data/processed/hh_1994_2023_selected.rds") %>%
  mutate(year = as_factor(id_w)) %>%
  mutate(id_h = as.factor(id_h)) %>%
  filter(id_w %in% c(25, 28)) %>%
  # clean up the income data - if NA or <0, take total consumption
  mutate(hh_income = case_when(tincm_r <= 0 | is.na(tincm_r) ~ totexpr,
                               TRUE                          ~ tincm_r)) %>%
  mutate(hh_income_per_cap = hh_income/nfam) %>%
  mutate(area = as_factor(status)) %>%
  select(1:10, nfam, hh_income_per_cap, totexpr, area)

# View(hh_fam)


# Variables for non-cognitive skills
ncs_vars <- 
  c("j445_3", "j445_11", "j445_14",
    "j445_2", "j445_12", "j445_17",
    "j445_1", "j445_4",  "j445_20",
    "j445_9", "j445_16", "j445_19",
    "j445_5", "j445_10", "j445_18")

opns <- c("o1", "o2", "o3") 
cons <- c("c1", "c2", "c3")
extr <- c("e1", "e2", "e3")
aggr <- c("a1", "a2", "a3")
emst <- c("es1", "es2", "es3") 

# this is the value above which missing values are decoded
missing <- 99999996

# Read the file
# summary(factor(ind_fam$j445_11))

ind_fam1 <- 
  readRDS("~/Documents/GitHub/Thesis/01_input_data/processed/rlms_ind_sel_2007_2023.rds") %>%
  filter(id_w %in% c(25, 28)) %>%
  #filter(idind %in% c(sibling_vector25, sibling_vector28)) %>%
  mutate(birth_year = ifelse(h6 >= 99999997, NA, h6)) %>%
  mutate(sex = as_factor(h5)) %>%
  mutate(across(all_of(ncs_vars), ~ ifelse(. >= 88888888, NA, .))) 

# dim(ind_fam1) 7059
# length(unique(ind_fam1$idind)) 4804
summary(ind_fam1$j532_1)
# View(ind_fam1)

ind_fam2 <-
  ind_fam1 %>%
  # produce NCS measures
  mutate(o1 = 5 - j445_3,
         o2 = 5 - j445_11,
         o3 = 5 - j445_14,
         c1 = 5 - j445_2,
         c2 = j445_12,
         c3 = 5 - j445_17,
         e1 = 5 - j445_1,
         e2 = j445_4, 
         e3 = 5 - j445_20,
         a1 = 5 - j445_9,
         a2 = 5 - j445_16,
         a3 = 5 - j445_19,
         es1 = 5 - j445_5,
         es2 = j445_10,
         es3 = j445_18) %>%
  mutate(O  = scale(rowMeans(select(., all_of(opns)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         C  = scale(rowMeans(select(., all_of(cons)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         E  = scale(rowMeans(select(., all_of(extr)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         A  = scale(rowMeans(select(., all_of(aggr)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         ES = scale(rowMeans(select(., all_of(emst)), na.rm = T),center = TRUE, scale = TRUE)[,1]) %>%
  select(-all_of(ncs_vars), -all_of(opns), -all_of(cons), 
         -all_of(extr), -all_of(aggr), -all_of(emst)) %>%
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(O, C, E, A, ES, .direction = "downup") %>%
  ungroup() %>%
  drop_na(O, C, E, A, ES) 

# dim(ind_fam2)

summary(ind_fam2$id_w)

# summary(ind_fam2$RT_gen)
# summary(ind_fam2$RT_fin)
# summary(ind_fam2$RT_prm)
# summary(ind_fam2$RT_lab)
# summary(ind_fam2$RT_hlt)


ind_fam3 <-
  ind_fam2 %>%
  mutate(id_h = as.factor(id_h),
         idind = as.factor(idind),
         id_i = as.factor(id_i)) %>%
  full_join(fam_id) %>%
  left_join(hh_fam) %>%
  filter(age < 30) %>%
  select(idind, id_h, id_w, id_i, family_id, 
         age, sex,  n_inds, birth_year,
         # , birth_order, birth_year_diff, twins, first_born,
         h6, psu, h5, age, marst,
         # hh level variables
         hh_income_per_cap, area, region, totexpr, nfam, 
         # socio-economic outcomes
         diplom, j319, j72_5a, j72_6a, j1, j11_1, j1_1_1,
         # being religious 
         j72_18,
         #and having kids
         j72_171,
         # health
         m20_7, m20_618, m3,
         # relations with parents 
         j533_1, j533_2,
         # NCS
         O, C, E, A, ES) %>%
  # create a binary variable for the treatment group
  mutate(treatment = case_when(n_inds >= 2 ~ 1, 
                               TRUE ~ 0)) %>%
  # we need to impute relations with parents question to the previous wave
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(j533_1, j533_2, .direction = "downup") %>%
  ungroup() %>%
  mutate(h_edu = case_when(diplom %in% c(1,2,3)  ~ "1. Below Secondary",
                           diplom == 4           ~ "2. Secondary",
                           diplom == 5           ~ "3. Vocational",
                           diplom == 6           ~ "4. Higher",
                           TRUE                  ~ as.factor(NA))) %>%
  mutate(school_completed = case_when(j319 %in% c(1,2,3) ~ "2. Advanced School",
                                      TRUE               ~ "1. Other")) %>%
  mutate(employed = case_when(j1 %in% c(1,2,3,4) & j11_1 == 1 ~ "2. Officially Employed",
                              TRUE                             ~ "1. Unemployed")) %>%
  mutate(job_satisfaction = case_when(	j1_1_1 %in% c(4,5) ~ "2. Not Satisfied",
                                       TRUE                ~ "1. Other")) %>%
  mutate(obtaining_higher_edu = case_when(j72_5a %in% c(1,2) ~ "2. Obtaining or obtained",
                                          TRUE               ~ "1. Not Obtaining")) %>%
  mutate(obtaining_phd = case_when(j72_6a %in% c(1,2) ~ "2. Obtaining or obtained",
                                   TRUE                ~ "1. Not Obtaining")) %>%
  mutate(marital_status = case_when(marst %in% c(2, 3) ~ "2. Married",
                                    marst == 4         ~ "3. Divorced",
                                    TRUE               ~ "1. Not married")) %>%
  mutate(religious = case_when(j72_18 == 1 ~ "3. Very Religious",
                               j72_18 == 2 ~ "2. Rather Religious",
                               TRUE        ~ "1. Not Religious")) %>%
  mutate(having_kids = case_when(j72_171 == 1 ~ "2. Has kids",
                                 TRUE         ~ "1. No kids")) %>%
  mutate(disability = case_when(m20_7 %in% c(1,5) ~ "2. Disabled",
                                TRUE              ~ "1. Not Disabled")) %>%
  mutate(cancer = case_when(m20_618 == 1 ~ "2. Cancer",
                            TRUE         ~ "1. No Cancer")) %>%
  mutate(poor_health = case_when(m3 %in% c(4,5) ~ "2. Poor or Very Poor Health",
                                 TRUE          ~ "1. Normal or Good Health")) %>%
  mutate(relations_with_father = case_when(j533_1 == 1 ~ "2. Very close",
                                           TRUE        ~ "1. Other")) %>%
  mutate(relations_with_mother = case_when(j533_2 == 1 ~ "2. Very close",
                                           TRUE        ~ "1. Other")) %>%
  mutate(relations_with_parents = case_when(relations_with_father == "2. Very close" & relations_with_mother == "2. Very close" ~ "2. Very close",
                                            TRUE        ~ "1. Other"))

summary(factor(ind_fam3$treatment))
# View(ind_fam3)
# dim(ind_fam3)

# create imputed version of variables to use in weighting
# reoplace with median of id_w (wave) for hh_income_per_cap 
ind_fam3 <-
  ind_fam3 %>%
  group_by(id_w) %>%
  mutate(hh_inc_imp = case_when(is.na(hh_income_per_cap) ~ median(hh_income_per_cap, na.rm = T),
                                TRUE                   ~ hh_income_per_cap)) %>%
  ungroup() %>%
  # those who are NA in h_edu are still studying at school
  mutate(h_edu = case_when(is.na(h_edu) ~ "1. Below Secondary",
                           TRUE          ~ h_edu)) %>%
  mutate(sex1 = ifelse(sex == "ЖЕНСКИЙ", 0, 1)) 

# # Specify covariates to balance
# covariates <- ind_fam3[, c("sex", "age")]
# 
# # View(covariates)
# 
# # Check for NA/NaN/Inf in the covariates matrix
# sapply(covariates, function(x) sum(is.na(x) | is.nan(x) | is.infinite(x)))
# summary(ind_fam3$treatment)
# 
# # Entropy balancing
# ebal_weights <- ebalance(
#   Treatment = ind_fam3$treatment, # Treated group
#   X = covariates                  # Covariates to balance
# )
# 
# length(ind_fam3$treatment)
# dim(covariates)
# 
# # Add weights to the dataset
# data$weights <- ifelse(data$family_type == 0, ebal_weights$w, 1)



# as the data sometimes has multiple records per ind, we need to create a birth order dataset
birth_order <-
  ind_fam3 %>%
  select(idind, family_id, id_i, birth_year, sex, n_inds) %>%
  filter(n_inds > 1) %>%
  mutate(family_id = as.factor(family_id)) %>%
  distinct(idind, .keep_all = TRUE) %>%
  group_by(family_id) %>%
  arrange(birth_year) %>%
  # flag if there is a duplicate by indid
  #mutate(dup = id_i %in% id_i[duplicated(id_i)]) %>%
  mutate(birth_order = rank(birth_year, ties.method = "min")) %>%
  # check if any value in birth order duplicates and create a dummy to indiocate that
  mutate(twins = birth_order %in% birth_order[duplicated(birth_order)]) %>%
  # add as a covariate the difference in years between the first born and other siblings
  mutate(birth_year_diff = birth_year - first(birth_year)) %>%
  # create a firstborn dummy 
  mutate(first_born = ifelse(birth_order == 1, 1, 0)) %>%
  # also count how many siblings are in the family
  mutate(n_siblings = n()) %>%
  mutate(sex_male = ifelse(sex == "ЖЕНСКИЙ", 0, 1)) %>%
  mutate(male_siblings_perc = mean(sex_male)) %>%
  ungroup() 

# View(birth_order)

summary(factor(ind_fam3$m20_618))
summary(factor(ind_fam3$j533))

# This is to create the full sample that needs to be balanced using enthropy weights
ind_fam_full <-
  ind_fam3 %>%
  inner_join(birth_order) 

dim(ind_fam_full)

# View(ind_fam_full)

sample_data <-
  ind_fam_full %>%
  select(sex, age, h_edu, 
         hh_inc_imp, area, nfam, n_inds, employed, 
         poor_health, disability, 
         O, C, E, A, ES)


