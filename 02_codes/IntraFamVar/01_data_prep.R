#-------------------------------------------------------------------
# Project: Intra-Family Variability in Non-Cognitive Skills
# Script:  RLMS 2016-2019 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    3 December 2024
#-------------------------------------------------------------------

# This script takes idind of those participated in 28th wave and if they were in 25th, it also selects their family data

# Read the file with relatives
ind_relations <- 
  read_sav("01_input_data/ind_relatives_id_i_idind_USER_RLMS-HSE_HH_1994_2022_rus.sav") %>%
  filter(id_w == 28) %>%
  mutate(idind = as.factor(idind),
         id_i = as.factor(id_i)) %>%
  select(id_w, idind, id_i, 
         # r01a1, # spouse
         # r02a1, r02a2, # parents (blood)
         # r04a1, r04a2, r04a3, r04a5, r04a6, r04a7, r04a8, r04a9, # children of respondents (blood)
         r06a1, r06a2, r06a3, r06a5, r06a6, r06a7, r06a8) 

# View(ind_relations)

library(igraph)

# Step 1: Reshape the data into long format
sibling_data_long <- 
  ind_relations %>%
  select(id_i, idind, starts_with("r06a")) %>% 
  pivot_longer(cols = starts_with("r06a"), 
               values_to = "sibling_id", 
               values_drop_na = TRUE) %>%
  mutate(id_i = factor(id_i),
         sibling_id = factor(sibling_id)) %>%
  rename(individual_id = id_i)

# View(sibling_data_long)

# Step 2: Create edges for graph (pairs of individual and sibling IDs)
edges <- 
  sibling_data_long %>%
  filter(!is.na(sibling_id)) %>%
  select(individual_id, idind, sibling_id)

# Step 3: Create a graph and identify connected components (families)
g <- 
  graph_from_data_frame(edges %>% select(individual_id, sibling_id), directed = FALSE)

family_groups <- components(g)$membership

# Step 4: Add family group ID back to the original data
family_ids <- 
  sibling_data_long %>%
  distinct(individual_id, idind) %>%
  mutate(family_id = family_groups[as.character(individual_id)]) %>%
  select(individual_id, idind, family_id)

# Merge back to the original dataset
ind_fam_with_family_id <- 
  ind_relations %>%
  left_join(family_ids, by = c("id_i" = "individual_id", "idind" = "idind")) %>%
  mutate(family_id = as.factor(family_id)) %>%
  drop_na(family_id)

# View results
# View(family_ids)
# View(ind_fam_with_family_id)

fam_id <-
  family_ids %>%
  select(idind, family_id) %>%
  group_by(family_id) %>%
  mutate(n_inds = n()) %>%
  ungroup() %>%
  mutate(family_id = as.factor(family_id))

# View(fam_id)

sibling_vector <- unique(fam_id$idind)

# Print the sibling vector
# length(sibling_vector)


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
  filter(idind %in% sibling_vector) %>%
  mutate(birth_year = ifelse(h6 >= 99999997, NA, h6)) %>%
  mutate(sex = as_factor(h5)) %>%
  mutate(across(all_of(ncs_vars), ~ ifelse(. >= 88888888, NA, .))) 

# dim(ind_fam1)
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
         ES = scale(rowMeans(select(., all_of(emst)), na.rm = T),center = TRUE, scale = TRUE)[,1]#,
         # RT_gen = scale(j446, center = T, scale = T)[,1],
         # RT_fin = scale(j447_2, center = T, scale = T)[,1],
         # RT_prm = scale(j447_3, center = T, scale = T)[,1],
         # RT_lab = scale(j447_4, center = T, scale = T)[,1],
         # RT_hlt = scale(j447_5, center = T, scale = T)[,1]
         ) %>%
  select(-all_of(ncs_vars), -all_of(opns), -all_of(cons), -all_of(extr), -all_of(aggr), -all_of(emst)) %>%
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(O, C, E, A, ES, .direction = "downup") %>%
  ungroup() %>%
  drop_na(O, C, E, A, ES, 
          #RT_gen, RT_fin, RT_prm, RT_lab, RT_hlt
          ) 

dim(ind_fam2)

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
  inner_join(fam_id) 
  
# View(ind_fam3)
# dim(ind_fam3)

# as the data sometimes has multiple records per ind, we need to create a birth order dataset
birth_order <-
  ind_fam3 %>%
  select(idind, family_id, id_i, birth_year) %>%
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
  ungroup() 

# View(birth_order)

summary(factor(ind_fam3$m20_618))
summary(factor(ind_fam3$j533))

ind_fam <-
  ind_fam3 %>%
  left_join(birth_order) %>%
  select(idind, id_h, id_w, id_i, family_id, 
         age, sex, birth_year, birth_order, birth_year_diff, twins, first_born, n_inds, n_siblings,
         h6, region, status, psu, h5, age, marst,
         # socio-economic outcomes
         diplom, j319, j72_5a, j72_6a, j1, j11_1, 
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
         #RT_gen, RT_fin, RT_prm, RT_lab, RT_hlt,
  filter(age >= 15 & age < 30) %>%
  # we need to impute relations with parents question to the previous wave
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(j533_1, j533_2, .direction = "downup") %>%
  ungroup() %>%
  # make necessary conversions to factors
  # mutate(j72_171 = as_factor(j72_171)) %>%
  # mutate(m20_7 = as_factor(m20_7)) %>%
  # mutate(j72_18 = as_factor(j72_18)) %>%
  mutate(h_edu = case_when(diplom %in% c(1,2,3)  ~ "1. Below Secondary",
                           diplom == 4           ~ "2. Secondary",
                           diplom == 5           ~ "3. Vocational",
                           diplom == 6           ~ "4. Higher",
                           TRUE                  ~ as.factor(NA))) %>%
  mutate(school_completed = case_when(j319 %in% c(1,2,3) ~ "2. Advanced School",
                                      TRUE               ~ "1. Other")) %>%
  mutate(employed = case_when(j1 %in% c(1,2,3,4) & j11_1 == 1 ~ "2. Officially Employed",
                             TRUE                             ~ "1. Unemployed")) %>%
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
    
    
  
summary(factor(ind_fam$relations_with_parents))
summary(factor(ind_fam$religious))
summary(factor(ind_fam$marital_status))
summary(factor(ind_fam$having_kids))



# View(ind_fam)
dim(ind_fam)

summary(ind_fam$age)

# View(test_data)


### factors that vary within the household
# sex, age, education achieved, current edu attendance, marital status, employment status,

summary(ind_fam$age)

# Baseline model is lme4 that basically estimates variation in each 5 traits (5 models) due to id_h ind indid
# Then we can add covariates to the model
m_opns_base <- lmer(O  ~ 1 + (1|family_id) + (1|idind), data = ind_fam)
m_cons_base <- lmer(C  ~ 1 + (1|family_id) + (1|idind), data = ind_fam)
m_extr_base <- lmer(E  ~ 1 + (1|family_id) + (1|idind), data = ind_fam)
m_aggr_base <- lmer(A  ~ 1 + (1|family_id) + (1|idind), data = ind_fam)
m_emst_base <- lmer(ES ~ 1 + (1|family_id) + (1|idind), data = ind_fam)

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


m_opns_cov <- lmer(O  ~ age + I(age^2) + sex + first_born + (1|family_id) + (1|idind), data = ind_fam)
m_cons_cov <- lmer(C  ~ age + I(age^2) + sex + first_born + (1|family_id) + (1|idind), data = ind_fam)
m_extr_cov <- lmer(E  ~ age + I(age^2) + sex + first_born + (1|family_id) + (1|idind), data = ind_fam)
m_aggr_cov <- lmer(A  ~ age + I(age^2) + sex + first_born + (1|family_id) + (1|idind), data = ind_fam)
m_emst_cov <- lmer(ES ~ age + I(age^2) + sex + first_born + (1|family_id) + (1|idind), data = ind_fam)

common_formula <- 
  O ~ age + I(age^2) + sex + first_born + 
  relations_with_father + 
  (1 | family_id) + (1 | idind)

m_opns_cov <- lmer(update(common_formula, O ~ .), data = ind_fam)
m_cons_cov <- lmer(update(common_formula, C ~ .), data = ind_fam)
m_extr_cov <- lmer(update(common_formula, E ~ .), data = ind_fam)
m_aggr_cov <- lmer(update(common_formula, A ~ .), data = ind_fam)
m_emst_cov <- lmer(update(common_formula, ES ~ .), data = ind_fam)

summary(m_opns_cov)
summary(m_cons_cov)
summary(m_extr_cov)
summary(m_aggr_cov)
summary(m_emst_cov)

soc_dem_models <- list("Openness" = m_opns_cov, 
                       "Conscientiousness" = m_cons_cov, 
                       "Extraversion" = m_extr_cov, 
                       "Agreeableness" = m_aggr_cov, 
                       "Emotional Stability" = m_emst_cov)

#### Socio-Economic Outcomes Model

socecon_formula <- 
  O ~ age + I(age^2) + sex + 
  h_edu +  employed + 
  marital_status + having_kids + poor_health + 
  (1 | family_id) + (1 | idind)

m_opns_socecon <- lmer(update(socecon_formula, O ~ .), data = ind_fam)
m_cons_socecon <- lmer(update(socecon_formula, C ~ .), data = ind_fam)
m_extr_socecon <- lmer(update(socecon_formula, E ~ .), data = ind_fam)
m_aggr_socecon <- lmer(update(socecon_formula, A ~ .), data = ind_fam)
m_emst_socecon <- lmer(update(socecon_formula, ES ~ .), data = ind_fam)

summary(m_opns_socecon)
summary(m_cons_socecon)
summary(m_extr_socecon)
summary(m_aggr_socecon)
summary(m_emst_socecon)

report_model(m_opns_socecon)

