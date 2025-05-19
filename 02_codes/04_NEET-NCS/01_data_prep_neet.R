library(msm)

neet_data =
  ind_master_empl %>%
  filter(age >= 15 & age < 25) %>%
  filter(id_w %in% c("25", "28")) %>%
  select(id_w, idind,
         year, age, sex,
         employed, in_education, seeking_employment, 
         neet_status, neet,
         O, C, E, A, ES) %>%
  mutate(neet3 = case_when(neet_status %in% c("NEET: Inactive", "NEET: Unemployed") ~ "NEET",
                          TRUE ~ neet_status)) %>%
  mutate(neet_numeric1 = case_when(neet3 == "NEET" ~ 3,
                                  neet3 == "In Education" ~ 1,
                                  neet3 == "Employed" ~ 2)) %>%
  mutate(neet_numeric2 = case_when(neet_status == "NEET: Inactive" ~ 4,
                                  neet_status == "NEET: Unemployed" ~ 3,
                                  neet_status == "In Education" ~ 1,
                                  neet_status == "Employed" ~ 2)) %>%
  # we need to create categorical variables for each non-cognitive skill based on 25% groups
  mutate(O_cat = ntile(O,   5),
         C_cat = ntile(C,   5),
         E_cat = ntile(E,   5),
         A_cat = ntile(A,   5),
         ES_cat = ntile(ES, 5)) %>%
  # create a binary variable for EACH personality trait indicating that if it is higher than 0.5, than it is 1 and 0 otherwise
  mutate(O_binary = ifelse(O > 0.5, 1, 0),
         C_binary = ifelse(C > 0.5, 1, 0),
         E_binary = ifelse(E > 0.5, 1, 0),
         A_binary = ifelse(A > 0.5, 1, 0),
         ES_binary = ifelse(ES > 0.5, 1, 0)) %>%
  drop_na() %>%
  arrange(idind, year) %>%
  group_by(idind) %>%
  # calculate number of obs per idind
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  # index variable year with 0 and 1 as 2016 and 2019
  mutate(year_index = case_when(id_w == 25 ~ 0,
                                id_w == 28 ~ 1)) %>%
  # create age group for neet
  mutate(age_group_neet = case_when(age > 15 & age < 20 ~ "1. 15-19",
                                    age >= 20 & age < 25 ~ "2. 20-24")) 
  
  

View(neet_data)

table(neet_data$neet_status, neet_data$year)
table(neet_data$neet3, neet_data$year)


# Estimate empirical transition probabilities
prop.table(table(from = neet_data$neet_numeric2[neet_data$year == 2016],
                 to   = neet_data$neet_numeric2[neet_data$year == 2019]), 1) %>%
  round(2)

Q4 <- rbind(
  c(0,     0.23/3, 0.09/3, 0.08/3),
  c(0.12/3, 0,     0.05/3, 0.08/3),
  c(0.13/3, 0.56/3, 0,     0.13/3),
  c(0.10/3, 0.24/3, 0.08/3, 0)
)

prop.table(table(from = neet_data$neet_numeric1[neet_data$year == 2016],
                 to   = neet_data$neet_numeric1[neet_data$year == 2019]), 1) %>%
  round(2)

Q3 <- rbind(
  c(0,     0.23/3, 0.18/3),
  c(0.12/3, 0,     0.13/3),
  c(0.11/3, 0.39/3, 0)
)

msm_model <- msm(neet_numeric1 ~ year_index, 
                 subject = idind, 
                 data = neet_data,
                 qmatrix = Q3,
                 covariates = ~ sex + O_binary + C_binary + E_binary + A_binary + ES_binary,
                 control = list(fnscale = 10000, maxit = 10000, optim.method = "Nelder-Mead")
                 )

msm_model

                 