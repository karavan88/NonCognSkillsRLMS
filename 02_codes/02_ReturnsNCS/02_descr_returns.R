#-------------------------------------------------------------------
# Project: Returns to Non-Cognitive Skills 
# Organization: SFedU Future Skills Research Lab
# Objective: Descriptive analysis
# Author:  Garen Avanesian
# Date: 3 December 2024
#-------------------------------------------------------------------

# source(file.path("02_codes/libraries.R"))

# Table of sample summary
# sex, average age, edu level, area of residence

# Descriptive statistics
# NCS, monthly wage, experience

youth_descr <-
  youth_master_returns

summary_stats <-
  youth_master_returns %>%
  select(sex, age, area, edu_lvl, exp_imp, marital_status, year) %>%
  tbl_summary(by = year,
              type = list( age ~ 'continuous2',
                           exp_imp ~ 'continuous2',
                           c(sex, area, edu_lvl, marital_status) ~ 'categorical'),
              label = list(age ~ "Age",
                           exp_imp ~ "Experience",
                           area ~ "Area",
                           sex ~ "Sex",
                           edu_lvl ~ "Highest Level of Education",
                           marital_status ~ "Marital Status"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data")) 


youth_long <- 
  youth_descr %>%
  # create a variable NCS and add there all big five traits
  pivot_longer(cols = c(O, C, E, A, ES), 
               names_to = "NCS", values_to = "Value") %>%
  # recode NCS into full names
  mutate(NCS = case_when(
    NCS == "O" ~ "Openness",
    NCS == "C" ~ "Conscientiousness",
    NCS == "E" ~ "Extraversion",
    NCS == "A" ~ "Agreeableness",
    NCS == "ES" ~ "Emotional Stability"
  ))

# View(youth_long)

# Plot the distribution of non-cognitive skills
ncs_hist <-
  ggplot(youth_long, aes(x = Value)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ NCS, scales = "free_x") +
  labs(x = "",
       y = "Frequency") +
  theme_bw()