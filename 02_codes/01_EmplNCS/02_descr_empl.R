#-------------------------------------------------------------------
# Project: Non-Cognitive Skills and Employment
# Script:  Descriptive Statistics of Employment by NCS
# Author:  Garen Avanesian
# Date:    17 December 2024
#-------------------------------------------------------------------


youth_empl <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, edu_lvl, region, sex, employed, year,
         in_education, hh_inc_quintile, area_binary, area, occupation, 
         # empl_offic, 
         employed_officially,
         self_employed, satisf_job, j1_1_1, 
         O, C, E, A, ES) %>%
  # if NA in edu_lvv that assign No School - this will fill 4 observations
  mutate(edu_lvl = ifelse(is.na(edu_lvl), "1. No school", edu_lvl),
         employed_officially = ifelse(is.na(employed_officially), 0, employed_officially)) %>%
  # fix NAs in self-employed
  mutate(self_employed = ifelse(is.na(self_employed), 0, self_employed)) %>%
  # create transition_successful variable
  # should be OFFICIALLy self-employed and satisfied with the job
  mutate(self_empl_offic_and_saisf = case_when(self_employed == 1 & j1_1_1 == 1 & employed_officially == 1 ~ 1,
                                               TRUE ~ 0),
         # exclude self-employed from those employed officially as successfully transitioned
         transition_successful1 = case_when(self_employed == 1 ~ 0,
                                            TRUE ~ employed_officially)) %>%
  # create a final transition successful variable
  mutate(transition_successful = case_when(transition_successful1 == 1 |
                                             self_empl_offic_and_saisf == 1 ~ 1,
                                           TRUE                             ~ 0)) %>%
  rename(ses5 = hh_inc_quintile) %>%
  #create age groups based on 15-29
  mutate(age_group = case_when(age >= 15 & age < 20 ~ "1. 15-20",
                               #age >= 18 & age < 20 ~ "1. 18-19",
                               age >= 20 & age < 25 ~ "2. 20-24",
                               age >= 25 & age < 30 ~ "3. 25-29")) 


summary_stats <-
  youth_empl %>%
  mutate(satisfied_with_job = case_when(j1_1_1 == 1 ~ 1, TRUE ~ 0)) %>%
  select(age, sex, year,
         employed, employed_officially, self_employed, satisfied_with_job, transition_successful,
         in_education, edu_lvl, 
         year, area, ses5) %>%
  tbl_summary(by = year, 
              type = list( age ~ 'continuous2',
                           c(employed, employed_officially, self_employed, transition_successful,
                             in_education, satisfied_with_job) ~ 'dichotomous',
                           c( ses5, area, edu_lvl) ~ 'categorical'),
              label = list(age ~ "Age",
                           employed ~ "Employed",
                           employed_officially ~ "Officially Employed",
                           self_employed ~ "Self-Employed",
                           satisfied_with_job ~ "Satisfied with Job",
                           transition_successful ~ "Transition Successful",
                           in_education ~ "Attending Education",
                           area ~ "Area",
                           sex ~ "Sex",
                           ses5 ~ "HH Income Per Cap Quintile",
                           edu_lvl ~ "Highest Level of Education") , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data")) 

summary_stats_data_chapter <-
  youth_empl %>%
  mutate(satisfied_with_job = case_when(j1_1_1 == 1 ~ 1, TRUE ~ 0)) %>%
  select(employed, employed_officially, 
         self_employed, satisfied_with_job, transition_successful,
         year) %>%
  tbl_summary(by = year, 
              type = list( # age ~ 'continuous2',
                           c(employed, employed_officially, self_employed, transition_successful,
                             satisfied_with_job) ~ 'dichotomous' #,
                          # c( ses5, area, edu_lvl) ~ 'categorical'
                           ),
              label = list(# age ~ "Age",
                           employed ~ "Трудоустроен",
                           employed_officially ~ "Формальная занятость",
                           self_employed ~ "Самозанятый",
                           satisfied_with_job ~ "Удовлетворен работой",
                           transition_successful ~ "Переход завершен" #,
                           # in_education ~ "Attending Education",
                           # area ~ "Area",
                           # sex ~ "Sex",
                           # ses5 ~ "HH Income Per Cap Quintile",
                           # edu_lvl ~ "Highest Level of Education"
                           ) , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels() %>%
  as_flex_table() 

ncs_descr <-
  youth_empl %>%
  select(O, C, E, A, ES) %>%
  rename(Openness = O,
         Conscientiousness = C,
         Extraversion = E,
         Agreeableness = A,
         `Emotional Stability` = ES) %>%
  datasummary_skim()



boxplot_data <-
  youth_empl %>%
  select(O, C, E, A, ES, 
         transition_successful, ses5, edu_lvl, sex) %>%
  mutate(transition_successful = ifelse(transition_successful == 1, "Employed", "Unemployed")) %>%
  pivot_longer(cols = c(O, C, E, A, ES),
               names_to = "non_cogn_skills",
               values_to = "ncs_values") %>%
  mutate(non_cogn_skills = case_when(non_cogn_skills == "O" ~ "Openness",
                                     non_cogn_skills == "C" ~ "Conscientiousness",
                                     non_cogn_skills == "E" ~ "Extraversion",
                                     non_cogn_skills == "A" ~ "Agreeableness",
                                     non_cogn_skills == "ES" ~ "Emotional Stability"))

ncs_levels = c("Openness", "Conscientiousness", "Extraversion", 
               "Agreeableness", "Emotional Stability")

boxplot_data$non_cogn_skills = 
  factor(boxplot_data$non_cogn_skills, levels = ncs_levels)

bp1 <-
  boxplot_data %>%
  mutate(Employment = "Transition: Successful") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = transition_successful)) +
  scale_fill_manual(values = c("Unemployed" = "#f8766b", "Employed" = "#00bfc4")) +  
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(Employment ~ non_cogn_skills)

bp2 <-
  boxplot_data %>%
  mutate(`Income Per Capita Percentile` = "HH Income Per Capita Quintile") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = factor(ses5))) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(`Income Per Capita Percentile` ~ non_cogn_skills)

bp3 <-
  boxplot_data %>%
  mutate(`Level of Education` = "Level of Education") %>%
  ggplot(aes(y = ncs_values)) +
  geom_boxplot(aes(fill = edu_lvl)) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.title = element_blank()
  ) +
  facet_grid(`Level of Education` ~ non_cogn_skills)

# bp_final <- grid.arrange(bp1, bp2, bp3, ncol = 1)

### Beautiful Density Plots by Sex ####

# Prepare data for density plots
density_data <- youth_empl %>%
  select(sex, ses5, O, C, E, A, ES) %>%
  pivot_longer(cols = c(O, C, E, A, ES),
               names_to = "skill",
               values_to = "value") %>%
  mutate(skill = case_when(
    skill == "O" ~ "Openness",
    skill == "C" ~ "Conscientiousness", 
    skill == "E" ~ "Extraversion",
    skill == "A" ~ "Agreeableness",
    skill == "ES" ~ "Emotional Stability"
  )) %>%
  mutate(skill = factor(skill, levels = c("Openness", "Conscientiousness", 
                                         "Extraversion", "Agreeableness", 
                                         "Emotional Stability")))

# Create beautiful violin plot with boxplots inside
bp_sex_ncs <- 
  ggplot(density_data, aes(x = sex, y = value, fill = sex)) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.shape = 21, 
               outlier.fill = "white", outlier.size = 1.5) +
  scale_fill_manual(values = c("Female" = "#FF6B6B", "Male" = "#4ECDC4"),
                    name = "Sex") +
  facet_wrap(~ skill, nrow = 1) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, size = 0.5)
  ) +
  labs(x = "",
       y = "") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))


bp_ses5_ncs <- 
  ggplot(density_data, aes(x = ses5, y = value, fill = ses5)) +
  geom_boxplot(width = 0.2, alpha = 0.9, outlier.shape = 21, 
               outlier.fill = "white", outlier.size = 1.5) +
  facet_wrap(~ skill, nrow = 1) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, size = 0.5)
  ) +
  labs(x = "",
       y = "") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))


occup_table <-
  youth_empl %>%
  select(year, occupation) %>%
  # recode in accordance with isco-08 in russin МСКЗ-08
  mutate(occupation = case_when(occupation == 0 ~ "0. Военные",
                                occupation == 1 ~ "1. Руководители", 
                                occupation == 2 ~ "2. Специалисты-профессионалы",
                                occupation == 3 ~ "3. Специалисты-техники и иной средний специальный персонал",
                                occupation == 4 ~ "4. Служащие, занятые подготовкой и оформлением документации",
                                occupation == 5 ~ "5. Работники сферы обслуживания и торговли",
                                occupation == 6 ~ "6. Квалифицированные работники сельского хозяйств",
                                occupation == 7 ~ "7. Квалифицированные рабочие промышленности ",
                                occupation == 8 ~ "8. Операторы и сборщики промышленных установок и машин",
                                occupation == 9 ~ "9. Неквалифицированные работники",
                                TRUE ~ as.character(NA)
                                )) %>%
  tbl_summary(by = year, 
              type = list( 
                c(occupation) ~ 'categorical'
              ),
              label = list(# age ~ "Age",
                occupation ~ "Классификация занятости"
              ) ) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels() %>%
  as_flex_table() 
  


