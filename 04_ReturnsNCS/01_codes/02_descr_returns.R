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

youth_descr$hourly_wage

# probability density plot of hourly wage depending on the year
wage_density <-
  ggplot(youth_descr, aes(x = hourly_wage, color = as.factor(year), fill = as.factor(year))) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(x = "Hourly Wage (RUB)", y = "Density", color = "Year", fill = "Year") +
  theme_bw()

### create a table with summary of wage: year is a column whereas min, median, mode, mean, max, sd are the rows for hourly wage
wage_summary <-
  youth_descr %>%
  group_by(year) %>%
  summarise(
    N = n(),
    Min = min(hourly_wage, na.rm = TRUE),
    Q1 = quantile(hourly_wage, 0.25, na.rm = TRUE),
    Median = median(hourly_wage, na.rm = TRUE),
    Mean = mean(hourly_wage, na.rm = TRUE),
    Q3 = quantile(hourly_wage, 0.75, na.rm = TRUE),
    Max = max(hourly_wage, na.rm = TRUE),
    SD = sd(hourly_wage, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -year, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = year, values_from = Value) %>%
  arrange(match(Statistic, c("N", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD"))) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  tinytable::tt(
    # caption = "Intraclass Correlation Coefficients by Group",
    notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы."
  )
  # 
  # cols_label(
  #   Statistic = "Statistic",
  #   `2016` = "2016",
  #   `2019` = "2019"
  # ) %>%
  # fmt_number(
  #   columns = vars(`2016`, `2019`),
  #   decimals = 2
  # ) %>%
  # tab_source_note(md("Source: Author's calculations based on RLMS-HSE data"))

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

