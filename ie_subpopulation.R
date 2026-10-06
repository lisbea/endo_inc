library(tidyverse)
library(survival)
library(dplyr)
library(broom)
library(naniar)
library(simputation)
library(car)
library(readr)
library(stringr)
library(ggplot2)
library(splines)


# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir1<- "/Data/RData/"

# load(str_c(directory, subdir_rdata, file))
load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))

endocarditis_joined_total <- endocarditis_joined_total %>% filter(year_of_diagnosis >2006 & year_of_diagnosis < 2024)

srie <- endocarditis_joined_total %>% filter(endoreg == 1)

srie %>%
  miss_var_summary() %>%
  filter(pct_miss > 0) %>%
  print(n = Inf)

srie %>% count(bacteria_groups, sort = TRUE)


srie <- srie %>%
  mutate(
    age_group = case_when(
      age_at_diagnosis < 60 ~ "<60",
      age_at_diagnosis >= 60 & age_at_diagnosis < 70 ~ "60-69",
      age_at_diagnosis >= 70 & age_at_diagnosis < 80 ~ "70-79",
      age_at_diagnosis >= 80 ~ "80+",
    )
  )

srie_bacteria <- srie %>%
  count(year_of_diagnosis, bacteria_groups) %>%
  group_by(year_of_diagnosis) %>%
  mutate(
    percent = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  filter(bacteria_groups %in% c("S_aureus",
                                "Oral_streptococci",
                                "Enterococci"))

ggplot(srie_bacteria,
       aes(x = year_of_diagnosis,
           y = percent,
           color = bacteria_groups)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "S_aureus" = "#D55E00",
      "Oral_streptococci" = "#009E73",
      "Enterococci" = "#0072B2"
    ),
    labels = c("Enterococci", "Oral streptococci", "Staphylococcus aureus")
  ) +
  labs(
    x = NULL,
    y = "Percent",
    color = NULL
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top"
  )

ggplot(srie_bacteria,
       aes(x = year_of_diagnosis,
           y = n,
           color = bacteria_groups)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "S_aureus" = "#D55E00",
      "Oral_streptococci" = "#009E73",
      "Enterococci" = "#0072B2"
    ),
    labels = c("Enterococci", "Oral streptococci", "Staphylococcus aureus")
  ) +
  labs(
    x = NULL,
    y = "Number of cases",
    color = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top"
  )

### Surgery use
surgery_srie_df <- srie %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    total_cases = n(),
    surgeries = sum(op_endocarditis == 1, na.rm = TRUE),
    surgery_rate = 100 * surgeries / total_cases,
    .groups = "drop"
  )


ggplot(srie_site,
       aes(x = year_of_diagnosis,
           y = percent,
           color = srie_site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # scale_color_manual(
  #   values = c(
  #     "xx" = "#D55E00",
  #     "xx" = "#009E73",
  #     "xx" = "#0072B2"
  #   ),
  #   labels = c("xx", "xx", "xx")
  # ) +
  labs(
    x = NULL,
    y = "Percent",
    color = NULL
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top"
  )



srie_drugs <- srie %>%
  mutate(drug_use_pre = factor(drug_use_pre, levels = c(0, 1), labels = c("Non-drug user", "Drug users"))) %>%
  count(year_of_diagnosis, drug_use_pre) %>%
  group_by(year_of_diagnosis) %>%
  mutate(
    percent = 100 * n / sum(n)
  ) %>%
  filter(drug_use_pre == "Drug users") %>%
  ungroup()

ggplot(srie_drugs,
       aes(x = year_of_diagnosis,
           y = percent,
           color = drug_use_pre)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Year",
    y = "Percent",
    color = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top"
  )



