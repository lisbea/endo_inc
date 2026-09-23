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
subdir<- "/Studier/Incidence_of_IE/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir, "ie_inc_df.RData"))

ie_inc_df <- ie_inc_df %>%
  mutate(
    age_group = case_when(
      age < 60 ~ "<60",
      age >= 60 & age < 70 ~ "60-69",
      age >= 70 & age < 80 ~ "70-79",
      age >= 80 ~ "80+",
    )
  )

ie_inc_df %>%
  group_by(age_group) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(cases))

age_trends <- ie_inc_df%>%
  group_by(year, age_group) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,
    .groups = "drop"
  )

age_cols <- c(
  "<60" = "#EFCB68", # KI gold
  "60-69"= "#4C979F", # KI turquoise
  "70-79"= "#4575B4", # KI blue
  "80+" = "#B24C63" # KI burgundy
)


# Plotting incidence by 100,000 inhabitants by age groups
inc_fig_ages <- ggplot(age_trends,
       aes(x = year,
           y = incidence,
           colour = age_group)) +
  geom_line() +
  scale_colour_manual(values = age_cols) +
  geom_point() +
  theme_classic() +
  labs(
    x = "Year",
    y = "Incidence per 100,000",
    colour = "Age group"
  ) +
  theme_classic(base_size = 16)

inc_fig_ages

### SAVING age group incidences
subdir2 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir2, "inc_fig_ages.pdf"), width = 15, height = 10, onefile = F)
inc_fig_ages
dev.off()

# Testing for dispersion
yearly_cases <- ie_inc_df %>%
  group_by(year, age_group) %>%
  summarise(
    cases = sum(cases),
    .groups = "drop"
  )

yearly_cases %>%
  group_by(age_group) %>%
  summarise(
    total_cases = sum(cases),
    min_cases_year = min(cases),
    max_cases_year = max(cases),
    mean_cases_year = mean(cases),
    median_cases_year = median(cases),
    .groups = "drop"
  )

### Overall change during the study period, by age group
# ie_inc_df_80 <- ie_inc_df %>% filter(age_group == "80+")
#
# first_year <- ie_inc_df_80 %>%
#   filter(year == min(year))
#
# last_year <- ie_inc_df_80 %>%
#   filter(year == max(year))
#
# overall_change_pct <-
#   (last_year$incidence / first_year$incidence - 1) * 100

