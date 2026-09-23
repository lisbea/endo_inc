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

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Studier/Incidence_of_IE/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir1, "ie_inc_df.RData"))

# Creating age- and sex-standardized incidence rates, ESP2013-compatible)

ie_inc_df <- ie_inc_df %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(18, 19, 24, 29, 34, 39, 44,
                 49, 54, 59, 64, 69, 74,
                 79, 84, Inf),
      right = FALSE,
      labels = c("18-19","20-24","25-29","30-34","35-39","40-44",
                 "45-49","50-54","55-59","60-64","65-69","70-74",
                 "75-79","80-84","85+")
    )
  )


# Creating standardization groups
incidence_grouped <- ie_inc_df %>%
  group_by(year, age_group, sex_binary) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  )

# Create matching ESP weights (18+ subset), using half of 15-19 group as an approximation
esp2013_18plus <- tibble::tibble(
  age_group = c("18-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74",
                "75-79","80-84","85+"),
  weight = c(5500/2,   # approximate split of 15–19
             6000,6000,6500,7000,7000,
             7000,7000,6500,6000,5500,
             5000,4000,2500,1500))

# Standardizing to ESP2013
incidence_std_eur <- incidence_grouped %>%
  left_join(esp2013_18plus, by = "age_group") %>%
  mutate(rate = cases / population)

incidence_eur_std <- incidence_std_eur %>%
  group_by(year) %>%
  summarise(
    std_incidence = sum(rate * weight) / sum(weight) * 100000,
    .groups = "drop"
  )

#Saving combined age-std incidence data, using ESP2013
save(incidence_eur_std, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Studier/Incidence_of_IE/Data/RData/incidence_eur_std.RData")

# ROUGH plot of the age-standardized incidence rates for individuals aged ≥18 years using the ESP2013 standard population.
ggplot(incidence_eur_std, aes(x = year, y = std_incidence)) +
  geom_line() +
  geom_point() +
  labs(
    y = "Age-standardised incidence per 100,000",
    x = "Year",
    title = "Age-standardised incidence rates were calculated for individuals aged ≥18 years using the ESP2013 standard population"
  )


### Better looking plot
ggplot(incidence_eur_std, aes(x = year, y = std_incidence)) +
  geom_line(color = "#1F4E79", linewidth = 1.2) +
  geom_point(color = "#1F4E79", size = 2.5) +

  scale_y_continuous(
    name = "Age-standardised incidence per 100,000",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(min(incidence_year$year), max(incidence_year$year), by = 2)
  ) +

  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Standardizing to the 2010 SWEDISH population only (sensitivity analysis)
swe_ref_pop <- ie_inc_df %>%
  filter(year == 2010) %>%
  group_by(age_group, sex_binary) %>%
  summarise(weight = sum(population), .groups = "drop")


incidence_std_swe <- incidence_grouped %>%
  left_join(swe_ref_pop, by = c("age_group", "sex_binary")) %>%
  mutate(rate = cases / population)


incidence_swe_std <- incidence_std_swe %>%
  group_by(year) %>%
  summarise(
    std_incidence = sum(rate * weight) / sum(weight) * 100000
  )

### Better looking plot
ggplot(incidence_swe_std, aes(x = year, y = std_incidence)) +
  geom_line(color = "#1F4E79", linewidth = 1.2) +
  geom_point(color = "#1F4E79", size = 2.5) +

  scale_y_continuous(
    name = "Age-standardised incidence per 100,000",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(min(incidence_year$year), max(incidence_year$year), by = 2)
  ) +

  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"))

#Saving combined age- and sex-std incidence data, using 2010 SWEDISH population
save(incidence_swe_std, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Studier/Incidence_of_IE/Data/RData/incidence_swe_std.RData")

# Plotting the age-and sex-standardized incidence rates for individuals aged ≥18 years using the 2010 Swedish population.
ggplot(incidence_swe_std, aes(x = year, y = std_incidence)) +
  geom_line() +
  geom_point() +
  labs(
    y = "Age- and sex-standardised incidence per 100,000",
    x = "Year",
    title = "Age- and sex-standardised incidence rates were calculated for individuals aged ≥18 years using the 2010 Swedish population"
  )
