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
  mutate(sex = factor(sex_binary, levels = c(0, 1), labels = c("Male", "Female")))

ie_inc_df <- ie_inc_df %>%
  group_by(year, sex) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,
    .groups = "drop"
  )

ie_inc_df <- ie_inc_df %>%
  mutate(
    lower_ci = qchisq(0.025, 2 * cases) /
      (2 * population) * 100000,
    upper_ci = qchisq(0.975, 2 * (cases + 1)) /
      (2 * population) * 100000
  )


### Plotting incidence among males and females
ggplot(
  ie_inc_df,
  aes(
    x = year,
    y = incidence,
    color = sex)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(
    aes(
      ymin = lower_ci,
      ymax = upper_ci,
      fill = sex
    ),
    alpha = 0.15,
    color = NA
  ) +
  scale_color_manual(
    values = c(
      "Female" = "#E69F00",
      "Male" = "#0072B2"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Female" = "#E69F00",
      "Male" = "#0072B2"
    )
  ) +
  labs(
    x = "Year",
    y = "Incidence per 100,000 person-years",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_classic(base_size = 16)

### Calculating incidence rate ratio Male/Female by year
incidence_ratio <- ie_inc_df %>%
  group_by(year, sex) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(cases, population)
  ) %>%
  mutate(
    rate_male =
      cases_Male / population_Male,
    rate_female =
      cases_Female / population_Female,
    irr =
      rate_male / rate_female
  )

### Plotting Male/Female incidence rate ratio
ggplot(
  incidence_ratio,
  aes(year, irr)
) +
  geom_line(linewidth = 1.1) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Year",
    y = "Male/Female incidence rate ratio"
  ) +
  theme_classic(base_size = 16)
