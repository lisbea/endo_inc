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
subdir1<- "/Data/RData/"
subdir2<- "/Data/SCB/Population_census_data_SCB/"


# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))


# loading population census data 1997-2024 by year, sex, and age, originally from SCB
pop_census <- read_delim(
  file.path(directory, subdir2, "population_scb_year_age_sex.csv"),
  delim = ",",
  skip = 2,
  quote = "",            # IGNORERA quotes
  locale = locale(encoding = "latin1"))

# CLEAN census data
# Remove excess ""
names(pop_census) <- gsub('"', '', names(pop_census))

pop_census <- pop_census %>%
  mutate(
    ålder = str_remove_all(ålder, "\""),
    kön = str_remove_all(kön, "\"")
  )

pop_census <- pop_census %>%
  rename(
    age = ålder,
    sex = kön
  ) %>%
  mutate(
    age = str_extract(age, "\\d+") |> as.numeric(),
    sex = dplyr::recode(sex,
                 "män" = "M",
                 "kvinnor" = "F"),
    sex_binary = case_when(
      sex == "M" ~ 0,
      sex == "F" ~ 1,
      TRUE ~ NA_real_)
    )

# Create correct age in IE dataset for correct matching to population data - defines age as floor, not rounding.
endocarditis_joined_total <- endocarditis_joined_total %>%
  mutate(age_at_diagnosis = floor(age_at_diagnosis))

# Check that the age ranges correspond in the two data sets
range(pop_census$age)
range(endocarditis_joined_total$age_at_diagnosis)

# Create count of cases per year, age, and sex
ie_counts <- endocarditis_joined_total %>% group_by(year_of_diagnosis, age_at_diagnosis, sex) %>%
  summarise(cases = n(), .groups = "drop")

# Rename column headings to correspond to census data
ie_counts <- ie_counts %>% rename(year = year_of_diagnosis, age = age_at_diagnosis, sex_binary = sex)

# Changing format of pop_census data
pop_census_long <- pop_census %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"), # selects year columns
    names_to = "year",
    values_to = "population")

pop_census_long <- pop_census_long %>% mutate(year = as.numeric(year))
pop_census_long <- pop_census_long %>% select(age, year, sex_binary, population)

# Merging the two datasets
ie_inc_df <- pop_census_long %>%
  left_join(ie_counts, by = c("year", "age", "sex_binary")) %>%
  mutate(cases = ifelse(is.na(cases), 0, cases),
         incidence = (cases / population) *100000)

ie_inc_df <- ie_inc_df %>% filter(year >= 1997, year <= 2023)

#Saving combined incidence dataframe
save(ie_inc_df, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Projekt/Incidence_of_IE/Data/RData/ie_inc_df.RData")


# Calculate crude incidence per year overall (not standardized)

incidence_year <- ie_inc_df %>%
  group_by(year) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = (cases / population) * 100000,
    .groups = "drop"
  )

# plotting crude incidence rate over time (not standardized)

ggplot(incidence_year, aes(x = year, y = incidence)) +
  geom_line() +
  geom_point() +
  labs(
    y = "Incidence per 100,000",
    x = "Year",
    title = "Incidence of Infective Endocarditis over Time"
  )

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
save(incidence_eur_std, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Projekt/Incidence_of_IE/Data/RData/incidence_eur_std.RData")

# ROUGH plot of the age-standardized incidence rates for individuals aged ≥18 years using the ESP2013 standard population.
ggplot(incidence_eur_std, aes(x = year, y = std_incidence)) +
  geom_line() +
  geom_point() +
  labs(
    y = "Age-standardised incidence per 100,000",
    x = "Year",
    title = "Age-standardised incidence rates were calculated for individuals aged ≥18 years using the ESP2013 standard population"
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

#Saving combined age- and sex-std incidence data, using 2010 SWEDISH population
save(incidence_swe_std, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Projekt/Incidence_of_IE/Data/RData/incidence_swe_std.RData")

# Plotting the age-and sex-standardized incidence rates for individuals aged ≥18 years using the 2010 Swedish population.
ggplot(incidence_swe_std, aes(x = year, y = std_incidence)) +
  geom_line() +
  geom_point() +
  labs(
    y = "Age- and sex-standardised incidence per 100,000",
    x = "Year",
    title = "Age- and sex-standardised incidence rates were calculated for individuals aged ≥18 years using the 2010 Swedish population"
  )

