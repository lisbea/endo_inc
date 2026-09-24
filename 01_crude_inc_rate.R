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

ie_inc_df <- ie_inc_df %>%
  mutate(sex = factor(sex_binary, levels = c(0, 1), labels = c("Male", "Female"))) %>%
  select("age", "year", "population", "cases", "incidence", "sex")

#Saving combined incidence dataframe
save(ie_inc_df, file = "~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/SHARED_OneDrive/Natalie Glaser's files - ENDO_CARE/Studier/Incidence_of_IE/Data/RData/ie_inc_df.RData")


# Calculate crude incidence per year overall (not standardized)
incidence_year <- ie_inc_df %>%
  group_by(year) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = (cases / population) * 100000,
    .groups = "drop"
  )

# Calculating 95% confidence intervals
incidence_year <- ie_inc_df |>
  group_by(year) |>
  summarize(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,

    lower_ci = qchisq(0.025, 2 * cases) /
      (2 * population) * 100000,

    upper_ci = qchisq(0.975, 2 * (cases + 1)) /
      (2 * population) * 100000,

    .groups = "drop"
  )

# Testing first to use a traditional Poisson model to fit the data
trend_model <- glm(cases ~ year, offset = log(population), family = poisson(), data = incidence_year)

# Predictions and 95% CIs on the log scale
pred <- predict(
  trend_model,
  newdata = incidence_year,
  type = "link",
  se.fit = TRUE
)

incidence_year <- incidence_year %>%
  mutate(
    fit = exp(pred$fit) / population * 100000,
    fit_lower = exp(pred$fit - 1.96 * pred$se.fit) /
      population * 100000,
    fit_upper = exp(pred$fit + 1.96 * pred$se.fit) /
      population * 100000
  )

### Plotting the data
inc_fig_simple <- ggplot(incidence_year, aes(x = year, y = incidence)) +
  geom_line(color = "#1F4E79", linewidth = 1) +
  geom_point(color = "#1F4E79", size = 2) +

  scale_y_continuous(
    name = "Incidence per 100,000 inhabitants",
    limits = c(0, NA),
    breaks = seq(0, 15, by = 3),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(min(incidence_year$year), max(incidence_year$year), by = 2)
  ) +

# # Fitted trend (Poisson, linear trend)
#   geom_line(aes(y = predicted),
#           colour = "red",
#           linewidth = 1) +
#
#   # Confidence band around trend
#   geom_ribbon(
#     aes(ymin = fit_lower,
#         ymax = fit_upper),
#     fill = "red",
#     alpha = 0.2
#   ) +

  theme_classic(base_size = 16) +
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

inc_fig_simple

# Checking for overdispersion, which indicates that there is overdispersion, ratio = 3.186954.
# Therefore Poisson model to fit linear trend is not the best method
deviance(trend_model) / df.residual(trend_model)

# Testing for non-linearity, to see if this fits the data better
# First creating the linear model
m1 <- glm(
  cases ~ year,
  offset = log(population),
  family = quasipoisson,
  data = incidence_year
)

# Then creating the nonlinear model (quasipoisson model with natural spline for calendar year with 3 df)
m_spline <- glm(
  cases ~ ns(year, df = 3),
  offset = log(population),
  family = quasipoisson,
  data = incidence_year
)

# Finally, testing the linear vs. non-linear model
anova(m1, m_spline, test = "F")
### This cave strong evidence of non-linearity, P < 0.001.  A quasipoisson regression model using a natural cubic spline for calendar year (3 df)
# provided a significantly better fit than a model assuming a linear trend.


# Therefore, progressing with generating smooth predictions, based on the non-linear quasipoisson model
newdat <- data.frame(
  year = seq(min(incidence_year$year),
             max(incidence_year$year),
             length.out = 200),
  population = median(incidence_year$population)
)

pred <- predict(
  m_spline,
  newdata = newdat,
  type = "link",
  se.fit = TRUE
)

newdat$fit <- exp(pred$fit) /
  newdat$population * 100000

newdat$lower <- exp(pred$fit - 1.96*pred$se.fit) /
  newdat$population * 100000

newdat$upper <- exp(pred$fit + 1.96*pred$se.fit) /
  newdat$population * 100000

# Plotting the smooth predictions
inc_fig_1 <- ggplot() +
  geom_point(
    data = incidence_year,
    aes(year, incidence),
    color = "black"
  ) +
  scale_y_continuous(
    name = "Incidence per 100,000 inhabitants",
    limits = c(0, 16),
    breaks = seq(0, 16, by = 2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(0, 2024, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  geom_line(
    data = incidence_year,
    aes(year, incidence),
    alpha = 1,
    color = "black",
    linewidth = 0.5
  ) +
  geom_ribbon(
    data = newdat,
    aes(year,
        ymin = lower,
        ymax = upper),
    fill = "red",
    alpha = 0.2
  ) +
  geom_line(
    data = newdat,
    aes(year, fit),
    colour = "red",
    linewidth = 0.75
  ) +
  theme_classic(base_size = 16)

inc_fig_1

### SAVING smoothly fitted curve
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "inc_fig_1.pdf"), width = 15, height = 10, onefile = F)
inc_fig_1
dev.off()

### SAVING simple incidence curve
pdf(str_c(directory, subdir3, "inc_fig_simple.pdf"), width = 15, height = 10, onefile = F)
inc_fig_simple
dev.off()

### Overall change during the study period, the answer is 118%
first_year <- incidence_year %>%
  filter(year == min(year))

last_year <- incidence_year %>%
  filter(year == max(year))

overall_change_pct <-
  (last_year$incidence / first_year$incidence - 1) * 100


