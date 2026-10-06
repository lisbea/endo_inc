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

trend_model <- glm(
  cases ~ year * age_group,
  offset = log(population),
  family = quasipoisson(),
  data = age_trends
)


# Predictions and 95% CIs on the log scale
### Linear quasipoisson model
pred_linear <- predict(
  trend_model,
  newdata = age_trends,
  type = "link",
  se.fit = TRUE
)

age_linear_trends <- age_trends %>%
  mutate(
    fit = exp(pred_linear$fit) / population * 100000,
    fit_lower = exp(pred_linear$fit - 1.96 * pred_lineard$se.fit) /
      population * 100000,
    fit_upper = exp(pred_linear$fit + 1.96 * pred_linear$se.fit) /
      population * 100000
  )

# Testing spline vs linear model for fit
m_linear <- glm(
  cases ~ year + age_group,
  offset = log(population),
  family = quasipoisson(),
  data = age_trends
)

m_spline <- glm(
  cases ~ splines::ns(year, df = 3) * age_group,
  offset = log(population),
  family = quasipoisson(),
  data = age_trends
)

m_no_interaction <- glm(
  cases ~ splines::ns(year, df = 3) + age_group,
  offset = log(population),
  family = quasipoisson(),
  data = age_trends
)


anova(m_no_interaction, m_spline, test = "F") ### Testing whether the incidence trends differ between the age groups, which gives a v. small P-value indicating the trends differ per age group
anova(m_linear, m_no_interaction, test = "F") ### Produces P=0.0036, which indicates that spline model is more appropriate

### Predict model, using splines

pred_spline <- predict(
  m_spline,
  newdata = age_trends,
  type = "link",
  se.fit = TRUE
)

plot_df <- age_trends %>%
  mutate(
    fit_cases = exp(pred_spline$fit),
    lower_cases = exp(pred_spline$fit - 1.96 * pred_spline$se.fit),
    upper_cases = exp(pred_spline$fit + 1.96 * pred_spline$se.fit),

    fit = fit_cases / population * 100000,
    lower = lower_cases / population * 100000,
    upper = upper_cases / population * 100000
  )


age_cols <- c(
  "<60" = "#EFCB68", # KI gold
  "60-69"= "#4C979F", # KI turquoise
  "70-79"= "#4575B4", # KI blue
  "80+" = "#B24C63" # KI burgundy
)

# Plotting incidence by 100,000 inhabitants by age groups
inc_fig_ages <- ggplot() +
  geom_point(
    data = age_trends,
    aes(
      x = year,
      y = incidence,
      colour = age_group,
    ),
    alpha = 0.8
  ) +
  geom_ribbon(
    data = plot_df,
    aes(
      x = year,
      ymin = lower,
      ymax = upper,
      fill = age_group
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(
    data = plot_df,
    aes(
      x = year,
      y = fit,
      colour = age_group
    ),
    linewidth = 1.2
  ) +
  scale_colour_manual(values = age_cols) +
  scale_fill_manual(values = age_cols) +
  labs(
    x = "Year",
    y = "Incidence per 100,000",
    colour = "Age group",
    fill = "Age group"
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

