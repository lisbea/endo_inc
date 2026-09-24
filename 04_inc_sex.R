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

incidence_sex_yr <- ie_inc_df %>%
  group_by(year, sex) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,
    .groups = "drop"
  )

incidence_sex_yr <- incidence_sex_yr %>%
  mutate(
    lower_ci = qchisq(0.025, 2 * cases) /
      (2 * population) * 100000,
    upper_ci = qchisq(0.975, 2 * (cases + 1)) /
      (2 * population) * 100000
  )

# Creating poisson model
trend_model <- glm(cases ~ year, offset = log(population), family = poisson(), data = incidence_sex_yr)

# Checking for overdispersion (answer = 40.18682)
deviance(trend_model) / df.residual(trend_model)

# Calculating the Pearson dispersion
sum(residuals(trend_model, type = "pearson")^2) /
  df.residual(trend_model)

m1 <- glm(
  cases ~ year,
  offset = log(population),
  family = quasipoisson,
  data = incidence_sex_yr
)

m_spline <- glm(
  cases ~ ns(year, df = 3),
  offset = log(population),
  family = quasipoisson,
  data = incidence_sex_yr
)

# anova test gives non-significant p-value (P = appr. 0.64), therefore linear model is just as good as splline model.
anova(m1, m_spline, test = "F")

### Fitting linear quasi-Poisson model

m_sex <- glm(
  cases ~ year * sex + offset(log(population)),
  family = quasipoisson,
  data = incidence_sex_yr
)

incidence_sex_yr$pred_cases <- predict(
  m_sex,
  type = "response"
)

incidence_sex_yr <- incidence_sex_yr %>%
  mutate(
    pred_incidence = pred_cases / population * 100000
  )

### Plotting incidence among males and females
sex_inc_fig <- ggplot(incidence_sex_yr,
       aes(x = year,
           color = sex)) +

  geom_point(aes(y = incidence),
             size = 2) +

  geom_line(aes(y = pred_incidence),
            linewidth = 1) +

  scale_color_manual(values = c(
    "Male" = "#0072B2",
    "Female" = "#D55E00"
  )) +

  labs(
    x = NULL,
    y = "Incidence per 100,000 person-years",
    color = NULL
  ) +

  theme_classic(base_size = 16) +

  theme(
    legend.position = "top"
  )

sex_inc_fig

### SAVING smoothly fitted curve
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "sex_inc_fig.pdf"), width = 15, height = 10, onefile = F)
sex_inc_fig
dev.off()


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
sex_irr_fig <- ggplot(
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

sex_irr_fig

### SAVING smoothly fitted curve
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "sex_irr_fig.pdf"), width = 15, height = 10, onefile = F)
sex_irr_fig
dev.off()
