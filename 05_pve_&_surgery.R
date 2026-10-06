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
library(patchwork)


# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir1<- "/Data/RData/"

load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))

endocarditis_joined_total <- endocarditis_joined_total %>% filter(year_of_diagnosis >= 1997, year_of_diagnosis <= 2023)

endocarditis_joined_total <- endocarditis_joined_total %>%
  mutate(
    ie_type = factor(
      protesendokardit,
      levels = c(0, 1),
      labels = c("NVE", "PVE")
    ),
    surgery = factor(
      op_endocarditis,
      levels = c(0, 1),
      labels = c("No surgery", "Surgery")
    )
  )

### Missing data
endocarditis_joined_total %>%
  miss_var_summary() %>%
  filter(pct_miss > 0) %>%
  print(n = Inf)

table(endocarditis_joined_total$ie_type)
table(endocarditis_joined_total$surgery)
table(endocarditis_joined_total$ie_type, endocarditis_joined_total$surgery)

### Testing difference in use of surgery among NVE and PVE
chisq.test(
  table(
    endocarditis_joined_total$protesendokardit,
    endocarditis_joined_total$op_endocarditis
  )
)


pve_df <- endocarditis_joined_total %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    total_cases = n(),
    pve_cases = sum(protesendokardit == 1),
    pve_pct = 100 * pve_cases / total_cases,
    .groups = "drop"
  )

# Fitting a linear logistic model
m_pve <- glm(
  cbind(pve_cases, total_cases - pve_cases) ~ year_of_diagnosis,
  family = binomial(),
  data = pve_df
)

# Fitting a spline model
m_pve_spline <- glm(
  cbind(pve_cases, total_cases - pve_cases) ~ splines::ns(year_of_diagnosis, df = 3),
  family = binomial(),
  data = pve_df
)

# Comparing the linear and spline models
anova(m_pve, m_pve_spline, test = "Chisq") # gives significant P-value indicating that spline model is better

### FITTING SPLINE MODEL
newdat <- data.frame(
  year_of_diagnosis = seq(
    min(pve_df$year_of_diagnosis),
    max(pve_df$year_of_diagnosis),
    length.out = 200
  )
)

pred <- predict(
  m_pve_spline,
  newdata = newdat,
  type = "link",
  se.fit = TRUE
)

newdat <- newdat %>%
  mutate(
    fit = plogis(pred$fit) * 100,
    lower = plogis(pred$fit - 1.96 * pred$se.fit) * 100,
    upper = plogis(pred$fit + 1.96 * pred$se.fit) * 100
  )

fig_pve <- ggplot() +
  geom_ribbon(
    data = newdat,
    aes(
      x = year_of_diagnosis,
      ymin = lower,
      ymax = upper
    ),
    fill = "#4BACC6",
    alpha = 0.2
  ) +
  geom_line(
    data = newdat,
    aes(
      x = year_of_diagnosis,
      y = fit
    ),
    colour = "#4BACC6",
    linewidth = 1.2
  ) +
  geom_point(
    data = pve_df,
    aes(
      x = year_of_diagnosis,
      y = pve_pct
    ),
    colour = "#4F81BD",
    size = 2
  ) +
  theme_classic(base_size = 18) +
  labs(
    x = "Year of diagnosis",
    y = "Prosthetic valve endocarditis (%)"
  )

fig_pve


### Plotting surgery increase over time

surgery_df <- endocarditis_joined_total %>%
  group_by(year_of_diagnosis) %>%
  summarise(
    total_cases = n(),
    surgeries = sum(op_endocarditis == 1, na.rm = TRUE),
    surgery_rate = 100 * surgeries / total_cases,
    .groups = "drop"
  )


fig_surgery <- ggplot(
  surgery_df,
  aes(
    x = year_of_diagnosis,
    y = surgery_rate
  )
) +
  geom_point(
    color = "#4BACC6",
    alpha = 0.6,
    size = 2
  ) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = TRUE,
    color = "#4BACC6",
    linewidth = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  scale_x_continuous(
    breaks = seq(1997, 2024, 5)
  ) +
  labs(
    x = "Year of diagnosis",
    y = "Surgery rate (%)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    panel.grid.minor = element_blank()
  )

fig_pve
fig_surgery

### SAVING PVE figure
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "fig_pve.pdf"), width = 15, height = 10, onefile = F)
fig_pve
dev.off()

### SAVING surgery figure
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "fig_surgery.pdf"), width = 15, height = 10, onefile = F)
fig_surgery
dev.off()
