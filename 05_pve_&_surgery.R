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
subdir2<- "/Data/SCB/Population_census_data_SCB/"

load(str_c(directory, subdir_rdata, file)) #Use if/when weights are needed
load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))


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


fig_pve <- ggplot(
  pve_df,
  aes(
    x = year_of_diagnosis,
    y = pve_pct
  )
) +
  geom_line(
    linewidth = 1,
    color = "#4F81BD"
  ) +
  geom_point(
    size = 2,
    color = "#4F81BD"
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    x = "Year of diagnosis",
    y = "PVE (% of all IE cases)"
  ) +
  theme_classic(base_size = 16)

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
    se = FALSE,
    color = "#4BACC6",
    linewidth = 1
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  scale_x_continuous(
    breaks = seq(1997, 2024, 5)
  ) +
  labs(
    x = "Year of diagnosis",
    y = "Surgery rate (%)"
  ) +
  theme_classic(base_size = 14) +
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
