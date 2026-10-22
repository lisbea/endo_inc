library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(naniar)
library(readr)
library(stringr)
library(splines)

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Studier/Incidence_of_IE/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir, "ie_inc_df.RData"))

cases_sex_year <- ie_inc_df %>%
  group_by(year, sex) %>%
  summarise(cases = sum(cases), .groups = "drop")

fig_cases_overview <- ggplot(cases_sex_year,
       aes(x = factor(year),
           y = cases,
           fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c("Female" = "#D55E00",
                               "Male" = "#0072B2")) +
  labs(
    x = "Year",
    y = "Number of cases",
    fill = "Sex"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12),
    panel.grid.major.x = element_blank()
  )

fig_cases_overview

### SAVING smoothly fitted curve, based on spline function with 95% confidence bands
subdir3 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir3, "cases_overview_fig.pdf"), width = 15, height = 10, onefile = F)
fig_cases_overview
dev.off()
