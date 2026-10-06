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
subdir1 <- "/Data/RData/"


# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))

endocarditis_joined_total <- endocarditis_joined_total %>% filter(year >= 1997, year <= 2023)


endocarditis_joined_total %>%
  summarise(
    n = n(),
    min = min(age_at_diagnosis, na.rm = TRUE),
    q1 = quantile(age_at_diagnosis, 0.25, na.rm = TRUE),
    median = median(age_at_diagnosis, na.rm = TRUE),
    q3 = quantile(age_at_diagnosis, 0.75, na.rm = TRUE),
    max = max(age_at_diagnosis, na.rm = TRUE),
    mean = mean(age_at_diagnosis, na.rm = TRUE),
    sd = sd(age_at_diagnosis, na.rm = TRUE)
  )


## Age distribution, age pyramid by sex

endocarditis_joined_total <- endocarditis_joined_total %>%
  mutate(
    age_group = case_when(
      age_at_diagnosis < 60 ~ "<60",
      age_at_diagnosis >= 60 & age_at_diagnosis < 70 ~ "60-69",
      age_at_diagnosis >= 70 & age_at_diagnosis < 80 ~ "70-79",
      age_at_diagnosis >= 80 ~ "80+"
    ),
    age_group = factor(
      age_group,
      levels = c("<60", "60-69", "70-79", "80+")
    )
  )

endocarditis_joined_total <- endocarditis_joined_total %>%
  mutate(
    sex = factor(
      sex,
      levels = c(0, 1),
      labels = c("Male", "Female")
    )
  )

age_sex_pyramid_df <- endocarditis_joined_total %>%
  count(sex, age_group) %>%
  group_by(sex) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(pct = ifelse(sex == "Male", -pct, pct)
  )


age_sex_pyr_fig <- ggplot(age_sex_pyramid_df,
       aes(x = age_group,
           y = n,
           fill = sex)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Male" = "#0072B2",
    "Female" ="#D55E00")) +
  labs(
    x = NULL,
    y = "Number of cases",
    fill = NULL
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


age_sex_pyr_fig

### SAVING age group incidences
subdir2 <- "/Studier/Incidence_of_IE/Fig/"
pdf(str_c(directory, subdir2, "age_sex_pyr_fig.pdf"), width = 15, height = 10, onefile = F)
age_sex_pyr_fig
dev.off()
