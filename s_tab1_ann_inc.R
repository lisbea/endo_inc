library(tidyverse)
library(tableone)
library(flextable)
library(officer)
library(magrittr)
library(datapasta)
library(stringr)

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Studier/Incidence_of_IE/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir, "ie_inc_df.RData"))

source("bioavr_endo_help.R")

incidence_year_df <- ie_inc_df %>%
  group_by(year) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = cases / population * 100000,

    lower_ci = qchisq(0.025, 2 * cases) /
      (2 * population) * 100000,

    upper_ci = qchisq(0.975, 2 * (cases + 1)) /
      (2 * population) * 100000,

    .groups = "drop"
  ) %>%
  mutate(
      year = as.character(year),
      incidence_ci = sprintf(
        "%.1f (%.1f-%.1f)",
        incidence,
        lower_ci,
        upper_ci)
  ) %>%
  select(
    Year = year,
    Cases = cases,
    Population = population,
    `Incidence` = incidence_ci,
  )


header <- str_squish(str_remove("sTable 1. Annual incidence of infective endocarditis per 100,000 person-years in Sweden 1997–2023 (95% CI)", "\n"))
footer <- str_squish(str_remove("CI = confidence interval", "\n"))

supp_table_1 <- bioavr_tab(incidence_year_df, header, footer)

supp_table_1

#### Save as docx
subdir2 <- "/Studier/Incidence_of_IE/Tab/"
file <- "s_tab1_ann_inc.docx"
location_tab_1 <- str_c(directory, subdir2, file)

save_as_docx(supp_table_1, path = location_tab_1,
             pr_section = prop_section(page_size = page_size(orient = "portrait"), type = "continuous"))


