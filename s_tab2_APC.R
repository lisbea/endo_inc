library(tidyverse)
library(tableone)
library(flextable)
library(officer)
library(magrittr)
library(datapasta)
library(stringr)
library(segmented)
library(dplyr)

source("bioavr_endo_help.R")

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Studier/Incidence_of_IE/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir, "ie_inc_df.RData"))

incidence_year <- ie_inc_df %>%
  group_by(year) %>%
  summarise(
    cases = sum(cases),
    population = sum(population),
    incidence = (cases / population) * 100000,
    .groups = "drop"
  )


m1 <- glm(
  cases ~ year,
  offset = log(population),
  family = quasipoisson,
  data = incidence_year
)

m_seg2 <- segmented(
  m1,
  seg.Z = ~ year,
  npsi = 2
)

s <- as.data.frame(slope(m_seg2)$year)
names(s)

apc_table <- s %>%
  mutate(
    "Time period" = c(
      "1997-2008",
      "2008-2016",
      "2016-2023"
    ),

    APC = (exp(`Est.`) - 1) * 100,

    APC_low = (exp(`CI(95%).l`) - 1) * 100,

    APC_high = (exp(`CI(95%).u`) - 1) * 100
  ) %>%
  mutate(
    "Annual percentage change" = sprintf(
      "%.1f",
      APC),
    "95% CI" = sprintf(
      "%.1f-%.1f",
      APC_low,
      APC_high
    ) ) %>%
  dplyr::select(
    `Time period`,
    `Annual percentage change`,
    `95% CI`
  )

rownames(apc_table) <- NULL
apc_table

header <- str_squish(str_remove("sTable 2. Annual percentage changes in infective endocarditis incidence in Sweden 1997-2023 according to joinpoint-defined time periods", "\n"))
footer <- str_squish(str_remove("CI = confidence interval", "\n"))

supp_table_2 <- bioavr_tab(apc_table, header, footer)

supp_table_2

#### Save as docx
subdir2 <- "/Studier/Incidence_of_IE/Tab/"
file <- "s_tab2_apc.docx"
location_tab_1 <- str_c(directory, subdir2, file)

save_as_docx(supp_table_2, path = location_tab_1,
             pr_section = prop_section(page_size = page_size(orient = "portrait"), type = "continuous"))

