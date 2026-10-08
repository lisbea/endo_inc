library(tidyverse)
library(tableone)
library(flextable)
library(officer)
library(magrittr)
library(datapasta)
library(stringr)


source("bioavr_endo_help.R")

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Data/RData/"
load(str_c(directory, subdir, "endocarditis_joined_total.RData"))

endocarditis_joined_total <- endocarditis_joined_total %>% filter(year_of_diagnosis >= 1997, year_of_diagnosis <= 2023)
srie <- endocarditis_joined_total %>% filter(endoreg == 1 & year_of_diagnosis > 2006)

bioavr_flextab_defaults()

#### OBS!!! Still need to categorize valve vegetation size to >10 and <10 mm

srie <- srie %>%
  mutate(
    time_ep = factor(case_when(
      year_of_diagnosis >= 2007 & year_of_diagnosis < 2015 ~ "2007-2014",
      year_of_diagnosis >= 2015 & year_of_diagnosis < 2024 ~ "2015-2023",
      TRUE ~ NA_character_),
      levels = c(
        "2007-2014",
        "2015-2023"),
      ordered = TRUE),
    age_group = case_when(
      age_at_diagnosis < 60 ~ "<60",
      age_at_diagnosis >= 60 & age_at_diagnosis < 70 ~ "60-69",
      age_at_diagnosis >= 70 & age_at_diagnosis < 80 ~ "70-79",
      age_at_diagnosis >= 80 ~ "80+"),
    protesendokardit = factor(case_when(
      protesendokardit == 0 ~ "Native valve endocarditis",
      protesendokardit == 1 ~ "Prosthetic valve endocarditis")),
    op_endocarditis = factor(
      op_endocarditis,
      levels = c(0, 1),
      labels = c("No surgery", "Surgery")),
    infection_site = factor(case_when(
      # Only CIED
      str_detect(final_infectionsite, "CIED") &
        !str_detect(final_infectionsite,"aortic|mitral|tricuspid|pulmonary") ~
        "CIED only",
      # Double-sided & CIED
      str_detect(final_infectionsite, "aortic|mitral") &
        str_detect(final_infectionsite, "tricuspid|pulmonary") &
        str_detect(final_infectionsite, "CIED") ~
        "Double-sided & CIED",
      # Double-sided
      str_detect(final_infectionsite, "aortic|mitral") &
        str_detect(final_infectionsite, "tricuspid|pulmonary") ~
        "Double-sided",
      # Left-sided & CIED
      str_detect(final_infectionsite, "aortic|mitral") &
        str_detect(final_infectionsite, "CIED") |
        (is.na(final_infectionsite) & left_sided_IE == 1) &
        str_detect(final_infectionsite, "CIED")  ~
        "Left-sided & CIED",
      # Left-sided
      str_detect(final_infectionsite, "aortic|mitral") |
        (is.na(final_infectionsite) & left_sided_IE == 1) ~
        "Left-sided",
      # Right-sided & CIED
      str_detect(final_infectionsite, "aortic|mitral") &
        str_detect(final_infectionsite, "CIED") |
        (is.na(final_infectionsite) & left_sided_IE == 0) &
        str_detect(final_infectionsite, "CIED") ~
        "Right-sided & CIED",
      # Right-sided
      str_detect(final_infectionsite, "aortic|mitral") |
        (is.na(final_infectionsite) & left_sided_IE == 0) ~
        "Right-sided",
      TRUE ~ NA_character_),
      levels = c("Left-sided",
                 "Left-sided & CIED",
                 "Right-sided",
                 "Right-sided & CIED",
                 "Double-sided",
                 "Double-sided & CIED",
                 "CIED only")))



table(endocarditis_joined_total$congenital_endoreg_pre, endocarditis_joined_total$CHD_preop)
table(srie$time_ep)

# clipr::write_clip(names(srie))
# dput(names(srie))
# datapasta::vector_paste_vertical() # Pastes the clipboard as a vertical vector, also available as an addin

variables <-     c("lpnr",
                    # "all_diagnoses",
                    # "indatum_PAR",
                    # "utdatum",
                    # "copd_preop",
                    "CHD_preop",
                    # "ami_preop",
                    # "stroke_preop",
                    # "hyperlipidemia_preop",
                    # "PVD_preop",
                    # "alco_preop",
                    # "HF_preop",
                    # "liver_disease_preop",
                    # "afib_preop",
                    # "bleeding_preop",
                    # "PCI_preop",
                    # "ckd_preop",
                    # "valve_disease_preop",
                    # "reendocarditis_14d_out",
                    # "reendocarditis_diagdate",
                    # "stay_group",
                    "length_of_stay_group",
                    # "dead",
                    # "deathdate",
                    # "opdate_endoreg",
                    # "indatum_endoreg",
                    # "utdatum_endoreg",
                    # "age_endoreg",
                    # "sex_endoreg",
                    # "dentist",
                    # "TTE",
                    # "TEE",
                    # "murmur_new",
                    # "valve_disease_endoreg_pre",
                    # "bacteria_complete",
                    # "final_infectionsite_detailed",
                    # "valve_op_type_detailed_endoreg",
                    # "indication_op_detailed",
                    "vegetation",
                    "abscess",
                    "emboli",
                    # "fever",
                    # "bicuspid_aortic_valve_endoreg",
                    # "congenital_endoreg_pre",
                    "duke_criteria",
                    # "op_valve_infectionsite",
                    "vegetation_size_mm",
                    # "emboli_vasc_phenomena",
                    # "emboli_site",
                    # "endocarditis_type",
                    # "poliklinisk_beh",
                    # "endoreg",
                    # "final_infectionsite_endoreg",
                    # "protesendokardit_endoreg",
                    # "indication_op",
                    "bacteria_groups",
                    # "birth_date_complete",
                    "age_at_diagnosis",
                    # "HF_out",
                    # "HF_out_diagdate",
                    # "pci_PAR_out",
                    # "pci_PAR_out_diagdate",
                    # "cabg_PAR_out",
                    # "cabg_PAR_out_diagdate",
                    # "TAVI_PAR_out",
                    # "TAVI_PAR_out_diagdate",
                    # "ami_out",
                    # "ami_out_diagdate",
                    # "bleeding_out",
                    # "bleeding_out_diagdate",
                    # "stroke_tia_out",
                    # "stroke_tia_out_diagdate",
                    # "CIED_out_diagdate",
                    # "CRT_exposure",
                    # "CRT_exposure_diagdate",
                    # "ckd_out_OBS_remove_preop_CKD",
                    # "ckd_out_diagdate",
                    # "afib_out",
                    # "afib_out_diagdate",
                    # "dissection_rupture_out",
                    # "dissection_rupture_out_diagdate",
                    # "aortic_event_out",
                    # "aortic_event_out_diagdate",
                    # "married",
                    # "dispink_mean_pre",
                    "EDU",
                    "sex",
                    # "birthregion",
                    # "reop_blodning",
                    # "crea_postop",
                    # "diag_code",
                    # "DISCHARGE_HSURG_DATE",
                    # "EURO_AGE",
                    # "EURO_ANGINA",
                    # "EURO_ANNANOP",
                    # "EURO_CCS4",
                    # "EURO_CREATININE_CLEAREANCE",
                    # "EURO_ENDOKARDIT",
                    # "EURO_EXTRAKAR",
                    # "EURO_HJINF",
                    # "EURO_LOG_SUM",
                    # "EURO_LOG_SUM2",
                    # "EURO_LUNGSJD",
                    # "EURO_NEUROLOGI",
                    # "EURO_NYHA",
                    # "EURO_POSTINFARKT",
                    # "EURO_PULMONCHOICE",
                    # "EURO_SKREANUM",
                    # "EURO_SUM",
                    # "EURO_THORAKAL",
                    # "EURO_WEIGHTOFPROCEDURE",
                    # "FIBRILLATION_FLUTTER_COMPL",
                    # "hb_postop",
                    # "hb_preop",
                    # "IMPL_PACEMAKER_COMPL",
                    # "KLAFFKIRURGI",
                    # "KORONARKIRURGI",
                    # "opdate",
                    # "op_code",
                    # "PERFUSION_AORTAOCCTIME",
                    # "PERFUSION_ECCTIME",
                    # "STROKE_COMPL",
                    # "BMI",
                    # "WEIGHT",
                    # "year_of_surgery",
                    # "crea_preop",
                    # "hospital",
                    # "lvef",
                    # "emergent_op",
                    # "MekAVR",
                    # "BioAVR",
                    # "homograft",
                    # "CABG",
                    # "Asc_aorta",
                    # "DHCA",
                    # "BSA",
                    # "prior_tavi",
                    # "SAVR_before_endocarditis",
                    # "first_SAVR_after_this_endocarditis",
                    # "first_SAVR_out_diagdate",
                    # "SAVR_due_to_IE_recidiv",
                    # "SAVR_due_to_IE_recidiv_diagdate",
                    # "multiple_SAVR_due_to_any_indication",
                    # "multiple_SAVR_due_to_any_indication_diagdate",
                    # "MVR_before_endocarditis",
                    # "first_MVR_after_this_endocarditis",
                    # "first_MVR_out_diagdate",
                    # "MVR_due_to_IE_recidiv",
                    # "MVR_due_to_IE_recidiv_diagdate",
                    # "multiple_MVR_due_to_any_indication",
                    # "multiple_MVR_due_to_any_indication_diagdate",
                    # "TVR_before_endocarditis",
                    # "first_TVR_after_this_endocarditis",
                    # "first_TVR_out_diagdate",
                    # "TVR_due_to_IE_recidiv",
                    # "TVR_due_to_IE_recidiv_diagdate",
                    # "multiple_TVR_due_to_any_indication",
                    # "multiple_TVR_due_to_any_indication_diagdate",
                    # "PVR_before_endocarditis",
                    # "first_PVR_after_this_endocarditis",
                    # "first_PVR_out_diagdate",
                    # "PVR_due_to_IE_recidiv",
                    # "PVR_due_to_IE_recidiv_diagdate",
                    # "multiple_PVR_due_to_any_indication",
                    # "multiple_PVR_due_to_any_indication_diagdate",
                    # "fu_time_death_days",
                    # "fu_time_death_years",
                    # "dead_30",
                    # "ethnicity",
                    # "egfr",
                    "drug_use_pre",
                    # "diabetes_pre",
                    # "cancer_pre",
                    # "dialys_pre",
                    "op_endocarditis",
                    # "hypertension_pre",
                    # "CIED_pre",
                    # "CIED_out_30",
                    # "CIED_out_30_diagdate",
                    # "CIED_out",
                    "protesendokardit",
                    # "op_endocarditis_valve_cat",
                    # "final_infectionsite",
                    # "op_multi_valve",
                    # "left_sided_IE",
                    # "year_of_diagnosis",
                    # "prior_cardiac_surgery",
                    # "egfr_cat",
                    "dispinkfam_Q4",
                    # "severe_periodontitis",
                    # "severe_caries",
                    # "poor_oral_health",
                    "time_ep",
                    "age_group",
                    "infection_site")

srie <- srie %>% select(all_of(variables)) %>% select(-"lpnr")
# clipr::write_clip(names(srie))
# dput(names(srie))
# datapasta::vector_paste_vertical() # Pastes the clipboard as a vertical vector, also available as an addin
srie <- srie %>%  rename(
                    `Length of stay (days)`      = "length_of_stay_group",
                    `Valve vegetation`           = "vegetation",
                    `Abscess`                    = "abscess",
                    `Embolic events`             = "emboli",
                    `Congenital heart disease`   = "CHD_preop",
                    `Duke criteria`              = "duke_criteria",
                    `Vegetation size (mm)`       = "vegetation_size_mm",
                    `Bacteria`                   = "bacteria_groups",
                    `Age, years (median [IQR])`  = "age_at_diagnosis",
                    `Education`                  = "EDU",
                    `Female`                     = "sex",
                    # `Non-Nordic birth region`    = "birthregion",
                    `Drug user`                  = "drug_use_pre",
                    `Surgery`                    = "op_endocarditis",
                    `Prosthetic valve endocarditis` = "protesendokardit",
                    `Household income`           = "dispinkfam_Q4",
                    `Time-period`                = "time_ep",
                    `Age group`                  = "age_group",
                    `Infection site`             = "infection_site")

# clipr::write_clip(srie %>% dplyr::select(-"Time-period") %>% names())
# dput(names(srie))
# datapasta::vector_paste_vertical() # Pastes the clipboard as a vertical vector, also available as an addin

myVars <-                     c("Age, years (median [IQR])",
                                "Age group",
                                "Female",
                                "Education",
                                # "Non-Nordic birth region",
                                "Length of stay (days)",
                                "Congenital heart disease",
                                "Drug user",
                                "Prosthetic valve endocarditis",
                                "Bacteria",
                                "Infection site",
                                "Valve surgery",
                                "Abscess",
                                "Valve vegetation",
                                "Vegetation size (mm)",
                                "Embolic events",
                                "Duke criteria")

catVars <- srie %>% select(-`Time-period`, -`Age, years (median [IQR])`, -`Length of stay (days)`) %>% names()
skewed <- c("Age, years (median [IQR])")

tab2 <- srie %>% CreateTableOne(vars = myVars,
                                data = . ,
                                factorVars = catVars,
                                strata = "Time-period",
                                addOverall = T,
                                test = T)


tab2_word <- print(tab2,
                   nonnormal = skewed,
                   quote = F,
                   noSpaces = T,
                   smd = F,
                   missing = F,
                   # test = T, # adds P-value
                   contDigits = 1,
                   printToggle = F,
                   dropEqual = T,
                   explain = F)



tab2_df <- as_tibble(tab2_word, rownames = "Variable")

tab2_df$Variable[1] <- "No."

header <- str_squish(str_remove("Table 2. Baseline characteristics in 7,080 patients registered in the Swedish Registry of Infective Endocarditis, classified by time-period", "\n"))

# Check order here according to order of variables in table 1
footer <- str_squish(str_remove("Numbers are No. (%) unless otherwise noted. IQR = interquartile range; Q = quartile","\n"))


flextable_2 <- bioavr_tab(tab2_df, header, footer)


#### Save as docx
subdir2 <- "/Studier/Incidence_of_IE/Tab/"
file <- "inc_tab_2.docx"
location_tab_2 <- str_c(directory, subdir2, file)

save_as_docx(flextable_2, path = location_tab_2,
             pr_section = prop_section(page_size = page_size(orient = "landscape"), type = "continuous"))



