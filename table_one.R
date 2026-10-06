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

endocarditis_joined_total <- endocarditis_joined_total %>%
  mutate(
    time_ep = as_factor(case_when(
      year_of_diagnosis < 2006 ~ "1997-2005",
      year_of_diagnosis >= 2006 & year_of_diagnosis < 2015 ~ "2006-2014",
      year_of_diagnosis >= 2015 & year_of_diagnosis < 2024 ~ "2015-2023")),
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
      !str_detect(final_infectionsite,
                  "aortic|mitral|tricuspid|pulmonary") ~
      "CIED only",
    # Double-sided
    str_detect(final_infectionsite, "aortic|mitral") &
      str_detect(final_infectionsite, "tricuspid|pulmonary") ~
      "Double-sided",
    # Left-sided
    str_detect(final_infectionsite, "aortic|mitral") ~
      "Left-sided",
    # Right-sided
    str_detect(final_infectionsite, "tricuspid|pulmonary") ~
      "Right-sided",
    TRUE ~ NA_character_),
    levels = c("Left-sided",
                "Right-sided",
                "Double-sided",
                "CIED only")))

# endocarditis_joined_total %>%
#   count(protesendokardit)
#
# endocarditis_joined_total %>%
#   count(infection_site)

bioavr_flextab_defaults()

# clipr::write_clip(names(endocarditis_joined_total))
# dput(names(endocarditis_joined_total))
# datapasta::vector_paste_vertical() # Pastes the clipboard as a vertical vector, also available as an addin


variables <-            c("lpnr",
                           # "all_diagnoses",
                           # "indatum_PAR",
                           # "utdatum",
                           "copd_preop",
                           "CHD_preop",
                           "ami_preop",
                           "stroke_preop",
                           "hyperlipidemia_preop",
                           "PVD_preop",
                           "alco_preop",
                           "HF_preop",
                           "liver_disease_preop",
                           "afib_preop",
                           # "bleeding_preop",
                           "PCI_preop",
                           "ckd_preop",
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
                           # "vegetation",
                           # "abscess",
                           # "emboli",
                           # "fever",
                           # "bicuspid_aortic_valve_endoreg",
                           # "congenital_endoreg_pre",
                           # "duke_criteria",
                           # "op_valve_infectionsite",
                           # "vegetation_size_mm",
                           # "emboli_vasc_phenomena",
                           # "emboli_site",
                           # "endocarditis_type",
                           # "poliklinisk_beh",
                           # "endoreg",
                           # "final_infectionsite_endoreg",
                           # "protesendokardit_endoreg",
                           # "indication_op",
                           # "bacteria_groups",
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
                           "married",
                           "dispink_mean_pre",
                           "EDU",
                           "sex",
                           "birthregion",
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
                           "BMI",
                           # "WEIGHT",
                           # "year_of_surgery",
                           # "crea_preop",
                           # "hospital",
                           "lvef",
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
                           "diabetes_pre",
                           "cancer_pre",
                           "dialys_pre",
                           "op_endocarditis",
                           "hypertension_pre",
                           "CIED_pre",
                           # "CIED_out_30",
                           # "CIED_out_30_diagdate",
                           # "CIED_out",
                           "protesendokardit",
                           # "op_endocarditis_valve_cat",
                           # "final_infectionsite",
                           # "op_multi_valve",
                           # "left_sided_IE",
                           # "year_of_diagnosis",
                           "prior_cardiac_surgery",
                           "egfr_cat",
                           # "dispinkfam_Q4",
                           # "severe_periodontitis",
                           # "severe_caries",
                           # "poor_oral_health",
                           "time_ep",
                           "age_group",
                           "infection_site")

endocarditis_joined_total <- endocarditis_joined_total %>% select(all_of(variables)) %>% select(-"lpnr")

# renaming variables for table. if there are spaces, back ticks need to be used.
endocarditis_joined_total <- endocarditis_joined_total %>%  rename(
                                         `Age, years (median [IQR])` = age_at_diagnosis,
                                         `Age group` = age_group,
                                         `Female sex` = sex,
                                         `Time episode` = time_ep,
                                         `Drug users` = drug_use_pre,
                                         `Congenital heart disease` = CHD_preop,
                                         `Prosthetic valve endocarditis` = protesendokardit,
                                         `Valve surgery` = op_endocarditis,
                                         `Infection site` = infection_site,
                                         `Length of stay` = length_of_stay_group,
                                         `Prior cardiac surgery (any)` = prior_cardiac_surgery,
                                         `Prior atrial fibrillation` = afib_preop,
                                         `Prior myocardial infarction` = ami_preop,
                                         `History of cancer` = cancer_pre,
                                         `Chronic obstructive pulmonary disease` = copd_preop,
                                         `Diabetes mellitus` = diabetes_pre,
                                         `Prior heart failure` = HF_preop,
                                         `Hypertension` = hypertension_pre,
                                         `Liver disease` = liver_disease_preop,
                                         `Hyperlipidemia` = hyperlipidemia_preop,
                                         `Peripheral vascular disease` = PVD_preop,
                                         `Prior stroke` = stroke_preop,
                                         `Prior PCI` = PCI_preop,
                                         `eGFR, mL/min/1.73 m2` = egfr_cat,
                                         `Dialysis` = dialys_pre,
                                         `Chronic kidney disease` = ckd_preop,
                                         `Left ventricular ejection fraction` = lvef,
                                         `Alcohol dependence` = alco_preop,
                                         `Body mass index, kg/m2` = BMI,
                                         `Married` = married,
                                         `Household income` = dispink_mean_pre,
                                         `Education` = EDU,
                                         `Prior pacemaker/ICD`= CIED_pre,
                                         `Non-Nordic birth region` = birthregion)


# clipr::write_clip(endocarditis_joined_total %>% dplyr::select(-"Time episode") %>% names())

# dput(names(endocarditis_joined_total))
# datapasta::vector_paste_vertical() # Pastes the clipboard as a vertical vector, also available as an addin


myVars <-   c( "Age, years (median [IQR])",
               "Female sex",
               "Age group",
               "Married",
               "Household income",
               "Length of stay",
               "Infection site",
               "Prosthetic valve endocarditis",
               "Congenital heart disease",
               "Drug users",
               "Prosthetic valve endocarditis",
               "Valve surgery",
               "Chronic obstructive pulmonary disease",
              "Prior myocardial infarction",
              "Prior stroke",
              "Hyperlipidemia",
              "Prior pacemaker/ICD",
              "Peripheral vascular disease",
              "Alcohol dependence",
              "Prior heart failure",
              "Liver disease",
              "Prior atrial fibrillation",
              "Prior PCI",
              "Chronic kidney disease",
              "Body mass index, kg/m2",
              "Left ventricular ejection fraction",
              "Diabetes mellitus",
              "History of cancer",
              "Dialysis",
              "Hypertension",
              "Prior cardiac surgery (any)",
              "eGFR, mL/min/1.73 m2",
              "Education",
              "Non-Nordic birth region"
             )


catVars <- endocarditis_joined_total %>% select(-`Time episode`, -`Age, years (median [IQR])`, -`Female sex`, -`Body mass index, kg/m2`, -`Household income`, -`Length of stay`) %>% names()

skewed <- c("Age, years (median [IQR])")

tab1 <- endocarditis_joined_total %>% CreateTableOne(vars = myVars,
                                        data = . ,
                                        factorVars = catVars,
                                        strata = "Time episode",
                                        addOverall = T,
                                        test = T)

tab1_word <- print(tab1,
                   nonnormal = skewed,
                   quote = F,
                   noSpaces = T,
                   smd = F,
                   missing = F,
                   test = F, # adds P-value
                   contDigits = 1,
                   printToggle = F,
                   dropEqual = T,
                   explain = F)

tab1_df <- as_tibble(tab1_word, rownames = "Variable") %>% select(-test)

tab1_df$Variable[1] <- "No."

header <- str_squish(str_remove("Table 1. Baseline characteristics by time period", "\n"))

# Check order here according to order of variables in table 1
footer <- str_squish(str_remove("Numbers are No. (%) unless otherwise noted. IQR = interquartile range; Q = quartile;
 PCI = percutaneous coronary intervention; eGFR = estimated glomerular filtration rate", "\n"))


flextable_1 <- bioavr_tab(tab1_df, header, footer)


#### Save as docx
subdir <-
file <-
location_tab_1 <- str_c(directory, subdir, file)

save_as_docx(flextable_1, path = location_tab_1,
             pr_section = prop_section(page_size = page_size(orient = "portrait"), type = "continuous"))




