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

directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir1<- "/Data/RData/"
subdir2<- "/Data/SCB/Population_census_data_SCB/"

load(str_c(directory, subdir1, "endocarditis_joined_total.RData"))

agens_inc <- endocarditis_joined_total %>% filter

# BACTERIA ICDcodes
# A40 Sepsis orsakad av streptokocker
# A40.1 Sepsis orsakad av streptokocker grupp B
# A40.2 Sepsis orsakad av streptokocker grupp D och enterokocker (Enterokockus faecalis)
# A40.3 Sepsis orsakad av Streptococcus pneumoniae
# A40.8 Annan streptokocksepsis
# A40.9 Streptokocksepsis, ospecificerad
#
# A41 – Annan sepsis
# A41.0 Sepsis orsakad av S. aureus
# A41.1 Sepsis orsakad av annan spec. stafylokock
# A41.2 Sepsis orsakad av ospecificerad stafylokock
# A41.3 Sepsis orsakad av H. influenzae
# A 41.4 Sepsis orsakad av anaeroba bakterier (eg. Enterococcus)
# A 41.5 Sepsis orsakad av gramnegativa organismer
# A41.8 Andra specificerade former av sepsis
# A41.9 Sepsis, ospecificerad
#
# A49 Bakterieinfektion med ospecificerad lokalisation
# A49.0 Stafylokockinfektion, ospec. Lokalisation
# A49.1 Streptokock- och enterokockinfektion, ospec. Lokalisation
# A49.2 Haemophilus influensae, ospec. Lokalisation
# A49.3 Mykoplasmainfektion, ospec. Lokalisation
# A49.4 Infektion med andra bakterier
# A49.9 Bakterieinfektion, ospec.
#
# B95 Streptokocker och stafylokocker
# B95.0 Grupp A streptokocker
# B95.1 Grupp B streptokocker
# B95.2 Grupp D streptokocker och enterokocker (E. faecalis)
# B95.3 Pneumokocker (Streptococcus pneumoniae)
# B95.4 Andra streptokocker
# B95.5 Ospec. Streptokocker
# B95.6 S aureus
# B95.7 Andra spec. stafylokocker
# B95.8 ospec. Stafylokocker
# B96 Andra bakterier
# B96.0 Mycoplasma pneumoniae
# B96.1 Klebsiella pneumoniae
# B96.2 Escherichia coli
# B96.3 Haemophilus influenzae
# B96.4 Proteus mirabilis morgagni
# B96.5 Pseudomonas aeruginosa
# B96.7 Clostridium perfringens
# B96.8 Andra spec. bakterier


### S. aureus ICD10 A41.0, B95.6.
# 2559/2860 patienter i endokarditregistret med s.aureus har även denna ICD-kod. Ytterligare 9012 patienter skulle kunna klassas om från NA till S. Aureus.
endocarditis_joined_total %>% select(bacteria_groups, bacteria_complete, all_diagnoses) %>%
  filter(grepl("A410|B956", all_diagnoses)) %>%  filter(bacteria_groups == "S_aureus")

### Oral streptococci ICD10 B95.4.
# 1197/2158 i endokarditregistret har denna ICD-kod). Ytterligare 1495 patienter skulle kunna klassas om från NA till Oral Streptococci.
endocarditis_joined_total %>% select(bacteria_groups, bacteria_complete, all_diagnoses) %>%
  filter(grepl("954", all_diagnoses)) %>%  filter(bacteria_groups == "Oral_streptococci")

### Enterococci ICD A40.2, B95.2, A49.1, A41.4.
# 503/781 i endokarditregistret med Enterococci har även denna ICD-kod. Ytterligare 1018 patienter skulle kunna klassas om från NA till Enterococci.
endocarditis_joined_total %>% select(bacteria_groups, bacteria_complete, all_diagnoses) %>%
  filter(grepl("A414|B952|A491|A402", all_diagnoses)) %>%  filter(bacteria_groups == "Enterococci") %>% arrange(all_diagnoses)


