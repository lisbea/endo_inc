library(tidyverse)
library(survival)
library(dplyr)
library(broom)
library(naniar)
library(simputation)
library(car)

# Set base directory
directory <- system("find ~ -name \"*- ENDO_CARE*\" -type d -depth 5 -maxdepth 5 2>/dev/null | grep CARE", intern = T)
subdir<- "/Data/RData/"

# load(str_c(directory, subdir_rdata, file)) Use if/when weights are needed
load(str_c(directory, subdir, "endocarditis_joined_total.RData"))
load(str_c(directory, subdir, "endo_reg_endocarditis_pop_joined.RData"))



# Fit model to obtain incidence rate of
