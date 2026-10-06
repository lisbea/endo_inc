#### BioAVR helper functions
# Require flextable package and magrittr style pipes



######### Set flextable defaults
#
# Dependencies : flextable
#
bioavr_flextab_defaults <- function(){set_flextable_defaults(font.family = "Calibri",
                                                             font.size = 10,
                                                             border.color = "black")
}




######### Create default BioAVR table from dataframe
#
# Dependencies : dplyr, flextable, officer
#
bioavr_tab <- function(df, header, footer){
  flextable(df) %>%
    add_header_lines(header) %>%
    add_footer_lines(footer) %>%
    bold(i = 1, part = "header") %>%
    hline_top(part = "header",
              border = fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    hline(i = 1,
          part = "header",
          border = fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    hline_top(part = "body",
              border = fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    hline_bottom(part = "body",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    hline_bottom(part = "footer",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    autofit(part = "body") %>%
    bg(part = "body", bg = "#f5f5f5") %>%
    align(part = "all", align = "center") %>%
    align(j = 1, part = "all", align = "left")
}



######### Create default grouped table from grouped flextab object
#
# Dependencies : dplyr, flextable, officer
#
bioavr_group_tab <- function(flextab, header, footer){
  flextab %>%
    add_header_lines(header) %>%
    add_footer_lines(footer) %>%
    bold(i = 1, part = "header") %>%
    hline_top(part = "header",
              border = fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    hline(i = 1,
          part = "header",
          border = fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    hline_top(part = "body",
              border = fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    hline_bottom(part = "body",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    hline_bottom(part = "footer",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    autofit(part = "body") %>%
    bg(part = "body", bg = "#f5f5f5") %>%
    align(part = "all", align = "center") %>%
    align(j = 1, part = "all", align = "left") %>%
    bg(i = ~ !is.na(id), bg = "transparent", part = "body") %>%
    flextable::border(i = ~ !is.na(id),
                      part = "body",
                      border.top = fp_border(color = "black",
                                             width = 0.25,
                                             style = "solid"))
}



######### Create cuminc table from survfit object
#
# Dependencies : dplyr, survival, stringr, tidyr
#
cuminc_table <- function(survfit_obj,
                         time_pts = c(5, 10, 15),
                         group_var,
                         round_dig = 1,
                         dec_dig = 1,
                         time_unit = "years"){
  summary_obj <- summary(survfit_obj, times = time_pts)


  wider <- tibble({{ group_var }} := str_remove(summary_obj$strata, paste0(group_var, "=")),
                  time = summary_obj$time,
                  cif = 100 * (1 - summary_obj$surv),
                  lower = 100 * (1 - summary_obj$upper),
                  upper = 100 * (1 - summary_obj$lower)) %>%

    pivot_wider(names_from = "time", values_from = c("cif", "lower", "upper"))


  bind_tab <- wider %>% transmute({{ group_var }} := .data[[{{ group_var }}]])

  for(i in seq_along(time_pts)){
    cif_temp <-  paste("cif", time_pts[i], sep = "_")
    lower_temp <- paste("lower", time_pts[i], sep = "_")
    upper_temp <- paste("upper", time_pts[i], sep = "_")
    column_temp <- paste(time_pts[i], time_unit)

    temp <- wider %>% transmute({{ column_temp }} := str_c(format(round(.data[[cif_temp]], {{ round_dig }}), nsmall = {{ dec_dig }}, trim = T),
                                                           " ",
                                                           "(",
                                                           format(round(.data[[lower_temp]], {{ round_dig }}), nsmall = {{ dec_dig }}, trim = T),
                                                           "-", format(round(.data[[upper_temp]], {{ round_dig }}), nsmall = {{ dec_dig }}, trim = T),
                                                           ")"))
    bind_tab <- bind_cols(bind_tab, temp)
  }


  map_df(bind_tab, ~ str_replace(.x, "NA.*", "NA"))
}

######### Create age and sex adjusted table from data frame
#
# Dependencies : dplyr, biostat3
#
age_sex_adjust <- function(data_var,
                           group_var,
                           age_var,
                           sex_var,
                           event_var,
                           time_var){

  group_levels <- levels({{ data_var }}[[{{ group_var }}]])
  offset <- log(as.numeric(data_var[[time_var]]/365.25/100))
  formula <- formula(paste0(event_var,
                            " ~ ",
                            group_var,
                            " + ",
                            "I(",
                            age_var,
                            " - ",
                            "mean(",
                            age_var,
                            "))",
                            " + ",
                            sex_var))

  poisson_fit <- glm(formula = formula,
                     offset = offset,
                     data = data_var,
                     family = poisson)


  vector <- character(length(rownames(as.data.frame(poisson_fit$coefficients))))
  vector[1] <- "(Intercept)"

  for(i in 2:(length(rownames(as.data.frame(poisson_fit$coefficients))))){
    vector[i] <- paste(rownames(as.data.frame(poisson_fit$coefficients))[1],
                       rownames(as.data.frame(poisson_fit$coefficients))[i],
                       sep = " + ")
  }

  linc_tab <- rownames_to_column(as.data.frame(lincom(poisson_fit, vector, eform = T)), var = "var")
  linc_tab$var[1] <- group_levels[1]

  IR_tab <- tibble({{ group_var }} := group_levels,
                   IR = numeric(length(group_levels)),
                   lower = numeric(length(group_levels)),
                   upper = numeric(length(group_levels)))

  for(i in seq_along(group_levels)){
    IR_tab$IR[i] <- linc_tab[[2]][[i]]
    IR_tab$lower[i] <- linc_tab[[3]][[i]]
    IR_tab$upper[i] <- linc_tab[[4]][[i]]
  }

  IR_tab
}



######### Create age and sex adjusted table from data frame from modmarg
#
# Dependencies : dplyr, modmarg
#

age_sex_adjust2 <- function(data_var,
                            group_var,
                            age_var,
                            sex_var,
                            event_var,
                            time_var){

  group_var <- deparse(substitute(group_var))
  age_var <- deparse(substitute(age_var))
  sex_var <- deparse(substitute(sex_var))
  event_var <- deparse(substitute(event_var))
  time_var <- deparse(substitute(time_var))

  group_levels <- levels({{ data_var }}[[{{ group_var }}]])
  offset <- log(as.numeric(data_var[[time_var]]/365.25/100))
  formula <- formula(paste0(event_var,
                            " ~ ",
                            group_var,
                            " * ",
                            age_var,
                            " + ",
                            group_var,
                            " * ",
                            sex_var))

  poisson_fit <- glm(formula = formula,
                     offset = offset,
                     data = data_var,
                     family = poisson)

  marg_data <- data_var

  marg_data[time_var] <- 36525 # To get IR in marg() since offset is specified as days/365.25/100

  marg_mod <- as.data.frame(marg(mod = poisson_fit, var_interest = group_var, type = "levels", data = marg_data))

  marg_mod %>% transmute({{ group_var }} := group_levels,
                         IR = Margin,
                         lower = Lower.CI..95..,
                         upper = Upper.CI..95..)

}

######### Create age and sex adjusted table from data frame from modmarg, quadratic age
#
# Dependencies : dplyr, modmarg
#

age_sex_adjust3 <- function(data_var,
                            group_var,
                            age_var,
                            sex_var,
                            event_var,
                            time_var){

  group_var <- deparse(substitute(group_var))
  age_var <- deparse(substitute(age_var))
  sex_var <- deparse(substitute(sex_var))
  event_var <- deparse(substitute(event_var))
  time_var <- deparse(substitute(time_var))

  group_levels <- levels({{ data_var }}[[{{ group_var }}]])
  offset <- log(as.numeric(data_var[[time_var]]/365.25/100))
  formula <- formula(paste0(event_var,
                            " ~ ",
                            group_var,
                            " * ",
                            "I(",
                            age_var,
                            "^2)",
                            " + ",
                            sex_var))

  poisson_fit <- glm(formula = formula,
                     offset = offset,
                     data = data_var,
                     family = poisson)

  marg_data <- data_var

  marg_data[time_var] <- 36525 # To get IR in marg() since offset is specified as days/365.25/100

  marg_mod <- as.data.frame(marg(mod = poisson_fit, var_interest = group_var, type = "levels", data = marg_data))

  marg_mod %>% transmute({{ group_var }} := group_levels,
                         IR = Margin,
                         lower = Lower.CI..95..,
                         upper = Upper.CI..95..) %>% as_tibble()

}




######### Create age and sex adjusted table from data frame from modmarg, quadratic age
#
# Dependencies : dplyr, modmarg, stdReg
#

age_sex_adjust4 <- function(data_var,
                            group_var,
                            age_var,
                            sex_var,
                            event_var,
                            time_var){

  group_var <- deparse(substitute(group_var))
  age_var <- deparse(substitute(age_var))
  sex_var <- deparse(substitute(sex_var))
  event_var <- deparse(substitute(event_var))
  time_var <- deparse(substitute(time_var))

  group_levels <- levels({{ data_var }}[[{{ group_var }}]])
  offset <- log(as.numeric(data_var[[time_var]]/365.25/100))
  data_trans <- transform(data_var, time_var = 36525) # To get IR since offset is specified as days/362.25/100
  formula <- formula(paste0(event_var,
                            " ~ ",
                            group_var,
                            " * ",
                            "I(",
                            age_var,
                            "^2)",
                            " + ",
                            sex_var))

  poisson_fit <- glm(formula = formula,
                     offset = offset,
                     data = data_var,
                     family = poisson)


  std_fit <- stdGlm(poisson_fit, data = data_trans, X = group_var)

  std_sum <- summary(std_fit, CI.type = "log") # To correct CI use type = "log", otherwise possible with negative values

  std_sum[["est.table"]] %>%
    as_tibble(rownames = group_var) %>%
    transmute({{ group_var }} := group_levels,
              IR = Estimate,
              lower = `lower 0.95`,
              upper = `upper 0.95`)

  # marg_mod %>% transmute({{ group_var }} := group_levels,
  #                        IR = Margin,
  #                        lower = Lower.CI..95..,
  #                        upper = Upper.CI..95..) %>% as_tibble()
  #
}


