
##########################
# Project: EFGH
# Analysis: Delayed Care-seeking Correlates Manuscript
# Manuscript first author: Maureen Ndalama
# Script purpose: Configuration file
# Author: Olivia Schultes
##########################

# Clear working directory
rm(list=ls())

# Load packages
pacman::p_load(tidyverse, magrittr, here, lubridate, janitor, gtsummary, gt, epiR, 
               broom, Hmisc, 
               # interactionR, 
               sandwich, geepack, 
               # marginaleffects, 
               modelsummary, biostat3, conflicted, patchwork)

 # lmtest, parameters, ggtext

 # Specify function conflicts 
conflicts_prefer(dplyr::select, dplyr::filter)

# Set working directory
setwd(here())


# Specify functions -------


## Function to examine univariate relationships between risk factors and delayed care-seeking and create a table using gtsummary -----

make_tab_univar_primary = function(predictor) {
  
  func_dat = fig1_data %>%
    filter(!is.na(UQ(sym(predictor)))) 
  
  formula <- as.formula(paste0("delayed_careseek ~ ", predictor, " + enroll_site_fac"))
  model <- geeglm(formula, 
                  data = func_dat, 
                  id = child_id,
                  family = poisson,
                  corstr = "exchangeable",
                  std.err = "san.se",
                  scale.fix = TRUE)
  
  if(predictor=="age_fac") {label = "Age (months)"
  } else if(predictor=="sex") {label = "Sex"
  } else if(predictor=="mat_edu_fac") {label = "Maternal education"
  } else if(predictor=="cg_age_fac") {label = "Caregiver age (years)"
  } else if(predictor=="cg_employ_fac") {label = "Caregiver employment status"
  } else if(predictor=="final_quintile_fac") {label = "Wealth index"
  } else if(predictor=="aav_fac") {label = "Age-appropriate vaccination"
  } else if(predictor=="caretype_fac") {label = "Prior care-seeking"
  } else if(predictor=="preenroll_antibiotics_fac") {label = "Pre-enrollment antibiotics received"
  } else if(predictor=="time_to_facility") {label = "Time to facility (minutes)"
  } else if(predictor=="dehydration_fac") {label = "Dehydration"
  } else if(predictor=="dysentery_fac") {label = "Dysentery"
  } else if(predictor=="gems_msd_fac") {label = "GEMS-MSD"
  } else if(predictor=="mvs_fac") {label = "Modified Vesikari score (MVS)"
  } else if(predictor=="hosp_fac") {label = "Hospitalized during index episode"
  } else if(predictor=="enr_wasting_fac") {label = "Wasting"
  } else if(predictor=="enr_stunting_fac") {label = "Stunting"
  } else if(predictor=="enr_underweight_fac") {label = "Underweight"
  } else("Error: incorrect predictor argument.")
  
  tbl_univar = tbl_regression(model,
                               include = paste0(predictor),
                               exponentiate = TRUE,
                               pvalue_fun = function(x) {
                                 if_else(
                                   is.na(x),
                                   NA_character_,
                                   if_else(x < 0.001, "<0.001", format(round(x, 3), scientific = FALSE))
                                 )
                               },
                               label = predictor ~ label) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  return(tbl_univar)
}



## Function to examine multivariate relationships between risk factors and delayed care-seeking and create a table using gtsummary -----

make_tab_multivar_primary = function(predictor) {
  
  # data
  func_dat = fig1_data |>
    filter(!is.na(cg_age_fac) & !is.na(caretype_fac))
  
  # formula
  if(predictor=="age_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + cg_age_fac + caretype_fac + preenroll_antibiotics_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="cg_age_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="caretype_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + preenroll_antibiotics_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="preenroll_antibiotics_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + caretype_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="dehydration_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + caretype_fac + preenroll_antibiotics_fac + dysentery_fac"))
  } else if (predictor=="dysentery_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + caretype_fac + preenroll_antibiotics_fac + dehydration_fac"))
  } else if (predictor=="gems_msd_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + caretype_fac + preenroll_antibiotics_fac"))
  } else if (predictor=="mvs_fac") {formula = as.formula(paste("delayed_careseek ~ ", predictor, " + enroll_site_fac + age_fac + cg_age_fac + caretype_fac + preenroll_antibiotics_fac"))
  } else ("Error: incorrect predictor argument.")
  
  # model
  model <- geeglm(formula, 
                  data = func_dat, 
                  id = child_id,
                  family = poisson,
                  corstr = "exchangeable",
                  std.err = "san.se",
                  scale.fix = TRUE)
  
  # labels
  if(predictor=="age_fac") {label = "Age (months)"
  } else if(predictor=="cg_age_fac") {label = "Caregiver age (years)"
  } else if(predictor=="caretype_fac") {label = "Prior care-seeking"
  } else if(predictor=="preenroll_antibiotics_fac") {label = "Pre-enrollment antibiotics received"
  } else if(predictor=="dehydration_fac") {label = "Dehydration"
  } else if(predictor=="dysentery_fac") {label = "Dysentery"
  } else if(predictor=="gems_msd_fac") {label = "GEMS-MSD"
  } else if(predictor=="mvs_fac") {label = "Modified Vesikari score (MVS)"
  } else("Error: incorrect predictor argument.")
  
  tbl_multi = tbl_regression(model,
                             include = paste0(predictor),
                             exponentiate = TRUE,
                             pvalue_fun = function(x) {
                               if_else(
                                 is.na(x),
                                 NA_character_,
                                 if_else(x < 0.001, "<0.001", format(round(x, 3), scientific = FALSE))
                               )
                             },
                             label = predictor ~ label) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  return(tbl_multi)
}


## Function to examine univariate relationships between risk factors and secondary definition of delayed care-seeking (sensitivity analysis) and create a table using gtsummary -----

make_tab_univar_sensitivity = function(predictor) {
  
  func_dat = tab2_data %>%
    filter(!is.na(UQ(sym(predictor)))) 
  
  formula <- as.formula(paste0("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac"))
  model <- geeglm(formula, 
                  data = func_dat, 
                  id = child_id,
                  family = poisson,
                  corstr = "exchangeable",
                  std.err = "san.se",
                  scale.fix = TRUE)
  
  if(predictor=="age_fac") {label = "Age (months)"
  } else if(predictor=="sex") {label = "Sex"
  } else if(predictor=="mat_edu_fac") {label = "Maternal education"
  } else if(predictor=="cg_age_fac") {label = "Caregiver age (years)"
  } else if(predictor=="cg_employ_fac") {label = "Caregiver employment status"
  } else if(predictor=="final_quintile_fac") {label = "Wealth index"
  } else if(predictor=="aav_fac") {label = "Age-appropriate vaccination"
  } else if(predictor=="caretype_fac") {label = "Prior care-seeking"
  } else if(predictor=="preenroll_antibiotics_fac") {label = "Pre-enrollment antibiotics received"
  } else if(predictor=="time_to_facility") {label = "Time to facility (minutes)"
  } else if(predictor=="dehydration_fac") {label = "Dehydration"
  } else if(predictor=="dysentery_fac") {label = "Dysentery"
  } else if(predictor=="gems_msd_fac") {label = "GEMS-MSD"
  } else if(predictor=="mvs_fac") {label = "Modified Vesikari score (MVS)"
  } else if(predictor=="hosp_fac") {label = "Hospitalized during index episode"
  } else if(predictor=="enr_wasting_fac") {label = "Wasting"
  } else if(predictor=="enr_stunting_fac") {label = "Stunting"
  } else if(predictor=="enr_underweight_fac") {label = "Underweight"
  } else("Error: incorrect predictor argument.")
  
  tbl2_univar = tbl_regression(model, 
                               include = paste0(predictor),
                               exponentiate = TRUE,
                               pvalue_fun = function(x) {
                                 if_else(
                                   is.na(x),
                                   NA_character_,
                                   if_else(x < 0.001, "<0.001", format(round(x, 3), scientific = FALSE))
                                 )
                               },
                               label = predictor ~ label) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  return(tbl2_univar)
}


## Function to examine multivariate relationships between risk factors and secondary definition of delayed care-seeking (sensitivity analysis) and create a table using gtsummary -----

make_tab_multivar_sensitivity = function(predictor) {
  
  # data
  func_dat = tab2_data |>
    filter(!is.na(cg_age_fac) & !is.na(enr_wasting_fac))
  
  # formula
  if(predictor=="age_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + caretype_fac + preenroll_antibiotics_fac + enr_wasting_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="caretype_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + preenroll_antibiotics_fac + enr_wasting_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="preenroll_antibiotics_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + enr_wasting_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="enr_wasting_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + dehydration_fac + dysentery_fac"))
  } else if (predictor=="dehydration_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + enr_wasting_fac + dysentery_fac"))
  } else if (predictor=="dysentery_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + enr_wasting_fac + dehydration_fac"))
  } else if (predictor=="gems_msd_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + enr_wasting_fac"))
  } else if (predictor=="mvs_fac") {formula = as.formula(paste("delayed_careseek_sensitivity ~ ", predictor, " + enroll_site_fac + age_fac + caretype_fac + preenroll_antibiotics_fac + enr_wasting_fac"))
  } else ("Error: incorrect predictor argument.")
  
  # model
  model <- geeglm(formula, 
                  data = func_dat, 
                  id = child_id,
                  family = poisson,
                  corstr = "exchangeable",
                  std.err = "san.se",
                  scale.fix = TRUE)
  
  # labels
  if(predictor=="age_fac") {label = "Age (months)"
  } else if(predictor=="caretype_fac") {label = "Prior care-seeking"
  } else if(predictor=="preenroll_antibiotics_fac") {label = "Pre-enrollment antibiotics received"
  } else if(predictor=="dehydration_fac") {label = "Dehydration"
  } else if(predictor=="dysentery_fac") {label = "Dysentery"
  } else if(predictor=="gems_msd_fac") {label = "GEMS-MSD"
  } else if(predictor=="mvs_fac") {label = "Modified Vesikari score (MVS)"
  } else if(predictor=="enr_wasting_fac") {label = "Wasting"
  } else("Error: incorrect predictor argument.")
  
  tbl2_multi = tbl_regression(model, 
                              include = paste0(predictor),
                              exponentiate = TRUE,
                              pvalue_fun = function(x) {
                                if_else(
                                  is.na(x),
                                  NA_character_,
                                  if_else(x < 0.001, "<0.001", format(round(x, 3), scientific = FALSE))
                                )
                              },
                              label = predictor ~ label) %>%
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  return(tbl2_multi)
}


## Function to examine univariate relationships between risk factors and delayed care-seeking by site and create a table using gtsummary -----

make_tab_univar_site = function(predictor, site=NULL) {
  
  # data, entire dataset or just site
  if(is.null(site)) {
    func_dat = tab3_data %>%
      filter(!is.na(UQ(sym(predictor)))) 
  } else if(site=="Bangladesh" | site=="Kenya" | site=="Malawi" | site=="Peru") {
    func_dat = tab3_data %>%
      filter(!is.na(UQ(sym(predictor))) & enroll_site_fac == site) |>
      select(-mat_edu_fac) |>
      rename(mat_edu_fac = mat_edu_fac_two)
  } else {
    func_dat = tab3_data %>%
      filter(!is.na(UQ(sym(predictor))) & enroll_site_fac == site)
  }
  
  # formula, entire dataset or just site
  if(is.null(site)) {
    formula <- as.formula(paste0("delayed_careseek ~ ", predictor, " + enroll_site_fac"))
  } else {
    formula <- as.formula(paste0("delayed_careseek ~ ", predictor))
  }
  
  model <- geeglm(formula, 
                  data = func_dat, 
                  id = child_id,
                  family = poisson,
                  corstr = "exchangeable",
                  std.err = "san.se",
                  scale.fix = TRUE)
  
  if(predictor=="age_fac") {label = "Age (months)"
  } else if(predictor=="sex") {label = "Sex"
  } else if(predictor=="mat_edu_fac") {label = "Maternal education"
  } else if(predictor=="cg_age_fac") {label = "Caregiver age (years)"
  } else if(predictor=="cg_employ_fac") {label = "Caregiver employment status"
  } else if(predictor=="final_quintile_simple_fac") {label = "Wealth index, simplified"
  } else if(predictor=="aav_fac") {label = "Age-appropriate vaccination"
  } else if(predictor=="prior_careseek_fac") {label = "Prior care-seeking, binary"
  } else if(predictor=="preenroll_antibiotics_fac") {label = "Pre-enrollment antibiotics received"
  } else if(predictor=="time_to_facility") {label = "Time to facility (minutes)"
  } else if(predictor=="dehydration_fac") {label = "Dehydration"
  } else if(predictor=="dysentery_fac") {label = "Dysentery"
  } else if(predictor=="gems_msd_fac") {label = "GEMS-MSD"
  } else if(predictor=="mvs_fac") {label = "Modified Vesikari score (MVS)"
  } else if(predictor=="hosp_fac") {label = "Hospitalized during index episode"
  } else if(predictor=="enr_wasting_fac") {label = "Wasting"
  } else if(predictor=="enr_stunting_fac") {label = "Stunting"
  } else if(predictor=="enr_underweight_fac") {label = "Underweight"
  } else("Error: incorrect predictor argument.")
  
  tbl3 = tbl_regression(model, 
                        include = paste0(predictor),
                        exponentiate = TRUE,
                        # pvalue_fun = function(x) {
                        #   if_else(
                        #     is.na(x),
                        #     NA_character_,
                        #     if_else(x < 0.001, "<0.001", format(round(x, 3), scientific = FALSE))
                        #   )
                        # },
                        label = predictor ~ label) %>%
    add_significance_stars(pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
                           hide_ci = TRUE,
                           hide_se = TRUE) |>
    # modify_table_styling(column = estimate,
    #                      rows = !is.na(estimate),
    #                      cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})") %>%
    # modify_header(estimate ~ "**PR (95% CI)**") %>%
    modify_column_hide(c(ci, p.value)) |>
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
  return(tbl3)
}


## Function to apply site-specific table function to each site to enable iteration -----

make_tab_all_sites = function(site) {
  
  uni_results = lapply(predictors_univar_site, make_tab_univar_site, site)
  
  site_uvregression = tbl_stack(uni_results) %>%
    modify_header(estimate = site) %>%
    modify_footnote(c(estimate) ~ "PR = Prevalence Ratio, CI = Confidence Interval")
  
  return(site_uvregression)
}



