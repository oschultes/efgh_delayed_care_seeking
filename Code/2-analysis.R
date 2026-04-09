
##########################
# Project: EFGH
# Analysis: Delayed Care-seeking Correlates Manuscript
# Manuscript first author: Maureen Ndalama
# Script purpose: Analyze data
# Author: Olivia Schultes
##########################


source("Code/0-config.R")


# Load data -------

rds_list <- list.files(path = "Last Step Datasets/", pattern = "*.Rds", full.names = TRUE)
name_list <- sub(".Rds", "", list.files(path = "Last Step Datasets/", pattern = "*.Rds", full.names = FALSE))

for (i in 1:length(rds_list)){
  assign(name_list[i], readRDS(rds_list[i]))
}


 







# Table 1: Baseline characteristics -------

table1 = tab1_data %>% 
  tbl_summary(include = c(age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, final_quintile_fac, aav_fac,
                          caretype_fac, preenroll_antibiotics_fac, time_to_facility,
                          dehydration_fac, dysentery_fac, gems_msd_fac, mvs_fac, hosp_fac,
                          enr_wasting_fac, enr_stunting_fac, enr_underweight_fac),
              by = enroll_site_fac,
              type = list(dysentery_fac ~ "categorical", hosp_fac ~ "categorical", caretype_fac ~ "categorical"),
              label = list(age_fac ~ "Age (months)",
                           sex ~ "Sex",
                           mat_edu_fac ~ "Maternal education",
                           cg_age_fac ~ "Caregiver age (years)",
                           cg_employ_fac ~ "Caregiver employment status",
                           final_quintile_fac ~ "Wealth index",
                           aav_fac ~ "Age-appropriate vaccination",
                           caretype_fac ~ "Prior care-seeking",
                           preenroll_antibiotics_fac ~ "Pre-enrollment antibiotics received",
                           time_to_facility ~ "Time to facility (minutes)",
                           dehydration_fac ~ "Dehydration",
                           dysentery_fac ~ "Dysentery",
                           gems_msd_fac ~ "GEMS-MSD",
                           mvs_fac ~ "Modified Vesikari score (MVS)",
                           hosp_fac ~ "Hospitalized during index episode",
                           enr_wasting_fac ~ "Wasting",
                           enr_stunting_fac ~ "Stunting",
                           enr_underweight_fac ~ "Underweight"),
              digits = list(all_categorical() ~ c(0, 1))) %>%
  add_overall(last = TRUE)


## Export -----

# table1 %>%
#   as_gt() %>%
#   gtsave(., "Results/Table1.rtf")








# Figure 1: Risk factors of delayed care-seeking, primary outcome definition -------

# Figure used in manuscript, but both table and figure coded here 


## Figure 1, table version - Summary (n (%)) -----

fig1_tab_summary = fig1_data %>% 
  tbl_summary(include = c(age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, final_quintile_fac, aav_fac, 
                          caretype_fac, preenroll_antibiotics_fac, time_to_facility,
                          dehydration_fac, dysentery_fac, gems_msd_fac, mvs_fac, hosp_fac,
                          enr_wasting_fac, enr_stunting_fac, enr_underweight_fac),
              by = delayed_careseek_fac,
              type = list(dysentery_fac ~ "categorical", hosp_fac ~ "categorical", caretype_fac ~ "categorical", preenroll_antibiotics_fac ~ "categorical"),
              label = list(age_fac ~ "Age (months)",
                           sex ~ "Sex",
                           mat_edu_fac ~ "Maternal education",
                           cg_age_fac ~ "Caregiver age (years)",
                           cg_employ_fac ~ "Caregiver employment status",
                           final_quintile_fac ~ "Wealth index",
                           caretype_fac ~ "Prior care-seeking",
                           preenroll_antibiotics_fac ~ "Pre-enrollment antibiotics received",
                           time_to_facility ~ "Time to facility (minutes)",
                           aav_fac ~ "Age-appropriate vaccination",
                           dehydration_fac ~ "Dehydration",
                           dysentery_fac ~ "Dysentery",
                           gems_msd_fac ~ "GEMS-MSD",
                           mvs_fac ~ "Modified Vesikari score (MVS)",
                           hosp_fac ~ "Hospitalized during index episode",
                           enr_wasting_fac ~ "Wasting",
                           enr_stunting_fac ~ "Stunting",
                           enr_underweight_fac ~ "Underweight"),
              statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 0, 0))
              ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  remove_row_type(type = "missing")




## Figure 1, table version - Univariate regression -----


# loop through predictors
predictors_univar_fig1 = c("age_fac", "sex", "mat_edu_fac", "cg_age_fac", "cg_employ_fac", "final_quintile_fac", "aav_fac", 
               "caretype_fac", "preenroll_antibiotics_fac", "time_to_facility",
               "dehydration_fac", "dysentery_fac", "gems_msd_fac", "mvs_fac", "hosp_fac", 
               "enr_wasting_fac", "enr_stunting_fac", "enr_underweight_fac")

univariate_results_fig1 <- lapply(predictors_univar_fig1, make_tab_univar_primary)


# combine results in one table
fig1_tab_uvregression = tbl_stack(univariate_results_fig1) %>%
  modify_header(estimate = "**PR**") %>%
  modify_footnote(c(estimate, conf.low) ~ "PR = Prevalence Ratio, CI = Confidence Interval")




## Figure 1, table version - Multivariate regression -----

# predictors with p < 0.02: age, caregiver age, prior care-seeking, pre-enrollment antibiotics, dehydration, dysentery, gems-msd, mvs


# loop through predictors
predictors_multivar_fig1 = c("age_fac", "cg_age_fac", "caretype_fac", "preenroll_antibiotics_fac", "dehydration_fac", "dysentery_fac", "gems_msd_fac", "mvs_fac")

multivariate_results_fig1 <- lapply(predictors_multivar_fig1, make_tab_multivar_primary)


# combine results in one table
fig1_tab_mvregression = tbl_stack(multivariate_results_fig1) %>%
  modify_header(estimate = "**PR**") %>%
  modify_footnote(c(estimate, conf.low) ~ "PR = Prevalence Ratio, CI = Confidence Interval")




## Figure 1, table version - Merge and export final table -----

# Option to export intermediate table containing figure 1 results

fig1_table = tbl_merge(list(fig1_tab_summary, fig1_tab_uvregression, fig1_tab_mvregression),
                   tab_spanner = FALSE) |>
  remove_abbreviation()



# Finalize & export
fig1_table_gt = fig1_table |>
  as_gt() |>
  tab_spanner(label = "Univariate",
              columns = c(estimate_2, conf.low_2, p.value_2)) |>
  tab_spanner(label = "Multivariate",
              columns = c(estimate_3, conf.low_3, p.value_3)) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_spanners()) |>
  tab_footnote(footnote = "Univariate models adjusted for enrollment site.",
               locations = cells_column_spanners(spanners = starts_with("uni"))) |>
  tab_footnote(footnote = "Multivariate models included enrollment site and predictors with p < 0.02 in univariate models. To avoid collinearity between severity indicators, multivariate models adjusted by dehydration and dysentery by default. 
               Models for GEMS-MSD and MVS did not include other severity indicators.",
               locations = cells_column_spanners(spanners = starts_with("mult")))
  
# Export
# fig1_table_gt %>%
#   gtsave(., "Results/Figure1_table.rtf")





## Figure 1 - Forest plot -----


# Rearrange data
fig1_prep_data = fig1_table_gt$`_data` |>
  select(var_label, label, estimate_2, conf.low_2, conf.high_2, ci_2, p.value_2, estimate_3, conf.low_3, conf.high_3, ci_3, p.value_3) |>
  # remove variable names & reference groups for binary predictors
  filter(!(label %in% c("Sex", "Female", "Caregiver employment status", "No formal employment", "Age-appropriate vaccination", "Has not received",
                        "Pre-enrollment antibiotics received", "Dysentery", "GEMS-MSD", "Less-severe", "Hospitalized during index episode",
                        "Stunting", "Not stunted", "No"))) |>
  # re-code labels so they make sense without variable names
  mutate(label = case_when(label=="Age (months)" ~ "`Age (months)`",
                           label=="Male" ~ "`Male`",
                           label=="Caregiver age (years)" ~ "`Caregiver age (years)`",
                           label=="6-11" ~ "`    6-11`",
                           label=="12-23" ~ "`    12-23`",
                           label=="24-35" ~ "`    24-35`",
                           label=="Maternal education" ~ "`Maternal education`",
                           label=="Primary school or less" ~ "`    Primary school or less`",
                           label=="Some secondary or greater" ~ "`    Some secondary or greater`",
                           label=="Koranic school only" ~ "`    Koranic school only`",
                           label=="<20" ~ "`    <20`",
                           label=="20-29" ~ "`    20-29`",
                           label=="30-39" ~ "`    30-39`",
                           label==">=40" ~ "`    >=40`",
                           label=="Wealth index" ~ "`Wealth index`",
                           label=="Quintile 1 (fewest assets)" ~ "`    Quintile 1 (fewest assets)`",
                           label=="Quintile 2" ~ "`    Quintile 2`",
                           label=="Quintile 3" ~ "`    Quintile 3`",
                           label=="Quintile 4" ~ "`    Quintile 4`",
                           label=="Quintile 5 (most assets)" ~ "`    Quintile 5 (most assets)`",
                           label=="Prior care-seeking" ~ "`Prior care-seeking`",
                           label=="None" & var_label=="Prior care-seeking" ~ "`    No prior care-seeking`",
                           label=="Medically-attended\n diarrhea" ~ "`    Medically-attended diarrhea`",
                           label=="Community health worker/\nHealth outpost" ~ "`    Community health worker/ Health outpost`",
                           label=="Pharmacy/Drug seller" ~ "`    Pharmacy/Drug seller`",
                           label=="Traditional healer" ~ "`    Traditional healer`",
                           label=="Other" ~ "`    Other`",
                           label=="Time to facility (minutes)" ~ "`Time to facility (minutes)`",
                           label=="Formal employment" ~ "`Caregiver has formal employment`",
                           label=="Received" ~ "`Age-appropriate vaccines received`",
                           label=="Yes" & var_label=="Pre-enrollment antibiotics received" ~ "`Received antibiotics pre-enrollment`",
                           label=="Dehydration" ~ "`Dehydration`",
                           label=="None" & var_label=="Dehydration" ~ "`    No dehydration`",
                           label=="Some" & var_label=="Dehydration" ~ "`    Some dehydration`",
                           label=="Severe" & var_label=="Dehydration" ~ "`    Severe dehydration`",
                           label=="Yes" & var_label=="Dysentery" ~ "`Dysentery`",
                           label=="Moderate-to-severe" ~ "`GEMS-MSD`",
                           label=="Modified Vesikari score (MVS)" ~ "`Modified Vesikari score (MVS)`",
                           label=="Mild" & var_label=="Modified Vesikari score (MVS)" ~ "`    Mild`",
                           label=="Moderate" & var_label=="Modified Vesikari score (MVS)" ~ "`    Moderate`",
                           label=="Severe" & var_label=="Modified Vesikari score (MVS)" ~ "`    Severe`",
                           label=="Yes" & var_label=="Hospitalized during index episode" ~ "`Hospitalized`",
                           label=="Wasting" ~ "`Wasting`",
                           label=="None" & var_label=="Wasting" ~ "`    No wasting`",
                           label=="Moderate" & var_label=="Wasting" ~ "`    Moderate wasting`",
                           label=="Severe" & var_label=="Wasting" ~ "`    Severe wasting`",
                           label=="Stunted" ~ "`Stunted`",
                           label=="Underweight" ~ "`Underweight`",
                           label=="None" & var_label=="Underweight" ~ "`    Not underweight`",
                           label=="Moderate" & var_label=="Underweight" ~ "`    Moderately underweight`",
                           label=="Severe" & var_label=="Underweight" ~ "`    Severely underweight`",
                           .default = label)) |>
  mutate(univar_estimate_lab=paste0(format(round(estimate_2,2), nsmall=2)," (",format(round(conf.low_2,2), nsmall=2),", ",format(round(conf.high_2,2), nsmall=2),")"),
         univar_estimate_lab=ifelse(label %in% c("`    Not underweight`", "`    No wasting`", "`    Mild`", "`    No dehydration`", "`    No prior care-seeking`", "`    Quintile 1 (fewest assets)`", 
                                                 "`    <20`", "`    Primary school or less`", "`    6-11`"), "Ref", 
                             ifelse(estimate_2=="NA (NA, NA)", NA, univar_estimate_lab)),
         multivar_estimate_lab=paste0(format(round(estimate_3,2), nsmall=2)," (",format(round(conf.low_3,2), nsmall=2),", ",format(round(conf.high_3,2), nsmall=2),")"),
         multivar_estimate_lab = ifelse(label %in% c("`    Mild`", "`    No dehydration`", "`    No prior care-seeking`", "`    <20`", "`    6-11`"), "Ref",
                                        ifelse(multivar_estimate_lab=="  NA (  NA,   NA)", NA, multivar_estimate_lab)),
         univar_pvalue_lab = ifelse(p.value_2<0.001, "<0.001", format(round(p.value_2, 3), nsmall=2)),
         univar_pvalue_lab = ifelse(p.value_2<0.05, paste0(univar_pvalue_lab,"*"), univar_pvalue_lab),
         multivar_pvalue_lab = ifelse(p.value_3<0.001, "<0.001", format(round(p.value_3, 3), nsmall=2)),
         multivar_pvalue_lab = ifelse(p.value_3<0.05, paste0(multivar_pvalue_lab,"*"), multivar_pvalue_lab)) %>%
  add_row(label="`Risk factor`", 
          univar_estimate_lab="Univariate PR (95% CI)",
          multivar_estimate_lab="Multivariate PR (95% CI)",
          univar_pvalue_lab = "p-value",
          multivar_pvalue_lab = "p-value",
          .before = 1) |>
  mutate(bold_variable_label = ifelse(label=="`Risk factor`", "Risk factor", NA)) |>
  mutate(fontface_factor = ifelse(bold_variable_label == "Risk factor" & !is.na(bold_variable_label), "bold", "plain"),
         fontface_estimate = ifelse(univar_estimate_lab == "Univariate PR (95% CI)" & !is.na(univar_estimate_lab), "bold", "plain"),
         fontface_pvalue = ifelse(univar_pvalue_lab == "p-value" & !is.na(univar_pvalue_lab), "bold", "plain")) |>
  arrange(-dplyr::row_number()) |>
  mutate(label = factor(label, label))
  


### Create each piece of the plot ---

# Predictor names and univariate PRs (95% CIs)
fig1_univar_pr <-
  ggplot(fig1_prep_data, aes(y = label)) +
  geom_text(data = fig1_prep_data,
            aes(x = 0, y = label, label = bold_variable_label, fontface = fontface_factor), hjust = 0) +
  geom_text(data = fig1_prep_data |>  filter(is.na(bold_variable_label)),
            aes(x = 0, y = label, label = label), hjust = 0, parse = TRUE) +
  geom_text(aes(x = 5, label = univar_estimate_lab, fontface = fontface_estimate), hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(0, 7))

# Univariate forest plot
fig1_univar_forest <- fig1_prep_data |>
  ggplot(aes(y = label)) + 
  theme_classic() +
  geom_point(aes(x=estimate_2), shape=19, size=3) +
  geom_linerange(aes(xmin=conf.low_2, xmax=conf.high_2)) +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="Prevalence Ratio", y="") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())

# Univariate p-values
fig1_univar_pval <- fig1_prep_data  |>
  ggplot() +
  geom_text(aes(x = 0, y = label, label = univar_pvalue_lab, fontface = fontface_pvalue),
            hjust = 0) +
  theme_void() 


# Multivariate PRs (95% CIs)
fig1_multivar_pr <-
  ggplot(fig1_prep_data, aes(y = label)) +
  geom_text(aes(x = 0, label = multivar_estimate_lab, fontface = fontface_estimate), hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(0, 7))

# Multivariate forest plot
fig1_multivar_forest <- fig1_prep_data |>
  ggplot(aes(y = label)) + 
  theme_classic() +
  geom_point(aes(x=estimate_3), shape=19, size=3) +
  geom_linerange(aes(xmin=conf.low_3, xmax=conf.high_3)) +
  geom_vline(xintercept = 1, linetype="dashed") +
  labs(x="Prevalence Ratio", y="") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())

# Multivariate p-values
fig1_multivar_pval <- fig1_prep_data |>
  ggplot() +
  geom_text(aes(x = 0, y = label, label = multivar_pvalue_lab, fontface = fontface_pvalue),
            hjust = 0) +
  theme_void() 


# Set layout 
fig1_layout <- c(patchwork::area(t = 0, l = 0, b = 30, r = 8), 
                 patchwork::area(t = 1, l = 9, b = 30, r = 13),
                 patchwork::area(t = 0, l = 13, b = 30, r = 14),
                 
                 patchwork::area(t = 0, l = 15, b = 30, r = 17),
                 patchwork::area(t = 1, l = 18, b = 30, r = 22),
                 patchwork::area(t = 0, l = 22, b = 30, r = 23))

### Combine plots ---
fig1_plot <- fig1_univar_pr + fig1_univar_forest + fig1_univar_pval + fig1_multivar_pr + 
  fig1_multivar_forest + fig1_multivar_pval + plot_layout(design = fig1_layout)

# Export
# ggsave(paste0("Results/Figure1.tiff"), width = 15, height = 8, compression = "lzw")






# Table 2: Risk factors of delayed care-seeking, sensitivity analysis -------


## Table 2 - Summary (n (%)) -----
table2_summary = tab2_data %>% 
  tbl_summary(include = c(age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, final_quintile_fac, aav_fac, 
                          caretype_fac, preenroll_antibiotics_fac, time_to_facility,
                          dehydration_fac, dysentery_fac, gems_msd_fac, mvs_fac, hosp_fac,
                          enr_wasting_fac, enr_stunting_fac, enr_underweight_fac),
              by = delayed_careseek_sensitivity_fac,
              type = list(dysentery_fac ~ "categorical", hosp_fac ~ "categorical", caretype_fac ~ "categorical", preenroll_antibiotics_fac ~ "categorical"),
              label = list(age_fac ~ "Age (months)",
                           sex ~ "Sex",
                           mat_edu_fac ~ "Maternal education",
                           cg_age_fac ~ "Caregiver age (years)",
                           cg_employ_fac ~ "Caregiver employment status",
                           final_quintile_fac ~ "Wealth index",
                           caretype_fac ~ "Prior care-seeking",
                           preenroll_antibiotics_fac ~ "Pre-enrollment antibiotics received",
                           time_to_facility ~ "Time to facility (minutes)",
                           aav_fac ~ "Age-appropriate vaccination",
                           dehydration_fac ~ "Dehydration",
                           dysentery_fac ~ "Dysentery",
                           gems_msd_fac ~ "GEMS-MSD",
                           mvs_fac ~ "Modified Vesikari score (MVS)",
                           hosp_fac ~ "Hospitalized during index episode",
                           enr_wasting_fac ~ "Wasting",
                           enr_stunting_fac ~ "Stunting",
                           enr_underweight_fac ~ "Underweight"),
              statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = list(all_categorical() ~ c(0, 0, 0))
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  remove_row_type(type = "missing")



## Table 2 - Univariate regression -----

# loop through predictors
predictors_univar_tab2 = c("age_fac", "sex", "mat_edu_fac", "cg_age_fac", "cg_employ_fac", "final_quintile_fac", "aav_fac", 
                      "caretype_fac", "preenroll_antibiotics_fac", "time_to_facility",
                      "dehydration_fac", "dysentery_fac", "gems_msd_fac", "mvs_fac", "hosp_fac", 
                      "enr_wasting_fac", "enr_stunting_fac", "enr_underweight_fac")

univariate_results_tab2 <- lapply(predictors_univar_tab2, make_tab_univar_sensitivity)


# combine results in one table

table2_uvregression = tbl_stack(univariate_results_tab2) %>%
  modify_header(estimate = "**PR**") %>%
  modify_footnote(c(estimate, conf.low) ~ "PR = Prevalence Ratio, CI = Confidence Interval")




## Table 2 - Multivariate regression -----

# predictors with p < 0.02: age, prior care-seeking, pre-enrollment antibiotics, wasting, dehydration, dysentery, gems-msd, mvs

# loop through predictors
predictors_multivar_tab2 = c("age_fac", "caretype_fac", "preenroll_antibiotics_fac", "dehydration_fac", "dysentery_fac", "gems_msd_fac", "mvs_fac", "enr_wasting_fac")

multivariate_results_tab2 <- lapply(predictors_multivar_tab2, make_tab_multivar_sensitivity)

# combine results in one table
table2_mvregression = tbl_stack(multivariate_results_tab2) %>%
  modify_header(estimate = "**PR**") %>%
  modify_footnote(c(estimate, conf.low) ~ "PR = Prevalence Ratio, CI = Confidence Interval")



## Table 2 - Merge and export final table -----
table2 = tbl_merge(list(table2_summary, table2_uvregression, table2_mvregression),
                   tab_spanner = FALSE) |>
  remove_abbreviation()


# Finalize & export
tab2_gt = table2 |>
  as_gt() |>
  tab_spanner(label = "Univariate",
              columns = c(estimate_2, conf.low_2, p.value_2)) |>
  tab_spanner(label = "Multivariate",
              columns = c(estimate_3, conf.low_3, p.value_3)) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_spanners()) |>
  tab_footnote(footnote = "Univariate models adjusted for enrollment site.",
               locations = cells_column_spanners(spanners = starts_with("uni"))) |>
  tab_footnote(footnote = "Multivariate models included enrollment site and predictors with p < 0.02 in univariate models. To avoid collinearity between severity indicators, multivariate models adjusted by dehydration and dysentery by default. 
               Models for GEMS-MSD and MVS did not include other severity indicators.",
               locations = cells_column_spanners(spanners = starts_with("mult")))

# Export
# tab2_gt %>%
#   gtsave("Results/Table2.rtf")









# Table 3: Univariate risk factors of delayed care-seeking, site-specific analysis -------

# predictors list
predictors_univar_site = c("age_fac", "sex", "mat_edu_fac", "cg_age_fac", "cg_employ_fac", "final_quintile_simple_fac", "aav_fac", 
                           "prior_careseek_fac", "preenroll_antibiotics_fac", "time_to_facility",
                           "dehydration_fac", "dysentery_fac", "gems_msd_fac", "mvs_fac", "hosp_fac", 
                           "enr_wasting_fac", "enr_stunting_fac", "enr_underweight_fac")

sites = c("Bangladesh", "Kenya", "Malawi", "Mali", "Pakistan", "Peru", "The Gambia")


# loop through sites and create tables by site
make_tab_all_sites = function(site) {
  
  uni_results = lapply(predictors_univar_site, make_tab_univar_site, site)
  
  site_uvregression = tbl_stack(uni_results) %>%
    modify_header(estimate = site) %>%
    modify_footnote(c(estimate) ~ "PR = Prevalence Ratio, CI = Confidence Interval")
  
  return(site_uvregression)
}

site_tables = lapply(sites, make_tab_all_sites)


# exporting tables - all sites

table3 = tbl_merge(site_tables,
                 tab_spanner = FALSE,
                 quiet = TRUE) |>
  remove_abbreviation() |>
  modify_header(estimate_1 = "**Bangladesh**",
                estimate_2 = "**Kenya**",
                estimate_3 = "**Malawi**",
                estimate_4 = "**Mali**",
                estimate_5 = "**Pakistan**",
                estimate_6 = "**Peru**",
                estimate_7 = "**The Gambia**") |>
  modify_spanning_header(starts_with("estimate_") ~ "**PR (95% CI)**") |>
  modify_footnote(everything() ~ NA) |>
  modify_table_body(~ .x %>% 
                      dplyr::slice(1:10, 60, 11:59))


# convert to gt and export
tab3_gt = table3 |>
  as_gt() |>
  tab_footnote(footnote = "PR = Prevalence Ratio, CI = Confidence Interval",
               locations = cells_column_spanners()) |>
  tab_footnote(footnote = "*p<0.05; **p<0.01; ***p<0.001",
               locations = cells_column_spanners())

# export
# tab3_gt |>
#   gtsave("Results/Table3.rtf")







# Additional summaries for manuscript text -------

# Prevalence of care-seeking

fig1_data |>
  tabyl(delayed_careseek_fac, enroll_site_fac) |> 
  adorn_totals("col") |>
  adorn_percentages("col") |>
  adorn_pct_formatting(digits = 1) |>
  adorn_ns(position = "front")

tab2_data |>
  tabyl(delayed_careseek_sensitivity_fac, enroll_site_fac) |>
  adorn_totals("col") |>
  adorn_percentages("col") |>
  adorn_pct_formatting(digits = 1) |>
  adorn_ns(position = "front")

