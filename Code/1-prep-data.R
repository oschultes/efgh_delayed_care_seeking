
##########################
# Project: EFGH
# Analysis: Delayed Care-seeking Correlates Manuscript
# Manuscript first author: Maureen Ndalama
# Script purpose: Prepare data for analysis
# Author: Olivia Schultes
##########################


source("Code/0-config.R")



# Load data -------
raw_data = read.csv("Raw Data/MaureenNdalma_enroll_FINAL.csv")
preenroll_antibiotics = read.csv("Raw Data/MaureenNdalma_preenroll_antibiotics_FINAL.csv")




# Prepare delayed careseeking data -------

delay_careseek1 = raw_data %>%
  select(!starts_with("enroll"), enroll_site, enroll_date, enroll_cg_moth_ed, enroll_cg_age, enroll_cg_work, enroll_cond_comor___2, enroll_hh_diag___5, 
         enroll_hs_beh, enroll_hs_beh_wh___1, enroll_hs_beh_wh___2, enroll_hs_beh_wh___3, enroll_hs_beh_wh___4, enroll_hs_beh_wh___5, enroll_hs_beh_wh___6,
         enroll_hs_beh_wh___7, enroll_hs_beh_wh___8, enroll_hs_beh_wh___9, enroll_cost_time_unit, enroll_cost_time_day, enroll_cost_tim_hr, enroll_cost_tim_min) %>%
  select(!starts_with("scr"), scr_diar_when) %>%

  # factor variables
  mutate(age_fac = factor(if_else(enr_age_months<12 & !is.na(enr_age_months), 1, 
                              if_else(enr_age_months>=12 & enr_age_months<24 & !is.na(enr_age_months), 2,
                                      if_else(enr_age_months>=24, 3, NA))), 
                      levels = c(1:3), labels = c("6-11", "12-23", "24-35")),
         sex_fac = factor(if_else(sex=="Female", 1,
                                  if_else(sex=="Male", 2, NA)),
                          levels = c(1:2), labels = c("Female", "Male")),
         cg_age_fac = factor(if_else(enroll_cg_age<20 & !is.na(enroll_cg_age), 1,
                                 if_else(enroll_cg_age>=20 & enroll_cg_age<30 & !is.na(enroll_cg_age), 2,
                                         if_else(enroll_cg_age>=30 & enroll_cg_age<40 & !is.na(enroll_cg_age), 3,
                                                 if_else(enroll_cg_age>=30 & !is.na(enroll_cg_age) & enroll_cg_age!=99, 4, NA)))),
                         levels = c(1:4), labels = c("<20", "20-29", "30-39", ">=40"), exclude = NULL),
         mat_edu_fac = factor(if_else(enroll_cg_moth_ed %in% c(1:3), 1,
                                  ifelse(enroll_cg_moth_ed %in% c(4,5), 2, 
                                         ifelse(enroll_cg_moth_ed==7, 3, NA))), 
                          levels = c(1:3), labels = c("Primary school or less", "Some secondary or greater", "Koranic school only"), exclude = NULL),
         mat_edu_fac_two = factor(if_else(enroll_cg_moth_ed %in% c(1:3), 1,
                                          ifelse(enroll_cg_moth_ed %in% c(4,5), 2, NA)), 
                                  levels = c(1:2), labels = c("Primary school or less", "Some secondary or greater"), exclude = NULL),
         cg_employ_fac = factor(if_else(enroll_cg_work %in% c(1:3,10), 0, 
                                    if_else(enroll_cg_work %in% c(4:8), 1, NA)),
                            levels = c(0,1), labels = c("No formal employment", "Formal employment"), exclude = NULL),
         enr_wasting_fac = factor(if_else(enr_wasting=="None" & !is.na(enr_wasting), 1,
                                      if_else(enr_wasting=="Moderate" & !is.na(enr_wasting), 2,
                                              if_else(enr_wasting=="Severe" & !is.na(enr_wasting), 3, NA))), 
                              levels = c(1:3), labels = c("None", "Moderate", "Severe"), exclude = NULL),
         enr_stunting_fac = factor(if_else(enr_stunting=="Not Stunted" & !is.na(enr_stunting), 1,
                                       ifelse(enr_stunting=="Stunted" & !is.na(enr_stunting), 2, NA)), 
                               levels = c(1:2), labels = c("Not stunted", "Stunted"), exclude = NULL),
         enr_underweight_fac = factor(if_else(enr_underweight=="None" & !is.na(enr_underweight), 1,
                                          if_else(enr_underweight=="Moderate" & !is.na(enr_underweight), 2,
                                                  if_else(enr_underweight=="Severe" & !is.na(enr_underweight), 3, NA))), 
                                  levels = c(1:3), labels = c("None", "Moderate", "Severe"), exclude = NULL),
         dysentery_fac = factor(dysentery, levels = c(0,1), labels = c("No", "Yes")),
         dehydration_fac = factor(if_else(dehydration=="None", 1,
                                          if_else(dehydration=="Some", 2,
                                                  if_else(dehydration=="Severe", 3, NA))), 
                                  levels = c(1:3), labels = c("None", "Some", "Severe")),
         gems_msd_fac = factor(gems_msd, 
                               levels = c("Less-severe", "Moderate-to-severe"),
                               labels = c("Less-severe", "Moderate-to-severe")),
         mvs_fac = factor(if_else(mvs_classification=="Mild", 1,
                                      ifelse(mvs_classification=="Moderate", 2, 
                                             ifelse(mvs_classification=="Severe", 3, NA))),
                              levels = c(1:3), labels = c("Mild", "Moderate", "Severe"), exclude = NULL),
         hosp_fac = factor(hosp_during_episode, levels = c(0,1), labels = c("No", "Yes")),
         aav_fac = factor(aav, levels = c(0,1), labels = c("Has not received", "Received"), exclude = NULL),
         final_quintile_fac = factor(final_quintile, 
                                     levels = c("Quintile 1 (fewest assets)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (most assets)"),
                                     labels = c("Quintile 1 (fewest assets)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (most assets)")),
         final_quintile_simple_fac = factor(ifelse(final_quintile=="Quintile 1 (fewest assets)" | final_quintile=="Quintile 2", 1, 2),
                                            levels = c(1:2), 
                                            labels = c("Less wealth",  "More wealth")),
         enroll_site_fac = factor(enroll_site, levels = c(1:7), labels = c("Bangladesh", "Kenya", "Malawi", "Mali", "Pakistan", "Peru", "The Gambia")),
         prior_careseek_fac = factor(enroll_hs_beh, levels = c(0,1), labels = c("No", "Yes"), exclude = NULL),
         episode_duration_fac = factor(ifelse(episode_duration_length<=3, 1,
                                              ifelse(episode_duration_length==4 | episode_duration_length==5, 2,
                                                     ifelse(episode_duration_length==6 | episode_duration_length==7, 3,
                                                            ifelse(episode_duration_length>7 & episode_duration_length<=14, 4,
                                                                   ifelse(episode_duration_length>14, 5, NA))))),
                                       levels = c(1:5), labels = c("1-3 days", "4-5 days", "6-7 days", "8-14 days", ">14 days"), exclude = NULL)) %>%
  
  # continuous variables
  mutate(time_to_facility = if_else(enroll_cost_time_unit==3, enroll_cost_tim_min,
                                    if_else(enroll_cost_time_unit==2, enroll_cost_tim_hr*60,
                                            if_else(enroll_cost_time_unit==1, enroll_cost_time_day*1440, NA))),
         
         # flag cases where time information is entered in incorrect unit variable (to be marked as unknown for analysis)
         error_time_to_facility = ifelse(enroll_cost_time_unit==3 & (!is.na(enroll_cost_tim_hr) | !is.na(enroll_cost_time_day)), 1,
                                         ifelse(enroll_cost_time_unit==2 & (!is.na(enroll_cost_tim_min) | !is.na(enroll_cost_time_day)), 1,
                                                ifelse(enroll_cost_time_unit==1 & (!is.na(enroll_cost_tim_min) | !is.na(enroll_cost_tim_hr)), 1, 0))),
         time_to_facility = ifelse(error_time_to_facility==1 & !is.na(error_time_to_facility), NA, time_to_facility),
         days_to_seek_care = as.numeric(difftime(as.Date(enroll_date), as.Date(scr_diar_when), units = "days"))) %>%
  
  # define outcome variables
  mutate(delayed_careseek = if_else(days_to_seek_care<=1, 0, 1),
         delayed_careseek_fac = factor(delayed_careseek, levels = c(1,0), labels = c("Delayed care-seeking", "No delayed care-seeking")),
         delayed_careseek_sensitivity = if_else(days_to_seek_care<=3, 0, 1),
         delayed_careseek_sensitivity_fac = factor(delayed_careseek_sensitivity, levels = c(1,0), labels = c("Delayed care-seeking", "No delayed care-seeking")))




# Prior careseeking categorical variable -------


# coded hierarchically

# 0 - no prior care-seeking
# 1 - medically-attended diarrhea (if any MAD -> coded as MAD)
# 2 - community health worker OR health outpost at site that does not consider health outpost to be medically-attended diarrhea (if any community health and no MAD -> community health)
# 3 - drug seller or pharmacist (if any pharmacy and no community health/MAD -> pharmacy)
# 4 - traditional or religious healer (if any traditional healer and no pharmacy/community health/MAD -> traditional healer)
# 5 - other (if other and no traditional/pharmacy/community health/MAD -> other)


# assumptions for sorting write-in care-seeking options

# any type of care-seeking that was already confirmed by site to be medically attended (from PE/HUS) classified as medically-attended
    # following sites considered health outpost as "medically-attended diarrhea": Mali, The Gambia, Peru, Pakistan 
# hospital and variations as medically-attended
# pediatrician and variations as medically-attended
# emergency and ER as medically-attended
# clinic/private clinic/private and variations as health outpost/chv
# health surveillance assistant/medical personal/medical representative as health outpost/chv
# bodega and grocery store classified as pharmacy/drug seller
# homeopathic and variations as traditional/religious
# family, friend or neighbor leave as "other"
# taking medicine from prior health visit marked as "no prior care-seeking"



# 2 pids missing this enrollment form

careseek = raw_data %>%
  select(pid, enroll_site, enroll_hs_beh, starts_with("enroll_hs_beh_wh"), enroll_hs_beh_oth,
         enroll_hs_beh_th, enroll_hs_beh_rh, enroll_hs_beh_ds, enroll_hs_beh_pharm, enroll_hs_beh_chw,
         enroll_hs_beh_heout, enroll_hs_beh_hefac_out, enroll_hs_beh_hefac_in, enroll_hs_beh_oth_num) %>%
  mutate(caretype = case_when(enroll_hs_beh_wh___7==1 | enroll_hs_beh_wh___8==1 ~ 1,
                              enroll_hs_beh_wh___6==1 & (enroll_site==4 | enroll_site==5 | enroll_site==6 | enroll_site==7) ~ 1,
                              enroll_hs_beh_wh___5==1 ~ 2,
                              enroll_hs_beh_wh___6==1 & (enroll_site==1 | enroll_site==2 | enroll_site==3) ~ 2,
                              enroll_hs_beh_wh___3==1 | enroll_hs_beh_wh___4==1 ~ 3,
                              enroll_hs_beh_wh___1==1 | enroll_hs_beh_wh___2==1 ~ 4,
                              enroll_hs_beh_wh___9==1 ~ 5,
                              enroll_hs_beh==0 ~ 0,
                              TRUE ~ NA)) %>%
  mutate(caretype = case_when(enroll_hs_beh_oth %in% c("EFGH FIELD CLINIC", "EFGH FIELD OFFICE ", "al khidmat", "Ali akbar Shah aga Khan center", "Landhi Hospital",
                                                       "Private hospital", "emergency ", "ER", "PASÓ CONSULTA CON EL PEDIATRA", "PEDIATRA", "PEDIATRA PARTICULAR", 
                                                       "Malabada Health centre", "Dr ehtisham clinic ", "Dr eshwar", "Dr. Afraz Ahmed Clinic", "fatimiya hospital", 
                                                       "Ali akbar Shah aga Khan center", "ESSALUD", "HAI - HOSPITAL APOYO IQUITOS", "MORONACOCHA") ~ 1,
                              enroll_hs_beh_oth %in% c("COMMUNITY HEALTH VOLUNTEER", "village health worker", "Health surveillance assistant", "Medical personal",
                                                       "MEDICAL REPRESENTATIVE ", "clinic", "Clinique ", "private", "private clinic", "private clinic ", "CONSULTORIO PARTICULAR",
                                                       "Medical clinic ", "MEDICINA PARTICULAR", "PARTICULAR(MEDICINA)") ~ 2,
                              enroll_hs_beh_oth %in% c("COMPRA EN FARMACIA", "BODEGA", "COMPRO EN LA BODEGA", "COMPRO MEDICAMENTO EN BODEGA", "FARMACIA", "Grocery store",
                                                       "Pharmacy ") ~ 3,
                              enroll_hs_beh_oth %in% c("HOMEOPATHI", "Homeopathic Clinic") ~ 4,
                              enroll_hs_beh_oth %in% c("Had in store already from previous visit at MOH", "Had some instock at home", "Had the medication from previous visit at MOH",
                                                       "Home , had zinc ", "Home made ORS", "left over meds", "Médicaments existants ", "MEDICINA QUE TIENE EN CASA", 
                                                       "Mother gave child ORS previous visit", "Self medication ", "SELF MEDICATION FROM LEFT OVER ") ~ 0,
                              TRUE ~ caretype)) %>%
  mutate(caretype_fac = factor(caretype, levels = c(0:5), labels = c("None", "Medically-attended\n diarrhea", "Community health worker/\nHealth outpost", 
                                                                     "Pharmacy/Drug seller", "Traditional healer", "Other"))) %>%
  select(pid, caretype, caretype_fac)




# Pre-enrollment antibiotics -------

# antibiotics prescribed prior to enrollment care-seeking

drug = preenroll_antibiotics %>%
  select(pid, drug) %>%
  group_by(pid) %>%
  mutate(n = row_number()) %>%
  ungroup %>%
  pivot_wider(id_cols = pid, values_from = drug, names_from = n, names_prefix = "drug_")

drug2 = raw_data %>%
  select(pid) %>%
  left_join(drug, by = "pid") %>%
  mutate(preenroll_antibiotics_ind = ifelse(!is.na(drug_1), 1, 0),
         preenroll_antibiotics_fac = factor(preenroll_antibiotics_ind, levels = c(0,1), labels = c("No", "Yes")))




# Combine data -------

delay_careseek = careseek %>% 
  select(pid, caretype, caretype_fac) %>%
  left_join(drug2, by = "pid") %>%
  left_join(delay_careseek1, by = "pid") %>%
  arrange(child_id)




# Prepare final dataset for each table/figure -------

tab1_data = delay_careseek |>
  select(pid, enroll_site_fac, age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, final_quintile_fac, aav_fac,
         caretype_fac, preenroll_antibiotics_fac, time_to_facility, dehydration_fac, dysentery_fac, gems_msd_fac, 
         mvs_fac, hosp_fac, enr_wasting_fac, enr_stunting_fac, enr_underweight_fac)


fig1_data = delay_careseek |>
  select(pid, delayed_careseek_fac, delayed_careseek, child_id, enroll_site_fac, age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, final_quintile_fac, 
         aav_fac, caretype_fac, preenroll_antibiotics_fac, time_to_facility, dehydration_fac, dysentery_fac, gems_msd_fac, 
         mvs_fac, hosp_fac, enr_wasting_fac, enr_stunting_fac, enr_underweight_fac)


tab2_data = delay_careseek |>
  select(pid, delayed_careseek_sensitivity_fac, delayed_careseek_sensitivity, child_id, enroll_site_fac, age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, 
         final_quintile_fac, aav_fac, caretype_fac, preenroll_antibiotics_fac, time_to_facility, dehydration_fac, dysentery_fac, gems_msd_fac, mvs_fac, hosp_fac,
         enr_wasting_fac, enr_stunting_fac, enr_underweight_fac)


tab3_data = delay_careseek |>
  select(pid, delayed_careseek_fac, delayed_careseek, child_id, enroll_site_fac, age_fac, sex, mat_edu_fac, cg_age_fac, cg_employ_fac, 
         final_quintile_fac, aav_fac, caretype_fac, preenroll_antibiotics_fac, time_to_facility, dehydration_fac, dysentery_fac, gems_msd_fac, mvs_fac, hosp_fac,
         enr_wasting_fac, enr_stunting_fac, enr_underweight_fac, final_quintile_simple_fac, mat_edu_fac_two, prior_careseek_fac)



# Save datasets as RDS -------

saveRDS(tab1_data, "Last Step Datasets/tab1_data.Rds")
saveRDS(fig1_data, "Last Step Datasets/fig1_data.Rds")
saveRDS(tab2_data, "Last Step Datasets/tab2_data.Rds")
saveRDS(tab3_data, "Last Step Datasets/tab3_data.Rds")



