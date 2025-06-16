library(data.table)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(moments)
library(gtsummary)
library(gt)
library(naniar)
library(naniar)
library(car)
library(ggplot2)
library(VGAM)
library(MCMCpack)

data <- read_excel("/Users/AGaffney/Documents/ciara-project/data/Copy of Complete CHorio with NDI ct.xlsx")
dt <- as.data.table(data)

# rename variables
#####
custom_rename <- c(
  "Study no" = "study_num",
  "Gender 1=male 0=female" = "gender",
  "Gestational age" = "gest_age",
  "Birth weight" = "birth_weight",
  "Maternal Age" = "mat_age",
  "Conception? 1=Spontaneous, 2=IVF/ICSI 3=Donor Egg IVF 4=IUI" = "conception",
  "HP Dep Score" = "hp_dep_score",
  "Reason for preterm birth 1=PET, 2= PTL, 3=Triple I (clinical chorio), 4= IUGR, 5= Maternal reasons 6=APH 7=TTTS, 8=Cord Prolapse, 9=AEDF, 10=NRCTG, 11=Others" = "reason_for_preterm_birth",
  "No. of babies 1=1, 2=2 3=3, 4=4" = "num_babies",
  "MgSO4 1=Yes, 0= None" = "mgso4",
  "Steroids 2=full, 1= 1 dose, 0=none" = "steroids",
  "ROM 1=yes 0=no" = "rom",
  "Duration of ROM" = "dur_rom",
  "Apgars 1" = "apgars_1",                                                                                                                                                
  "Apgars 5" = "apgars_5",                                                                                                                                        
  "Apgars 10" = "apgars_10",
  "CRIB II" = "crib_ii",
  "Feeding? 1=EMBM, 2=DEBM, 3= Formula" = "feeding",
  "Time to full feeds (120ml/kg/day)" = "time_to_full_feeds",
  "Days of therapy antibiotic exposure (DOT)" = "dot",
  "Microbiological chorio" = "micro_chorio",
  "Histiological chorio?" = "histo_chorio",
  "FIRS grade" = "firs_grade",                                                                                                                                                 
  "FIRS stage" = "firs_stage",                                                                                                                                             
  "Organism" = 'organism',
  "Dx of sepsis during NICU" = "sepsis_during_nicu",
  "Total episodes of clinically suspected sepsis?" = "num_episodes_sepsis",                                                                                                           
  "Culture +ve?" = "culture_positve",                                                                                                                                   
  "Organism on culture" = "organism_on_culture",                                                                                                                                  
  "NEC dx during NICU stay" = "nec_dx_nicu",                                                                                                                           
  "Total episodes of suspected NEC" = "total_ep_sus_nec",                                                                                                                      
  "Bells stage (per episode suspected NEC)" = "bells_stage_per_ep_sus_nex",                                                                               
  "Days on mechanical ventilation" = "days_mech_ventilation",                                                                                                         
  "Days on NIV (CPAP/Bipap)" = "days_niv_cpap_bipap",                                                                                        
  "Bronchopulmonnary Dysplasia/Chronic Lung disease (O2 requirement at 36 weeks or 28 days consecutively )" = "bronchopulmonnary_dysplasia_chronic_lung_disease",                                                 
  "Evidence of IVH" = "evidence_ivh",                                                                                                                                            
  "Grade of IVH 1-4=GRADES 5=PVL" = "grade_ivh",                                                                                                               
  "Location of IVH" = "loc_ivh",                                                                                                                                         
  "Highest WCC in first 72 hrs" = "highest_wcc_72hrs",                                                                                                          
  "WCC on Day 1" = "wcc_d1",                                                                                                                          
  "WCC on Day 3" = "wcc_d3",                                                                                                                                         
  "Lowest platelets in first 72 hrs" = "lowest_platelets_72hrs",                                                                                                       
  "Platelets Day 1" = "platelets_d1",                                                                                                                           
  "Platelets Day 3" = "platelets_d3",                                                                                                                                           
  "Highest neutrophil count first 72 hours" = "highest_neutrophil_72hrs",                                                                                                            
  "Neuts D1" = "neuts_d1",                                                                                                                                     
  "Neuts D3" = "neuts_d3",                                                                                                                                           
  "Highest Lymphs in first 72hrs" = "highest_lymps_72hrs",                                                                                                                              
  "Lymphs on Day 1" = "lymphs_d1",                                                                                                                                      
  "Lymphs on Day3" = "lymphs_d3",                                                                                                                                         
  "Pneumothoraces?" = "pneumothoraces",                                                                                                                                   
  "RDS requiring surfactant" = "rds_requiring_surfactant",                                                                                                                           
  "Doses of surfactant" = "doses_surfactant",                                                                                                                             
  "PDA treatment?" = "pda_treatment",                                                                                                                         
  "Which PDA treatment (1=medical only, 2=surgical only, 3=medical, then surgical, N/A=no treatment)" = "pda_treatment_type",                                                    
  "Number of medical treatments PDA" = "num_med_treatments_pda",                                                                                                                      
  "Device closure?" = "device_closure",                                                                                                                                
  "Length of stay" = "length_stay",                                                                                                                                         
  "Death? 1=yes, 0=No" = "death",                                                                                                                                       
  "Death from Respiratory Failure or Chronic Lung Disease? Yes=1 No=0" = "death_resp_failure_chronic_lung",                                                                                      
  "Comments...61" = "comments",                                                                                                                                        
  "Bayley scores 1=Y, 0=n" = "bayley_score",                                                                                                           
  "If no, reason" = "reason_no_bayleys",                                                                                                                                      
  "If no, referral to other services? 1=Y, 0=N" = "no_bayleys_ref",                                                                                                    
  "Age at Bayley (months, chronilogical)" = "age_at_bayleys",                                                                                                          
  "Cognitive" = "cognitive",                                                                                                                                  
  "Motor" = "motor",                                                                                                                                                  
  "Language" = "language",                                                                                                                                       
  "Social-emotional" = "social_emotional",                                                                                                                                        
  "...70" = "remove",                                                                                                                                             
  "...71" = "remove2",                                                                                                                                          
  "...72" = "remove3",                                                                                                                                              
  "PVL 1=y, 0=n" = "pvl",                                                                                                                                           
  "PVHD" = "pvhd",                                                                                                                                          
  "MRI performed 1=yes, 0=no" = "mri",                                                                                                                          
  "Evidence of white matter injury on MRI 1=yes, 0=no" = "evidence_white_matte_injury",                                                                                           
  "Age at MRI" = "age_mri",                                                                                                                                    
  "MRI comments" = "mri_comment",                                                                                                                                    
  "OFC at birth CENTILE" = "ofc_birth_centile",                                                                                                                                   
  "OFC at MRI if done" = "ofc_mri",                                                                                                                          
  "OFC at discharge CENTILE" = "ofc_discharge_centile",                                                                                                                                 
  "follow up timeline" = "follow_up_timeline",                                                                                                                                        
  "OFC at follow up" = "ofc_follow_up",                                                                                                                               
  "AIMS centile" = "aims_centile",                                                                                                                                          
  "HINE score 3/12" = "hine_3_12",                                                                                                                                   
  "HINE score 6/12" = "hine_6_12",                                                                                                                                     
  "GMA writhing age 1=normal, 2=PR, 3=CS, 4=chaotic" = "gma_writhing",                                                                                                 
  "GMA Fidgety age 1=normal, 2=absent, 3=exaggerated" = "gma_fidgety" ,                                                                                                    
  "Diagnosis 1=yes, 0=no" = "diagnosis",                                                                                                                                
  "CP 1=yes, 0=no" = "cp",                                                                                                                                 
  "NDI (CP or GDD without Bayley score) 1=yes, 0=no" = "ndi",                                                                                                    
  "ASD 1=yes" = "asd",                                                                                                                                        
  "Comments...93" = "comment2",                                                                                                                                 
  "DNA 1=yes, 0=no" = "dna",                                                                                                                                        
  "Length of follow up (months)" = "length_fu",                                                                                                                 
  "Referral to CDNT 1=yes, 0=no" = "ref_cdnt"
)
setnames(dt, old = names(custom_rename), new = custom_rename)
#####
# remove any NA study #'s and check for duplicate rows
dt <- dt[!is.na(study_num)]
any(duplicated(dt$study_num))

# remove empty cols
dt[, c("remove", "remove2", "remove3") := NULL]


# Number of total participants
n <- dt[, uniqueN(study_num)]
n

# Change character variables to numeric
dt[, firs_grade := as.numeric(firs_grade)]
dt[, firs_stage := as.numeric(firs_stage)]
dt[, motor := as.numeric(motor)]
dt[, language := as.numeric(language)]
dt[, cognitive := as.numeric(cognitive)]
dt[, social_emotional := as.numeric(social_emotional)]
dt[, apgars_1 := as.numeric(apgars_1)]
dt[, apgars_5 := as.numeric(apgars_5)]
dt[, apgars_10 := as.numeric(apgars_10)]
dt[, length_fu := as.numeric(length_fu)]
dt[, bayley_score := as.numeric(bayley_score)]
dt[, asd := as.numeric(asd)]


# birth weight wrong in three - update
# Update values based on study_number
dt[study_num == 190, birth_weight := 1230]
dt[study_num == 191, birth_weight := 1350]
dt[study_num == 188, birth_weight := 1440]


# Add FIRS exposure flag - those with FIRS grade OR stage >=1
dt[, firs_exposed := fcase(
  (!is.na(firs_grade) & firs_grade >= 1) | 
    (!is.na(firs_stage) & firs_stage >= 1), "FIRS",
  default = "No FIRS"
)]

# Add birth weight category 
dt$birth_weight_cat <- cut(
  dt$birth_weight,
  #breaks = c(-Inf, 1499, 2499, Inf),
  #labels = c("VLBW", "LBW", "NBW"),
  # get rid of NBW - there's only one and messing up regression
  breaks = c(-Inf, 1499, Inf),
  labels = c("VLBW", "LBW"),
  right = TRUE
)

dt$mat_age_cat <- cut(
  dt$mat_age,
  breaks = c(min(dt$mat_age), 34, max(dt$mat_age)),
  labels = c("NMA","AMA"),
  right = TRUE
)

# Rescale maternal age: effect per 5-year increase
dt$mat_age_rescale5 <- dt$mat_age / 5


# Create composite outcome variables
dt[, compNDIdeath := fcase(
  death == 1, "YES",
  ndi == 1, "YES",
  !is.na(motor) & motor < 85, "YES",
  !is.na(language) & language < 85, "YES",
  !is.na(cognitive) & cognitive < 85, "YES",
  default = "NO"
)]

dt[, compNDIdeath_Motor := fcase(
  death == 1, "YES",
  ndi == 1, "YES",
  !is.na(motor) & motor < 85, "YES",
  default = "NO"
)]

dt[, compNDIdeath_Language := fcase(
  death == 1, "YES",
  ndi == 1, "YES",
  !is.na(language) & language < 85, "YES",
  default = "NO"
)]

# Create composite outcome variables
dt[, compNDIdeath_Cognitive := fcase(
  death == 1, "YES",
  ndi == 1, "YES",
  !is.na(cognitive) & cognitive < 85, "YES",
  default = "NO"
)]

# Sev
dt[, compsevNDIdeath := fcase(
  death == 1, "YES",
  cp == 1, "YES",
  !is.na(motor) & motor < 70, "YES",
  !is.na(language) & language < 70, "YES",
  !is.na(cognitive) & cognitive < 70, "YES",
  default = "NO"
)]

dt[, compsevNDIdeath_Motor := fcase(
  death == 1, "YES",
  cp == 1, "YES",
  !is.na(motor) & motor < 70, "YES",
  default = "NO"
)]

dt[, compsevNDIdeath_Language := fcase(
  death == 1, "YES",
  cp == 1, "YES",
  !is.na(language) & language < 70, "YES",
  default = "NO"
)]

dt[, compsevNDIdeath_Cognitive := fcase(
  death == 1, "YES",
  cp == 1, "YES",
  !is.na(cognitive) & cognitive < 70, "YES",
  default = "NO"
)]


# Clnical chorio: 1 if reason for preterm = 3 (Triple I (clinical chorio))
dt[, clinical_chorio := fcase(
  reason_for_preterm_birth == 3, "Yes",
  default = 'No'
)]

# HP Dep Score groups

dt$SESgroups <- cut(
  dt$hp_dep_score,
  breaks = c(-40, -10, 10, 40),
  labels = c("Disadvantaged", "Average", "Affluent")
)

# Steroids
dt <- dt %>%
  mutate(
    steroids = case_when(
      steroids %in% c(0, 1) ~ "Partial",
      steroids == 2 ~ "Full",
      TRUE ~ NA_character_
    )
  )

#rescale birth weight, e.g., per 100 grams:
dt$birth_weight_100g <- dt$birth_weight / 100


# check for missing data
vis_miss(dt)

# change variables to factors
dt[, compNDIdeath := factor(compNDIdeath)]
dt[, compNDIdeath_Motor := factor(compNDIdeath_Motor)]
dt[, compNDIdeath_Cognitive := factor(compNDIdeath_Cognitive)]
dt[, compNDIdeath_Language := factor(compNDIdeath_Language)]

dt[, compsevNDIdeath := factor(compsevNDIdeath)]
dt[, compsevNDIdeath_Motor := factor(compsevNDIdeath_Motor)]
dt[, compsevNDIdeath_Cognitive := factor(compsevNDIdeath_Cognitive)]
dt[, compsevNDIdeath_Language := factor(compsevNDIdeath_Language)]

dt[, firs_exposed := factor(firs_exposed)]
dt[, birth_weight_cat := factor(birth_weight_cat)]
dt[, gender := factor(gender,
                      levels = c(0, 1),
                      labels = c("Male", "Female"))]
dt[, conception := factor(
  conception,
  levels = c(1, 2, 3, 4),
  labels = c("Spontaneous", "IVF/ICSI", "Donor Egg IVF", "IUI")
)]

dt[, rom := factor(rom, levels = c(0, 1), labels = c("No", "Yes"))]
dt[, mat_age_cat := factor(mat_age_cat)]
dt[, grade_ivh := factor(grade_ivh)]
dt[, pvl := factor(pvl)]
dt[, gender := factor(gender)]
dt[, steroids := factor(steroids)]
dt[, firs_grade := factor(firs_grade)]
dt[, firs_stage := factor(firs_stage)]
dt[, clinical_chorio := factor(clinical_chorio)]
dt[, SESgroups := factor(SESgroups)]
dt[, asd := factor(asd, levels = c(0,1), labels = c("No", "Yes"))]

# Bayleys
###################

dt_no_bayleys <- dt[bayley_score == 0,]

# dt <- dt %>%
#   mutate(bayley_score_flag = ifelse(is.na(motor), "No", "Yes"))
# 
# View(table(dt$bayley_score_flag, dt$no_bayleys_ref))


# Recategorize into broader, cleaner labels
dt_no_bayleys[, no_bayleys_cat := fcase(
  no_bayleys_ref %in% c("0"),"No",
  no_bayleys_ref %in% c("1"),"Yes",
  no_bayleys_ref %in% c("2"),"Yes",
  grepl("dna", no_bayleys_ref, ignore.case = TRUE), "DNA",
  grepl("lost", no_bayleys_ref, ignore.case = TRUE), "Lost to Follow-Up",
  grepl("discharge", no_bayleys_ref, ignore.case = TRUE), "No",
  grepl("referred", no_bayleys_ref, ignore.case = TRUE), "Yes",
  is.na(no_bayleys_ref) | no_bayleys_ref %in% c("NA", "na", ""), "NA",
  default = "Other"
)]

#dim(dt_no_bayleys)
#table(dt_no_bayleys$no_bayleys_cat)

