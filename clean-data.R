library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

#"C:\Users\AGaffney\Documents\ciara-project\data\Copy of Complete CHorio with NDI ct.xlsx"

data <- read_excel("/Users/AGaffney/Documents/ciara-project/data/Copy of Complete CHorio with NDI ct.xlsx",)
dt <- as.data.table(data)
dt <- dt[!is.na(`Study no`)]

table(dt$`Histiological chorio?`)

# number histo chorio pos 
histo_chorio_pos <- dt[`Histiological chorio?` == 1]
nrow(histo_chorio_pos)
n_histo_chorio_pos <- histo_chorio_pos[, uniqueN(`Study no`)]


# number of participants
n_participants <- dt[, uniqueN(`Study no`)]
print(n_participants)

dt[, sum(duplicated(`Study no`))]

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
  "Length of stay"                                                                                                                                               
  "Death? 1=yes, 0=No"                                                                                                                                           
  "Death from Respiratory Failure or Chronic Lung Disease? Yes=1 No=0"                                                                                           
  "Comments...61"                                                                                                                                                
  "Bayley scores 1=Y, 0=n"                                                                                                                                       
  "If no, reason"                                                                                                                                                
  "If no, referral to other services? 1=Y, 0=N" = "no_bayleys_ref",                                                                                                    
  "Age at Bayley (months, chronilogical)" = "age_at_bayleys",                                                                                                          
  "Cognitive"                                                                                                                                                    
  "Motor" = "motor",                                                                                                                                                  
  "Language" = "language",                                                                                                                                       
  "Social-emotional"                                                                                                                                             
  "...70" = "remove"                                                                                                                                                   
  "...71" =                                                                                                                                                
  "...72"                                                                                                                                                        
  "PVL 1=y, 0=n"                                                                                                                                                 
  "PVHD"                                                                                                                                                         
  "MRI performed 1=yes, 0=no"                                                                                                                                    
  "Evidence of white matter injury on MRI 1=yes, 0=no"                                                                                                           
  "Age at MRI"                                                                                                                                                   
  "MRI comments"                                                                                                                                                 
  "OFC at birth CENTILE"                                                                                                                                         
  "OFC at MRI if done"                                                                                                                                           
  "OFC at discharge CENTILE"                                                                                                                                     
  "follow up timeline"                                                                                                                                           
  "OFC at follow up"                                                                                                                                             
  "AIMS centile"                                                                                                                                                 
  "HINE score 3/12"                                                                                                                                              
  "HINE score 6/12"                                                                                                                                              
  "GMA writhing age 1=normal, 2=PR, 3=CS, 4=chaotic"                                                                                                             
  "GMA Fidgety age 1=normal, 2=absent, 3=exaggerated"                                                                                                            
  "Diagnosis 1=yes, 0=no"                                                                                                                                        
  "CP 1=yes, 0=no"                                                                                                                                               
  "NDI (CP or GDD without Bayley score) 1=yes, 0=no"                                                                                                             
  "ASD 1=yes"                                                                                                                                                    
  "Comments...93"                                                                                                                                                
  "DNA 1=yes, 0=no"                                                                                                                                              
  "Length of follow up (months)"                                                                                                                                 
  "Referral to CDNT 1=yes, 0=no" 
)

#####

setnames(dt, old = names(custom_rename), new = custom_rename)




### MATERNAL CHARACTERISTICS
# Compute median and IQR components per group
age_summary <- dt[, .(
  Median = median(`Maternal Age`, na.rm = TRUE),
  Q1 = quantile(`Maternal Age`, 0.25, na.rm = TRUE),
  Q3 = quantile(`Maternal Age`, 0.75, na.rm = TRUE)
),`Histiological chorio?`]

# Format as scientific-style
age_summary[, `Median [IQR]` := sprintf("%.2f [%.2f–%.2f]", Median, Q1, Q3)]


## INFANT CHACTERISTICS
# Recode Gender
dt[, Gender_label := factor(`Gender 1=male 0=female`, levels = c(1, 0), labels = c("Male", "Female"))]
