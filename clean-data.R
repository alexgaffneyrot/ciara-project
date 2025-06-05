library(data.table)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(moments)

#"C:\Users\AGaffney\Documents\ciara-project\data\Copy of Complete CHorio with NDI ct.xlsx"

data <- read_excel("/Users/AGaffney/Documents/ciara-project/data/Copy of Complete CHorio with NDI ct.xlsx",)
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
dt <- dt[!is.na(study_num)]

# Number of total participants
n <- dt[, uniqueN(study_num)]
print(n)

# Change character variables to numeric
dt[, firs_grade := as.numeric(firs_grade)]
dt[, firs_stage := as.numeric(firs_stage)]
dt[, motor := as.numeric(motor)]
dt[, language := as.numeric(language)]
dt[, cognitive := as.numeric(cognitive)]

# Add FIRS exposure flag - those with FIRS grade OR stage >=1
dt[, firs_exposed := fcase(
  (!is.na(firs_grade) & firs_grade >= 1) | 
    (!is.na(firs_stage) & firs_stage >= 1), 1,
  default = 0
)]

# Composite NDI/death outcome: 1 if motor, language, cognitive score <85 or CP/GDD/Death is recorded
dt[, compNDIdeath := fcase(
  death == 1, 1,
  motor < 85, 1,
  language < 85, 1,
  cognitive < 85, 1,
  default = 0
)]

# Composite severe NDI/death outcome: 1 if thresholds <70 and CP included
dt[, compsevNDIdeath := fcase(
  death == 1, 1,
  cp == 1, 1,
  motor < 70, 1,
  language < 70, 1,
  cognitive < 70, 1,
  default = 0
)]

# Clnical chorio: 1 if reason for preterm = 3 (Triple I (clinical chorio))
dt[, clinical_chorio := fcase(
  reason_for_preterm_birth == 3, 1,
  default = 0
)]

# table(dt$compNDIdeath)
# table(dt$compsevNDIdeath)
table(dt$firs_exposed)
# table(dt$clinical_chorio)
# table(dt$firs_grade)
# table(dt$firs_stage)

### MATERNAL CHARACTERISTICS
#####
## AGE
# Compute median and IQR components per group - FIR expose or not
range(dt$mat_age)

age_summary <- dt[, .(
  Median = median(mat_age, na.rm = TRUE),
  Q1 = quantile(mat_age, 0.25, na.rm = TRUE),
  Q3 = quantile(mat_age, 0.75, na.rm = TRUE)
),firs_exposed]

# Format as scientific-style
age_summary[, `Median [IQR]` := sprintf("%.2f (%.2f,%.2f)", Median, Q1, Q3)]
age_summary

# check if mat_age is normally distributed
# Should look bell-shaped
ggplot(dt, aes(x = mat_age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of Maternal Age")

# Q-Q plot: Points should fall on the line
qqnorm(dt$mat_age)
qqline(dt$mat_age, col = "red")

#Shapiro-Wilk test (for sample size < 5000):
# p < 0.05 = not normal
shapiro.test(dt$mat_age)
shapiro.test(na.omit(dt$mat_age))

# Skewness and Kurtosis
#Skewness ≠ 0 and Kurtosis ≠ 3 suggest deviation from normality.
skewness(na.omit(dt$mat_age))
kurtosis(na.omit(dt$mat_age))

#variable mat_age is approximately normally distributed
# Null hypothesis: There is no difference in the mean maternal age between the FIRS exposed and non-exposed groups
# Non-normal
p_age <- wilcox.test(mat_age ~ firs_exposed, data = dt)$p.value
# Normal
t.test(mat_age ~ firs_exposed, data = dt)
# Fail to reject the null hypothesis

# CLINICAL CHORIO
# Summary counts and proportions
class(dt$clinical_chorio)
table(factor(dt$clinical_chorio))
clinical_chorio_table <- dt[, .(
  Count = .N,
  Chorio_Positive = sum(clinical_chorio == 1, na.rm = TRUE),
  Percent_Positive = mean(clinical_chorio == 1, na.rm = TRUE) * 100
), by = firs_exposed]
clinical_chorio_table

dt[, clinical_chorio_f := factor(clinical_chorio, levels = c(0,1), labels = c("No", "Yes"))]
dt[, firs_exposed_f := factor(firs_exposed, levels = c(0,1), labels = c("Not Exposed", "Exposed"))]

# Create contingency table
clinical_chorio_tbl <- table(dt$firs_exposed_f, dt$clinical_chorio_f)
print(clinical_chorio_table)

# Counts <5 so use Chi-squared
# Chi-square test
# Null hypothesis: that there is no association between FIRS exposure and clinical chorioamnionitis
chisq.test(clinical_chorio_tbl)

# reject the null hypothesis 
# there is a statistically significant association between FIRS exposure and clinical chorioamnionitis in this data

##ROM
class(dt$rom)
table(factor(dt$rom))
# should only be 0,1 - change 2 to NA
dt[rom == 2, rom := NA]

# Summary counts and proportions
rom_table <- dt[, .(
  Count = .N,
  ROM_Positive = sum(rom == 1, na.rm = TRUE),
  Percent_Positive = mean(rom == 1, na.rm = TRUE) * 100
), by = firs_exposed]
rom_table

# Change to factor and add labels
dt[, rom_f := factor(rom, levels = c(0,1), labels = c("No", "Yes"))]

# Create contingency table
rom_tbl <- table(dt$firs_exposed, dt$rom)
print(rom_tbl)

# Counts <5 so use Chi-squared
# Chi-square test
# Null hypothesis: that there is no association between FIRS exposure and ROM
chisq.test(rom_tbl)
# Reject the null hypothesis: there is strong evidence of an association between FIRS exposure and ROM in this dataset

## STEROIDS
class(dt$steroids)
table(dt$steroids)
# should only be 0,1,2 - NA rest
dt[steroids %in% c("3", "NR"), steroids := NA_character_]

# group 0+1 
# Assign new column without warning
dt[, steroids_f := fifelse(steroids %in% c("0", "1"), "No/Partial",
                           fifelse(steroids == "2", "Full", NA_character_))]

#dt[, steroids_f := factor(steroids_f, levels = c("No/Partial", "Full"))]

# Check counts by firs_exposed and steroids_f
steroids_table <- dt[, .(
  Count = .N,
  Steroids_Full = sum(steroids_f == "Full", na.rm = TRUE),
  Percent_Full = mean(steroids_f == "Full", na.rm = TRUE) * 100
), by = firs_exposed]

steroids_table

# Create contingency table
steroids_tbl <- table(dt$firs_exposed_f, dt$steroids_f)
steroids_tbl

#Null hypothesis: no association between FIRS exposure and steroid dosing group
chisq.test(steroids_tbl)
p_steroids <- chisq.test(table(dt$firs_exposed_f, dt$steroids_f))$p.value

# Reject null hypothesis at the 5% significance level - there is a  a statistically significant association between FIRS exposure and steroid dosing group (0+1 vs 2).


#####
## INFANT CHACTERISTICS
# Recode Gender
dt[, Gender_label := factor(`Gender 1=male 0=female`, levels = c(1, 0), labels = c("Male", "Female"))]




##################
## FORMAT TABLE ##
##################

library(gtsummary)
library(dplyr)
library(gt)

# Group 0 and 1 as "0+1", keep 2, and convert to factor on the fly
dt <- dt %>%
  mutate(
    firs_exposed = factor(firs_exposed, levels = c(0, 1), labels = c("No FIR", "FIR")),
    steroids = case_when(
      steroids %in% c(0, 1) ~ "0+1",
      steroids == 2 ~ "2",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("0+1", "2"))
  )

# Build summary table
summary_tbl <- dt %>%
  select(firs_exposed_f, mat_age, clinical_chorio_f, rom_f, steroids_f) %>%
  tbl_summary(
    by = firs_exposed_f,
    type = list(
      all_continuous() ~ "continuous2",
      all_categorical() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      mat_age ~ "Maternal age, yr (Median, IQR)",
      clinical_chorio_f ~ "Clinical Chorioamnionitis",
      rom_f ~ "ROM",
      steroids_f ~ "Antenatal steroids"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(
    all_continuous() ~ "wilcox.test",
    all_categorical() ~ "chisq.test"
  )) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Relationship between Fetal Inflammatory Response (FIR) and Maternal and Infant characteristics**") %>%
  modify_table_body(
    ~ .x %>%
      mutate(group = case_when(
        variable %in% c("mat_age_f", "clinical_chorio_f", "rom_f", "steroids_f") ~ "**Maternal Characteristics**",
        TRUE ~ "**Infant Characteristics**"
      ))
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_row_group(
    label = "**Maternal Characteristics**",
    rows = variable %in% c("Maternal age, yr (Median, IQR)", "Clinical Chorioamnionitis", "ROM", "Antenatal steroids")
  ) %>%
  gt::tab_row_group(
    label = "**Infant Characteristics**",
    rows = variable %in% c()
  )
summary_tbl
