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
library("ResourceSelection")
library("pROC")
library("ggeffects")
library(mgcv)
library(broom)
library(janitor)
library(purrr)

data <- read.csv("/Users/AGaffney/Documents/ciara-project/data/CerebralPalsyEDI-CRUSSRotunda_DATA_2025-08-20_1128.CSV")
dt <- as.data.table(data)
#colnames(dt)
custom_rename <- c(
  "record_id" = "study_num",
  "dc_cruss" = "cruss_performed",
  "dc_cruss_result___1" = "normal_cruss",
  "dc_cruss_result___2" = "ivh_cruss",
  "dc_cruss_result___3" = "iph_cruss",
  "dc_cruss_result___4" = "sah_cruss",
  "dc_cruss_result___5" = "gen_edema_cruss",
  "dc_cruss_result___6" = "is_cruss",
  "dc_cruss_result___7" = "th_cruss",
  "dc_cruss_result___8" = "oth_cruss",
  "dc_cruss_result_other" = "oth_cruss_specify",
  "gma_result" = "gma_result",
  "gma_result_v1" = "gma_result_v1",
  "hine_score_v1" = "hine_score_v1",
  "hine_asym_score_v1" = "hine_asym_score_v1",
  "general_hr" = "hrcp",
  "general_cp" = "cp",
  "dc_diagnosis___21" = "anaemia_diag",
  "dc_diagnosis___1" = "bpd_diag",
  "dc_diagnosis___2" = "chd_diag",
  "dc_diagnosis___3" = "cld_diag",
  "dc_diagnosis___4" = "early_sepsis_diag",
  "dc_diagnosis___5" = "late_sepsis_diag",
  "dc_diagnosis___6" = "hie_mild_diag",    
  "dc_diagnosis___7" = "hie_mod_diag",     
  "dc_diagnosis___8" = "hie_sev_diag",     
  "dc_diagnosis___9" = "th_diag",     
  "dc_diagnosis___10" = "ivh_1_2_diag",  
  "dc_diagnosis___11" = "ivh_3_4_diag",  
  "dc_diagnosis___12" = "nec_entero_diag",
  "dc_diagnosis___13" = "pda_diag",    
  "dc_diagnosis___20" = "prem_diag",    
  "dc_diagnosis___14" = "pvl_diag",    
  "dc_diagnosis___15" = "rop_diag",    
  "dc_diagnosis___16" = "seiz_diag",    
  "dc_diagnosis___17" = "stroke_diag",   
  "dc_diagnosis___18" = "neo_enceph_diag",   
  "dc_diagnosis___19" = "other_diag",    
  "dc_mri_req" = "mri_requested",
  "reg_ga_wks" = "ga_wks",
  "reg_ga_days"= "ga_days",
  "reg_bw_grams" = "bw_g",
  "reg_sex" = "sex",
  "attend_v1" = "attend_v1"
)

setnames(dt, old = names(custom_rename), new = custom_rename)

# demographics table
summary_tbl <- dt[, .(study_num, ga_wks, ga_days, bw_g, sex, attend_v1)] %>%
  mutate(
    ga_total_days = ga_wks * 7 + ga_days,    # total gestational age in days
    ga_weeks_exact = ga_total_days / 7,      # GA in exact weeks
    sex = factor(sex, labels = c("Male", "Female")),
    attend_v1 = factor(attend_v1, labels = c("No", "Yes")),
    pat_group = case_when(
      ga_wks >= 40 ~ "Term",
      TRUE ~ "Preterm"
    )
  )

summary_tbl %>%
  tbl_summary(
    include = c(ga_weeks_exact, bw_g, attend_v1, sex, pat_group),
    label = list(
      ga_weeks_exact ~ "Gestational Age (weeks)",
      bw_g ~ "Birthweight (g)",
      sex ~ "Sex",
      pat_group ~ "Patient Group",
      attend_v1 ~ "Attended Visit 1"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,   # show 1 decimal place
    missing = "no"
    ) %>%
  add_n()


# Recoding variables
dt$gma_result <- as.character(dt$gma_result)

levels(as.factor(dt$gma_result))
dt <- dt %>%
  mutate(gma_result = case_when(
    gma_result == "2" ~ "Normal writhing",
    gma_result == "3" ~ "Poor repertoire",
    gma_result == "5" ~ "Cramped Synchronized"
  ))

levels(as.factor(dt$gma_result_v1))
dt$gma_result_v1 <- as.character(dt$gma_result_v1)

dt <- dt %>%
  mutate(gma_result_v1 = case_when(
    gma_result_v1 == "1" ~ "Normal fidgety",
    gma_result_v1 == "6" ~ "Abnormal Fidgety movements",
    gma_result_v1 == "7" ~ "Absent Fidgety movements"
  ))

dt <- dt %>% 
  mutate(pat_group = case_when(
    ga_wks >= 40 ~ "Term",
    TRUE ~ "Preterm"
  ))


dt[, hine_cat := fifelse(hine_score_v1 >= 67, "Normal",
                         fifelse(hine_score_v1 >= 57 & hine_score_v1 <= 66, "Borderline",
                                 fifelse(hine_score_v1 <= 56, "HRCP", NA_character_)))]

##################
## NORMAL CRUSS ##
##################

normal_cruss <- dt[normal_cruss == 1 & pat_group == "Preterm",]
nrow(normal_cruss)

levels(factor(normal_cruss$gma_result))
levels(factor(normal_cruss$gma_result_v1))
normal_cruss_abnormal_gma_mri <- normal_cruss[gma_result %in% c("Poor repertoire", "Cramped Synchronized") |
                                                gma_result_v1 %in% c("Abnormal Fidgety movements", "Absent Fidgety movements") |
                                                mri_requested == 1, ]
#nrow(normal_cruss_abnormal_gma_mri)

# variables to summarize
vars <- c("gma_result", "gma_result_v1", "hrcp", "cp", "mri_requested", "hine_cat")

# # function to get counts + percentages
# summary_table <- function(dt, var) {
#   dt[, .N, by = var][
#     , percent := round(100 * N / sum(N), 1)
#   ][order(-N)]
# }
# 
# # generate summary tables for all variables
# normal_cruss_abnormal_gma_mri_summaries <- lapply(vars, function(v) summary_table(normal_cruss_abnormal_gma_mri, v))
# names(normal_cruss_abnormal_gma_mri_summaries) <- vars
# 
# #view summaries
# normal_cruss_abnormal_gma_mri_summaries$gma_result
# normal_cruss_abnormal_gma_mri_summaries$gma_result_v1
# normal_cruss_abnormal_gma_mri_summaries$hrcp
# normal_cruss_abnormal_gma_mri_summaries$cp
# normal_cruss_abnormal_gma_mri_summaries$mri_requested
# normal_cruss_abnormal_gma_mri_summaries$hine_cat

# word doc
# Initialize empty table to store everything
normal_cruss_abnormal_gma_mri_all_summary <- data.table(
  Variable = character(),
  Category = character(),
  N = integer(),
  Percent = numeric()
)

# Initialize empty table
normal_cruss_abnormal_gma_mri_all_summary <- data.table(
  Variable = character(),
  Category = character(),
  N = integer(),
  Percent = numeric()
)

for (v in vars) {
  # Calculate counts by category
  summary <- normal_cruss_abnormal_gma_mri[, .N, by = .(Category = get(v))][order(-N)]
  summary[, Percent := round(100 * N / sum(N), 1)]
  summary[, Variable := v]
  
  # Append category counts only
  normal_cruss_abnormal_gma_mri_all_summary <- rbind(
    normal_cruss_abnormal_gma_mri_all_summary, summary, fill = TRUE
  )
}

# Add a single total row per variable at the very end (or top)
totals <- normal_cruss_abnormal_gma_mri_all_summary[, .(
  Category = "Total",
  N = sum(N),
  Percent = 100
), by = Variable]

# Combine totals and detailed table
normal_cruss_abnormal_gma_mri_final_summary <- rbind(totals, normal_cruss_abnormal_gma_mri_all_summary, fill = TRUE)

# Print with tabs
write.table(
  normal_cruss_abnormal_gma_mri_final_summary, 
  sep = "\t", 
  row.names = FALSE, 
  quote = FALSE
)

# create plot data
normal_cruss_abnormal_gma_mri_all_summary_plot_data <- normal_cruss_abnormal_gma_mri_final_summary %>% 
  filter(Category != "Total") %>%  # remove totals for plotting
  mutate(
    Variable = case_when(
      Variable == "gma_result"     ~ "GMA (Writhing Phase)",
      Variable == "gma_result_v1"  ~ "GMA (Fidgety Phase)",
      Variable == "hrcp"           ~ "High Risk of CP",
      Variable == "cp"             ~ "CP Diagnosis",
      Variable == "mri_requested"  ~ "MRI Requested",
      Variable == "hine_cat"       ~ "HINE Category",
      TRUE                         ~ Variable
    ),
    Category = case_when(
      Category == "0"      ~ "No",
      Category == "1"      ~ "Yes",
      Category == "NA"     ~ "Missing",
      Category == "Normal writhing" ~ "Normal Writhing",
      Category == "Poor repertoire" ~ "Poor Repertoire",
      Category == "Normal fidgety"  ~ "Normal Fidgety",
      Category == "Absent Fidgety movements" ~ "Absent Fidgety",
      TRUE               ~ Category
    ),
    # order categories within each variable
    Category = factor(Category,
                      levels = c("Yes", "No", "Missing", 
                                 "Normal Writhing", "Poor Repertoire",
                                 "Normal Fidgety", "Absent Fidgety",
                                 "Normal", "Borderline", "HRCP"))
  ) %>%
  # order the facet variable order
  mutate(Variable = factor(Variable, levels = c(
    "GMA (Writhing Phase)",
    "GMA (Fidgety Phase)",
    "HINE Category",
    "High Risk of CP",
    "CP Diagnosis",
    "MRI Requested"
  )))

category_colors <- c(
  "Normal Writhing"    = "#1b9e77",  # green
  "Poor Repertoire"    = "#d95f02",  # orange
  "Normal Fidgety"     = "#1b9e77",  # green
  "Absent Fidgety"     = "#d95f02",  # orange
  "No"                 = "#7570b3",  # muted purple
  "Yes"                = "#e7298a",  # pink/red
  "Missing"            = "#666666",  # grey
  "Normal"             = "#1b9e77",  # green
  "Borderline"         = "#d95f02",  # orange
  "HRCP"               = "#e7298a"   # red/pink
)

ggplot(normal_cruss_abnormal_gma_mri_all_summary_plot_data, 
       aes(x = Category, y = Percent, fill = Category)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = paste0(Percent, "%")), 
            vjust = -0.3, size = 3.2, color = "black") +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = category_colors, na.value = "grey80") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11)
  ) +
  labs(
    title = "Normal CRUSS: Abnormal GMA Result or MRI Requested",
    y = "Percent (%)"
  ) +
  expand_limits(y = max(normal_cruss_abnormal_gma_mri_all_summary_plot_data$Percent) + 10)


##############
## IVH 1 +2 ##
##############

ivh_1_2 <- dt[ivh_1_2_diag == 1 & pat_group == "Preterm",]
#nrow(ivh_1_2)

ivh_1_2_abnormal_gma_mri <- ivh_1_2[gma_result %in% c("Poor repertoire", "Cramped Synchronized") |
                                  gma_result_v1 %in% c("Abnormal Fidgety movements", "Absent Fidgety movements") |
                                  mri_requested == 1, ]

#nrow(ivh_1_2_abnormal_gma_mri)

ivh_1_2_abnormal_gma_mri_all_summary <- data.table(
  Variable = character(),
  Category = character(),
  N = integer(),
  Percent = numeric()
)

for (v in vars) {
  # Calculate counts by category
  summary <- ivh_1_2_abnormal_gma_mri[, .N, by = .(Category = get(v))][order(-N)]
  summary[, Percent := round(100 * N / sum(N), 1)]
  summary[, Variable := v]
  
  # Append category counts only
  ivh_1_2_abnormal_gma_mri_all_summary <- rbind(
    ivh_1_2_abnormal_gma_mri_all_summary, summary, fill = TRUE
  )
}

# Add a single total row per variable at the very end (or top)
ivh_1_2_abnormal_gma_mri_total <- data.table(
  Variable = "Total",
  Category = "",
  N = sum(ivh_1_2_abnormal_gma_mri_all_summary$N),
  Percent = 100
)

# Combine totals and detailed table
ivh_1_2_abnormal_gma_mri_final_summary <- rbind(
  ivh_1_2_abnormal_gma_mri_total, 
  ivh_1_2_abnormal_gma_mri_all_summary, 
  fill = TRUE
)

# Ensure column order
setcolorder(ivh_1_2_abnormal_gma_mri_final_summary, c("Variable", "Category", "N", "Percent"))

# Print with tabs
write.table(
  ivh_1_2_abnormal_gma_mri_final_summary, 
  sep = "\t", 
  row.names = FALSE, 
  quote = FALSE
)

# create plot data
ivh_1_2_abnormal_gma_mri_all_summary_plot_data <- ivh_1_2_abnormal_gma_mri_final_summary %>% 
  filter(Category != "Total") %>%  # remove totals for plotting
  mutate(
    Variable = case_when(
      Variable == "gma_result"     ~ "GMA (Writhing Phase)",
      Variable == "gma_result_v1"  ~ "GMA (Fidgety Phase)",
      Variable == "hrcp"           ~ "High Risk of CP",
      Variable == "cp"             ~ "CP Diagnosis",
      Variable == "mri_requested"  ~ "MRI Requested",
      Variable == "hine_cat"       ~ "HINE Category",
      TRUE                         ~ Variable   # keep others unchanged
    ),
    Category = case_when(
      Category == "0"      ~ "No",
      Category == "1"      ~ "Yes",
      Category == "NA"     ~ "Missing",
      Category == "Normal writhing" ~ "Normal Writhing",
      Category == "Poor repertoire" ~ "Poor Repertoire",
      Category == "Normal fidgety"  ~ "Normal Fidgety",
      Category == "Absent Fidgety movements" ~ "Absent Fidgety",
      TRUE               ~ Category
    ),
    # order categories within each variable
    Category = factor(Category,
                      levels = c("Yes", "No", "Missing", 
                                 "Normal Writhing", "Poor Repertoire",
                                 "Normal Fidgety", "Absent Fidgety",
                                 "Normal", "Borderline", "HRCP"))
  ) %>%
  # order the facet variable order
  mutate(Variable = factor(Variable, levels = c(
    "GMA (Writhing Phase)",
    "GMA (Fidgety Phase)",
    "HINE Category",
    "High Risk of CP",
    "CP Diagnosis",
    "MRI Requested"
  )))

ggplot(ivh_1_2_abnormal_gma_mri_all_summary_plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = paste0(Percent, "%")), 
            vjust = -0.3, size = 3.2, color = "black") +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = category_colors, na.value = "grey80") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11)
  ) +
  labs(
    title = "IVH 1-2: Abnormal GMA Result or MRI Requested",
    y = "Percent (%)"
  ) +
  expand_limits(y = max(ivh_1_2_abnormal_gma_mri_all_summary_plot_data$Percent) + 10)




###############################
## NORMAL CRUSS - NORMAL GMA ##
###############################

normal_cruss_normal_gma <- normal_cruss[gma_result %in% c("Normal writhing") |
                                          gma_result_v1 %in% c("Normal fidgety"), ]

#nrow(normal_cruss_normal_gma)

normal_cruss_normal_gma_all_summary <- data.table(
  Variable = character(),
  Category = character(),
  N = integer(),
  Percent = numeric()
)

for (v in vars) {
  summary <- normal_cruss_normal_gma[, .N, by = .(Category = get(v))][order(-N)]
  summary[, Percent := round(100 * N / sum(N), 1)]
  summary[, Variable := v]
  
  # Add total row
  total_row <- data.table(
    Variable = v,
    Category = "Total",
    N = sum(summary$N),
    Percent = 100
  )
  summary <- rbind(summary, total_row, fill = TRUE)
  
  normal_cruss_normal_gma_all_summary <- rbind(normal_cruss_normal_gma_all_summary, summary, fill = TRUE)
}

# Print all together with tabs
write.table(
  normal_cruss_normal_gma_all_summary,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)


# create plot data
normal_cruss_normal_gma_plot_data <- normal_cruss_normal_gma_all_summary %>% 
  filter(Category != "Total") %>%  # remove totals for plotting
  mutate(
    Variable = case_when(
      Variable == "gma_result"     ~ "GMA (Writhing Phase)",
      Variable == "gma_result_v1"  ~ "GMA (Fidgety Phase)",
      Variable == "hrcp"           ~ "High Risk of CP",
      Variable == "cp"             ~ "CP Diagnosis",
      Variable == "mri_requested"  ~ "MRI Requested",
      Variable == "hine_cat"       ~ "HINE Category",
      TRUE                         ~ Variable   # keep others unchanged
    ),
    Category = case_when(
      Category == "0"      ~ "No",
      Category == "1"      ~ "Yes",
      Category == "NA"     ~ "Missing",
      Category == "Normal writhing" ~ "Normal Writhing",
      Category == "Poor repertoire" ~ "Poor Repertoire",
      Category == "Normal fidgety"  ~ "Normal Fidgety",
      Category == "Absent Fidgety movements" ~ "Absent Fidgety",
      TRUE               ~ Category
    ),
    # order categories within each variable
    Category = factor(Category,
                      levels = c("Yes", "No", "Missing", 
                                 "Normal Writhing", "Poor Repertoire",
                                 "Normal Fidgety", "Absent Fidgety",
                                 "Normal", "Borderline", "HRCP"))
  ) %>%
  # order the facet variable order
  mutate(Variable = factor(Variable, levels = c(
    "GMA (Writhing Phase)",
    "GMA (Fidgety Phase)",
    "HINE Category",
    "High Risk of CP",
    "CP Diagnosis",
    "MRI Requested"
  )))

ggplot(normal_cruss_normal_gma_plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = paste0(Percent, "%")), 
            vjust = -0.3, size = 3.2, color = "black") +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = category_colors, na.value = "grey80") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11)
  ) +
  labs(
    title = "Normal CRUSS: Normal GM (at either writhing or fidgety age)",
    y = "Percent (%)"
  ) +
  expand_limits(y = max(normal_cruss_normal_gma_plot_data$Percent) + 10)


#########################################
## NORMAL CRUSS + PR -> Absent Fidgety ##
#########################################

#Number of infants with gma_result = Poor repertoire and gma_result_v1 = Absent Fidgety movements
normal_cruss_abnormal_gma_mri %>%
  filter(
    gma_result == "Poor repertoire",
    gma_result_v1 == "Absent Fidgety movements"
  ) %>%
  summarise(n = n())
# 
# table(normal_cruss_abnormal_gma_mri$gma_result, normal_cruss_abnormal_gma_mri$gma_result_v1)

#table(normal_cruss_abnormal_gma_mri$gma_result, normal_cruss_abnormal_gma_mri$mri_requested)

normal_cruss_pr_af <- normal_cruss[gma_result %in% c("Poor repertoire") &
                                          gma_result_v1 %in% c("Absent Fidgety movements"), ]

normal_cruss_pr_n <- normal_cruss[gma_result %in% c("Poor repertoire") &
                                    gma_result_v1 %in% c("Normal fidgety"), ]

normal_cruss_pr_af_all_summary <- data.table(
  Variable = character(),
  Category = character(),
  N = integer(),
  Percent = numeric()
)


for (v in vars) {
  summary <- normal_cruss_pr_af[, .N, by = .(Category = get(v))][order(-N)]
  summary[, Percent := round(100 * N / sum(N), 1)]
  summary[, Variable := v]
  
  # Add total row
  total_row <- data.table(
    Variable = v,
    Category = "Total",
    N = sum(summary$N),
    Percent = 100
  )
  summary <- rbind(summary, total_row, fill = TRUE)
  
  normal_cruss_pr_af_all_summary <- rbind(normal_cruss_pr_af_all_summary, summary, fill = TRUE)
}

# Print all together with tabs
write.table(
  normal_cruss_pr_af_all_summary,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# create plot data
normal_cruss_pr_af_plot_data <- normal_cruss_pr_af_all_summary %>%
  filter(Category != "Total") %>%  # remove totals for plotting
  mutate(
    Variable = case_when(
      Variable == "gma_result"     ~ "GMA (Writhing Phase)",
      Variable == "gma_result_v1"  ~ "GMA (Fidgety Phase)",
      Variable == "hrcp"           ~ "High Risk of CP",
      Variable == "cp"             ~ "CP Diagnosis",
      Variable == "mri_requested"  ~ "MRI Requested",
      Variable == "hine_cat"       ~ "HINE Category",
      TRUE                         ~ Variable
    ),
    Category = case_when(
      Category == "0"      ~ "No",
      Category == "1"      ~ "Yes",
      Category == "NA"     ~ "Missing",
      Category == "Poor repertoire" ~ "Poor Repertoire",
      Category == "Absent Fidgety movements" ~ "Absent Fidgety",
      Category == "HRCP"   ~ "HRCP",
      TRUE                 ~ Category
    ),
    # order categories for plotting
    Category = factor(Category,
                      levels = c("Yes", "No", "Missing", 
                                 "Poor Repertoire", "Absent Fidgety",
                                 "HRCP"))
  ) %>%
  # order facet variables
  mutate(Variable = factor(Variable, levels = c(
    "GMA (Writhing Phase)",
    "GMA (Fidgety Phase)",
    "HINE Category",
    "High Risk of CP",
    "CP Diagnosis",
    "MRI Requested"
  )))

ggplot(normal_cruss_normal_gma_plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = paste0(Percent, "%")), 
            vjust = -0.3, size = 3.2, color = "black") +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = category_colors, na.value = "grey80") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11)
  ) +
  labs(
    title = "Normal CRUSS: Normal GM (at either writhing or fidgety age)",
    y = "Percent (%)"
  ) +
  expand_limits(y = max(normal_cruss_normal_gma_plot_data$Percent) + 10)

# TRANSITIONS PR - normal/adsent fidgety
table(normal_cruss$gma_result, normal_cruss$gma_result_v1)

pr_transitions <- normal_cruss %>%
  filter(gma_result == "Poor repertoire") %>%
  count(gma_result_v1) %>%
  mutate(percent = round(100 * n / sum(n), 1))

pr_transitions

ggplot(pr_transitions, aes(x = gma_result_v1, y = n, fill = gma_result_v1)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(n, " (", percent, "%)")), 
            vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(
    title = "Transitions from Poor Repertoire (Writhing Phase)",
    x = "Fidgety Phase Outcome",
    y = "Count"
  )
