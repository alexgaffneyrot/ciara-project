library(rmarkdown)
library("markdown")

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


# same for birth_weight
ggplot(dt, aes(x = birth_weight)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of Birth Weight")

# Q-Q plot: Points should fall on the line
qqnorm(dt$birth_weight)
qqline(dt$birth_weight, col = "red")

#Shapiro-Wilk test (for sample size < 5000):
# p < 0.05 = not normal
shapiro.test(dt$birth_weight)
shapiro.test(na.omit(dt$birth_weight))

# Skewness and Kurtosis
#Skewness ≠ 0 and Kurtosis ≠ 3 suggest deviation from normality.
skewness(na.omit(dt$birth_weight))
kurtosis(na.omit(dt$birth_weight))

# Summary using mean and standard deviation
bw_summary <- dt[, .(
  Mean = mean(birth_weight, na.rm = TRUE),
  SD = sd(birth_weight, na.rm = TRUE)
), by = firs_exposed]

# Format as "Mean (SD)"
bw_summary[, `Mean (SD)` := sprintf("%.2f (%.2f)", Mean, SD)]
bw_summary

# Non-normal
p_bwe <- wilcox.test(birth_weight ~ firs_exposed, data = dt)$p.value

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
# Gender
table(dt$gender)
dt[, gender_f := factor(gender, levels = c(0,1), labels = c("Female", "Male"))]

# Gestation
table(dt$gest_age)

# APGARS
# @ 1 min
class(dt$apgars_1)

##################
## FORMAT TABLE ##
##################

# Group 0 and 1 as "0+1", keep 2, and convert to factor on the fly
dt <- dt %>%
  mutate(
    #firs_exposed = factor(firs_exposed, levels = c(0, 1), labels = c("No FIR", "FIR")),
    steroids = case_when(
      steroids %in% c(0, 1) ~ "0+1",
      steroids == 2 ~ "2",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("0+1", "2"))
  )


#dt[, firs_exposed_f := factor(firs_exposed, levels = c("No FIRS","FIRS"), labels = c("Not Exposed", "Exposed"))]
class(dt$firs_exposed)
# Create your summary table
summary_tbl <- dt %>%
  select(
    firs_exposed, mat_age, clinical_chorio, rom, steroids,
    gest_age, birth_weight, gender, apgars_1, apgars_5, apgars_10
  ) %>%
  tbl_summary(
    by = firs_exposed,
    type = list(
      all_continuous() ~ "continuous2",
      all_categorical() ~ "categorical",
      steroids ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      mat_age ~ "Maternal age, yr (Median, IQR)",
      clinical_chorio ~ "Clinical Chorioamnionitis",
      rom ~ "ROM",
      steroids ~ "Antenatal steroids",
      gest_age ~ "Gestation",
      birth_weight ~ "Birth Weight",
      gender ~ "Sex",
      apgars_1 ~ "1 min",
      apgars_5 ~ "5 min",
      apgars_10 ~ "10 min"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(
    all_continuous() ~ "wilcox.test",
    all_categorical() ~ "chisq.test"
  )) %>%
  modify_spanning_header(
    all_stat_cols() ~ "**Exposure to FIR**"
  ) %>%
  modify_caption("**Table 1.** Relationship between Fetal Inflammatory Response (FIR) and Maternal and Infant characteristics."
  ) %>%
  modify_table_body(
    ~ bind_rows(
      tibble(
        variable = "maternal_header",
        label = "  Maternal Characteristics",
        row_type = "label"
      ),
      .x %>% filter(variable %in% c("mat_age", "clinical_chorio", "rom", "steroids")),
      tibble(
        variable = "infant_header",
        label = "  Infant Characteristics",
        row_type = "label"
      ),
      .x %>% filter(variable %in% c("gest_age", "birth_weight", "gender")),
      tibble(
        variable = "apgar_header",
        label = "  Apgar scores",
        row_type = "label"
      ),
      .x %>% filter(variable %in% c("apgars_1", "apgars_5", "apgars_10"))
    )
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::gtsave(filename = "table1.pdf")

summary_tbl

# remove as_gt() to save like this
summary_tbl %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "FIR_summary_table.docx")


# Descriptive stats
# continuous variables
hist(dt$birth_weight)
summary(dt$birth_weight)

hist(dt$gest_age)
summary(dt$gest_age)


# categorical variables
prop.table(table(dt$gender))

#Missingness by group
gg_miss_fct(dt, fct = firs_exposed)

#####

