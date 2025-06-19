library(ggplot2)
library(plotly)
library(moments)
library(dunn.test)
library(ggsignif)
library(dplyr)

# Overall distribution
ggplot(dt_complete, aes(x = hp_dep_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Deprivation Scores", x = "Deprivation Score", y = "Count")

# Distribution by FIRS exposure
ggplot(dt_complete, aes(x = hp_dep_score, fill = firs_exposed)) +
  geom_density(alpha = 0.5) +
  labs(title = "Deprivation Scores by FIRS Exposure", x = "Deprivation Score", y = "Density")

# boxplot / violin
ggplot(dt_complete, aes(x = firs_exposed, y = hp_dep_score, fill = firs_exposed)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Deprivation Scores by FIRS Exposure", x = "FIRS Exposure", y = "Deprivation Score")

ggplot(dt_complete, aes(x = firs_exposed, y = hp_dep_score, fill = firs_exposed)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Deprivation Scores by FIRS Exposure", x = "FIRS Exposure", y = "Deprivation Score")

# scatter plot
ggplot(dt_complete, aes(x = hp_dep_score, y = birth_weight, color = firs_exposed)) +
  geom_point(alpha = 0.6) +
  labs(title = "Ventilation Days vs Deprivation Score", x = "Deprivation Score", y = "Days on Ventilation")


# Barplot counts by category and group
ggplot(dt_complete, aes(x = SESgroups, fill = firs_exposed)) +
  geom_bar(position = "dodge") +
  labs(title = "Deprivation Categories by FIRS Exposure", x = "Deprivation Category", y = "Count")

# boxplot and outcome
ggplot(dt_complete, aes(x = factor(compNDIdeath), y = hp_dep_score, fill = factor(compNDIdeath))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No Event", "Death/NDI")) +
  labs(title = "Deprivation Scores by Outcome", x = "Outcome", y = "Deprivation Score")


#How to visualize proportions (weighted by group size):
#1. Bar plot showing proportions (normalized counts):

ggplot(dt, aes(x = SESgroups)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of SES Groups", x = "SES Group", y = "Percentage") +
  theme_minimal()


#2. Use geom_bar(position = "fill") for stacked bar plots showing proportions within groups:
ggplot(dt, aes(x = SESgroups, fill = firs_exposed)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of FIRS Exposure Within SES Groups",
       x = "SES Group", y = "Proportion") +
  theme_minimal()


# length of follow up 
length_fu_sesgroup_boxplot <-
ggplot(dt, aes(x = SESgroups, y = length_fu)) +
  geom_boxplot(fill = "lightblue") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean point
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linetype = "dashed") +  # Connect means across groups
  #stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "red") +
  labs(title = "Length of Follow-Up by SES Group",
       x = "SES Group",
       y = "Follow-Up Duration") +
  theme_minimal()

ggplotly(length_fu_sesgroup_boxplot)

################################################################################
# BAYLEYS
# check is motor score normally distributed
ggplot(dt, aes(x = motor)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Bayleys Motor Scores")

ggplot(dt, aes(sample = motor)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Bayleys Motor")
shapiro.test(dt$motor)
skewness(dt$motor, na.rm = TRUE)
kurtosis(dt$motor, na.rm = TRUE)

# data is not normal and more than 2 groups - use kruskal-wallis
kruskal.test(motor ~ SESgroups, data = dt)

# reject null hypothesis that the median motor scores are the same accross all the SES groups 

# see where the diff is - dunn's test
dunn.test::dunn.test(dt$motor, dt$SESgroups, method = "bonferroni")

# Summary by group
summary_table <- dt[, .(
  count = .N,
  mean_motor = mean(motor, na.rm = TRUE),
  median_motor = median(motor, na.rm = TRUE)
), by = SESgroups]

summary_table

#There is a significant difference between Disadvantaged and Affluent groups (p = 0.0061).
# borderline disadvantaged and average 0.0631

# max y for positioning lines a bit above boxplots
max_mot <- max(dt$motor, na.rm = TRUE)
line_y1 <- max_lang + 1      # significant line height
line_y2 <- max_lang + 8   # second line height

ggplot(dt, aes(x = SESgroups, y = motor, fill = SESgroups)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Motor Scores by SES Group",
       x = "SES Group",
       y = "Motor Score") +
  
  # Significant comparison (red)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Affluent")),
    annotations = "p = 0.0061",
    y_position = line_y1,
    tip_length = 0.02,
    textsize = 5,
    color = "red"
  ) +
  
  # Borderline comparison (gray)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Average")),
    annotations = "p = 0.0631",
    y_position = line_y2,
    tip_length = 0.02,
    textsize = 5,
    color = "gray50"
  )


# Bayleys - cognitive
ggplot(dt, aes(x = cognitive)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Bayleys Cognitive Scores")

ggplot(dt, aes(sample = cognitive)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Bayleys Cognitive")

shapiro.test(dt$cognitive)
skewness(dt$cognitive, na.rm = TRUE)
kurtosis(dt$cognitive, na.rm = TRUE)

# data is not normal and more than 2 groups - use kruskal-wallis
kruskal.test(cognitive ~ SESgroups, data = dt)

# reject null hypothesis that the median cognitive scores are the same accross all the SES groups 

dunn.test::dunn.test(dt$cognitive, dt$SESgroups, method = "bonferroni")

# infants in the Disadvantaged group had significantly lower cognitive scores than those in the:
# Affluent group (p = 0.0094)
# Average group (p = 0.0382)

# Summary by group
summary_table <- dt[, .(
  count = .N,
  mean_motor = mean(cognitive, na.rm = TRUE),
  median_motor = median(cognitive, na.rm = TRUE)
), by = SESgroups]

summary_table

# max y for positioning lines a bit above boxplots
max_cog <- max(dt$cognitive, na.rm = TRUE)
line_y1 <- max_lang + 7      # significant line height
line_y2 <- max_lang + 15   # second line height

ggplot(dt, aes(x = SESgroups, y = cognitive, fill = SESgroups)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Cognitive Scores by SES Group",
       x = "SES Group",
       y = "Cognitive Score") +
  
  # Significant comparison (red)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Affluent")),
    annotations = "p = 0.0094",
    y_position = line_y1,
    tip_length = 0.02,
    textsize = 5,
    color = "red"
  ) +
  
  # Significant comparison 2 (red)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Average")),
    annotations = "p = 0.0382",
    y_position = line_y2,
    tip_length = 0.02,
    textsize = 5,
    color = "red"
  )


# Bayleys - language
ggplot(dt, aes(x = language)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Bayleys Language Scores")

ggplot(dt, aes(sample = language)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Bayleys Language")

shapiro.test(dt$language)
skewness(dt$language, na.rm = TRUE)
kurtosis(dt$language, na.rm = TRUE)

# not normally distributed - more than 2 groups - use kruskal-wallis
kruskal.test(language ~ SESgroups, data = dt)

# cannot reject null hypothesis - however borderline p value so do dunn test anyway

dunn.test::dunn.test(dt$language, dt$SESgroups, method = "bonferroni")

# dis - average : sig difference 0.0263
# dis - affluent : borderline 0.0639

# Summary by group
summary_table <- dt[, .(
  count = .N,
  mean_motor = mean(language, na.rm = TRUE),
  median_motor = median(language, na.rm = TRUE)
), by = SESgroups]

summary_table

max_lang <- max(dt$language, na.rm = TRUE)
line_y1 <- max_lang + 7      # significant line height
line_y2 <- max_lang + 15   # borderline line height

ggplot(dt, aes(x = SESgroups, y = language, fill = SESgroups)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Language Scores by SES Group",
       x = "SES Group",
       y = "Language Score") +
  
  # Significant comparison (red)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Average")),
    annotations = "p = 0.0263",
    y_position = line_y1,
    tip_length = 0.02,
    textsize = 5,
    color = "red"
  ) +
  
  # Borderline comparison (gray)
  geom_signif(
    comparisons = list(c("Disadvantaged", "Affluent")),
    annotations = "p = 0.064",
    y_position = line_y2,
    tip_length = 0.02,
    textsize = 5,
    color = "gray50"
  )


# Bayleys - socio_emotional
# check is score normally distributed
ggplot(dt, aes(x = social_emotional)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Bayleys social_emotional Scores")

ggplot(dt, aes(sample = social_emotional)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for social_emotional Motor")

shapiro.test(dt$social_emotional)
skewness(dt$social_emotional, na.rm = TRUE)
kurtosis(dt$social_emotional, na.rm = TRUE)


# data is not normal and more than 2 groups - use kruskal-wallis
kruskal.test(social_emotional ~ SESgroups, data = dt)

# reject null hypothesis that the median social_emotional scores are the same accross all the SES groups 

# see where the diff is - dunn's test
dunn.test::dunn.test(dt$social_emotional, dt$SESgroups, method = "bonferroni")


# Summary by group
summary_table <- dt[, .(
  count = .N,
  mean_motor = mean(social_emotional, na.rm = TRUE),
  median_motor = median(social_emotional, na.rm = TRUE)
), by = SESgroups]

summary_table


max_lang <- max(dt$social_emotional, na.rm = TRUE)
line_y1 <- max_lang + 7      # significant line height
line_y2 <- max_lang + 15   # borderline line height
ggplot(dt, aes(x = SESgroups, y = social_emotional, fill = SESgroups)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Social Emotional Scores by SES Group",
       x = "SES Group",
       y = "Social Emotional Score") +
  
  # Significant line: Disadvantaged vs Affluent
  geom_signif(
    comparisons = list(c("Disadvantaged", "Affluent")),
    annotations = "p = 0.015",
    y_position = line_y1,
    tip_length = 0.02,
    textsize = 5,
    color = "red"
  ) +
  
  # Trend line: Average vs Affluent
  geom_signif(
    comparisons = list(c("Average", "Affluent")),
    annotations = "p = 0.079",
    y_position = line_y2,
    tip_length = 0.02,
    textsize = 5,
    color = "gray50"
  )
