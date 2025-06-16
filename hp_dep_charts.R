library(ggplot2)
library(plotly)

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



# Bayleys - motor
bayleys_motor_sesgroup_boxplot <-
  ggplot(dt, aes(x = SESgroups, y = motor)) +
  geom_boxplot(fill = "lightblue") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean point
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linetype = "dashed") +  # Connect means across groups
  #stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "red") +
  labs(title = "Bayleys Motor by SES Group",
       x = "SES Group",
       y = "Bayleys Motor Score") +
  theme_minimal()

bayleys_motor_sesgroup_boxplot

ggplotly(bayley_score_sesgroup_boxplot)

# Bayleys - cognitive
bayleys_cognitive_sesgroup_boxplot <-
  ggplot(dt, aes(x = SESgroups, y = cognitive)) +
  geom_boxplot(fill = "lightblue") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean point
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linetype = "dashed") +  # Connect means across groups
  #stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "red") +
  labs(title = "Bayleys Cognitive by SES Group",
       x = "SES Group",
       y = "Bayleys Cognitive Score") +
  theme_minimal()

bayleys_cognitive_sesgroup_boxplot

ggplotly(bayleys_cognitive_sesgroup_boxplot)

# Bayleys - language
bayleys_language_sesgroup_boxplot <-
  ggplot(dt, aes(x = SESgroups, y = language)) +
  geom_boxplot(fill = "lightblue") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean point
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linetype = "dashed") +  # Connect means across groups
  #stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "red") +
  labs(title = "Bayleys Language by SES Group",
       x = "SES Group",
       y = "Bayleys Language Score") +
  theme_minimal()

bayleys_language_sesgroup_boxplot

ggplotly(bayleys_language_sesgroup_boxplot)

# Bayleys - socio_emotional
bayleys_socio_emotional_sesgroup_boxplot <-
  ggplot(dt, aes(x = SESgroups, y = social_emotional)) +
  geom_boxplot(fill = "lightblue") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +  # Add mean point
  #stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linetype = "dashed") +  # Connect means across groups
  #stat_summary(fun = mean, geom = "crossbar", width = 0.3, color = "red") +
  labs(title = "Bayleys Socio-emotional by SES Group",
       x = "SES Group",
       y = "Bayleys Socio-Emotional Score") +
  theme_minimal()

bayleys_socio_emotional_sesgroup_boxplot

ggplotly(bayleys_socio_emotional_sesgroup_boxplot)
