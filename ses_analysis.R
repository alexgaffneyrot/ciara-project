library(dplyr)
library(ARTool)

# ALL - bayleys and no bayleys, FIRs no FIRs

hist(dt$length_fu, main="Histogram of Follow-up Time", xlab="Follow-up Time")
qqnorm(dt$length_fu)
qqline(dt$length_fu, col = "red")
# not normally distributed - kruscal-wallis test

# Boxplot of follow-up time by SES
ggplot(dt, aes(x = SESgroups, y = length_fu)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Follow-up Time by SES Group")

# Kruskal-Wallis test
kruskal.test(length_fu ~ SESgroups, data = dt)

# no sig difference in fu time between SES groups

# BAYLEYS VS NO BAYLEYS

library(dplyr)

# Data with Bayley
dt_has_bayley <- filter(dt, bayley_score == 1)
dim(dt_has_bayley)
krusdt_has_bayleykruskal_has <- kruskal.test(length_fu ~ SESgroups, data = dt_has_bayley)
kruskal_has

# Data without Bayley
dt_no_bayley <- filter(dt, bayley_score == 0)
dim(dt_no_bayley)
kruskal_no <- kruskal.test(length_fu ~ SESgroups, data = dt_no_bayley)
kruskal_no

#Whether participants have Bayley scores or not, follow-up time does not 
#significantly differ between SES groups.
#This supports the idea that SES groups do not differ in follow-up duration in sample.

table(dt$bayley_score)

ggplot(dt, aes(x = SESgroups, y = length_fu, fill = SESgroups)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ bayley_score) +
  labs(title = "Follow-up Time by SES Group, Split by Bayley Status",
       x = "Socio-Economic Group",
       y = "Length of Follow-up (time units)") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(dt, aes(x = SESgroups, y = length_fu, color = factor(bayley_score), group = bayley_score)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Mean Follow-up Time by SES Group and Bayley Status",
       x = "SES Group", y = "Mean Follow-up Time",
       color = "Bayley Score") +
  theme_minimal()


# fu time not normally distributed - Aligned Rank Transform (ART)
dt_clean <- dt %>% 
  filter(!is.na(length_fu), !is.na(SESgroups), !is.na(bayley_score)) %>%
  mutate(
    SESgroups = factor(SESgroups),
    bayley_score = factor(bayley_score)
  )

art_model <- art(length_fu ~ SESgroups * bayley_score, data = dt_clean)
anova(art_model)

#Follow-up time differs between SES groups, but this difference changes depending on whether participants have Bayley scores or not.
#The interaction suggests that the relationship between SES and follow-up time is not the same for those with Bayley scores compared to those without.
#This matches what your graph showed — a visible difference in patterns split by Bayley status.


library(emmeans)

# Extract the linear model inside the ART object
lm_model <- artlm(art_model, "SESgroups:bayley_score")

# Now run emmeans on this lm model
emm <- emmeans(lm_model, pairwise ~ SESgroups | bayley_score)

summary(emm$contrasts, adjust = "BH")


ggplot(dt_clean, aes(x = SESgroups, y = length_fu, color = bayley_score, group = bayley_score)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(
    title = "Mean Follow-up Time by SES Group and Bayley Status",
    x = "SES Group",
    y = "Mean Follow-up Time",
    color = "Bayley Status"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))






# FIRS VS NO FIRS

table(dt$firs_exposed)

# Subset: Exposed to FIRS
dt_firs_yes <- filter(dt, firs_exposed == "FIRS")
kruskal_firs_yes <- kruskal.test(length_fu ~ SESgroups, data = dt_firs_yes)
print(kruskal_firs_yes)

# Subset: Not exposed to FIRS
dt_firs_no <- filter(dt, firs_exposed == "No FIRS")
kruskal_firs_no <- kruskal.test(length_fu ~ SESgroups, data = dt_firs_no)
print(kruskal_firs_no)


ggplot(dt, aes(x = SESgroups, y = length_fu, fill = SESgroups)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ firs_exposed) +
  labs(title = "Follow-up Time by SES Group, Split by FIRS Exposure",
       x = "SES Group",
       y = "Follow-up Length") +
  theme_minimal() +
  theme(legend.position = "none")

