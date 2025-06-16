library(dplyr)
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
kruskal_has <- kruskal.test(length_fu ~ SESgroups, data = dt_has_bayley)
kruskal_has

# Data without Bayley
dt_no_bayley <- filter(dt, bayley_score == 0)
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

