library(MASS)
library(broom)
# ordinal logistic regression
ses_mod <- polr(SESgroups ~ firs_exposed + gest_age + motor + language + cognitive + asd,
                data = dt,
                Hess = TRUE)
summary(ses_mod)

vif(ses_mod)


# Tidy the polr model output
tidy_ses_mod <- tidy(ses_mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)

tidy_ses_mod

#SESgroups ~ firs_exposed + gest_age + birth_weight
# vif(ses_mod)
#firs_exposed     gest_age birth_weight 
#2.783894     9.828801     7.999996
# drop birth weight 

#SESgroups ~ firs_exposed + gest_age
# gest_age t value 2.26 
# exp(0.092) ≈ 1.096
#The odds of being in a higher SES group are about 9.6% higher per week of gestation


# SESgroups ~ firs_exposed + gest_age + motor + language + cognitive + asd
#After accounting for motor, language, cognitive scores, gestational age, 
#and FIRS exposure, an ASD diagnosis is associated with a notably lower l
#ikelihood of being in higher SES groups.
#Other variables don’t appear to significantly differentiate SES groups in this model.
