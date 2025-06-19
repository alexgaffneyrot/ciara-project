library(MASS)
library(broom)
library(logistf)
library(lmtest)
library(sandwich)


# ordinal logistic regression
ses_mod <- polr(SESgroups ~  gest_age + motor + language + cognitive + asd,
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


# logistic regression
# compNDIdeath
compNDIdeath_mod_ses <-
  glm(
    compNDIdeath ~ SESgroups + gest_age + birth_weight + asd,#+ motor + language + cognitive,
    data = dt,
    family = "binomial"
  )


# table(dt$compNDIdeath, dt$SESgroups)
# table(dt$compNDIdeath, dt$firs_exposed)
# table(dt$compNDIdeath, dt$asd)

# check for multicollinearity
vif(compNDIdeath_mod_ses)

# tidy results
compNDIdeath_mod_ses_tidymod <- tidy(compNDIdeath_mod_ses,
                                     exponentiate = TRUE,
                                     conf.int = TRUE)

compNDIdeath_mod_ses_tidymod


# quasi complete seperation os ASD - use firth model
firth_model <- logistf(
  compNDIdeath ~ SESgroups + gest_age + birth_weight + asd,,
  data = dt
)
summary(firth_model)


################################################################################

# asd referral as outcome -  ses group + asd 

asd_ses_mod <-   glm(
  asd ~ SESgroups + gest_age + birth_weight,
  data = dt,
  family = "binomial"
)

asd_ses_tidymod <- tidy(asd_ses_mod,
                        exponentiate = TRUE,
                        conf.int = TRUE)
asd_ses_tidymod

#Affluent SES is significantly associated with reduced odds of ASD referral (OR = 0.0952, p = 0.006).
#Average SES also trends toward lower odds, but is marginally non-significant (p ≈ 0.085).


################################################################################

# bayleys as outcome

levels(dt$SESgroups)

# continuous but not normally distributed, which can violate the assumptions of ordinary least squares (OLS) regression - use Huber/White Sandwich SEs
# Linear Regression

# Motor
motor_lin_mod <- lm(motor ~ SESgroups + birth_weight + gest_age, data = dt)
coeftest(motor_lin_mod, vcov = vcovHC(motor_lin_mod, type = "HC1"))
#Average SES is associated with a ~5.4 point higher motor score.
#Affluent SES is associated with a ~8.1 point higher motor score.

# Cognitive
cog_lin_mod <- lm(cognitive ~ SESgroups + birth_weight + gest_age, data = dt)
coeftest(cog_lin_mod, vcov = vcovHC(cog_lin_mod, type = "HC1"))
#Average SES is associated with a ~6.9 point higher cognitive score.
#Affluent SES is associated with a ~10.2 point higher cognitive score.

# Language
lang_lin_mod <- lm(language ~ SESgroups + birth_weight + gest_age, data = dt)
coeftest(lang_lin_mod, vcov = vcovHC(lang_lin_mod, type = "HC1"))
#Average SES is associated with a ~9.8 point higher language score.
#Affluent SES is associated with a ~9.9 point higher language score.
#Higher birth weight is significantly associated with better language scores

# Social emotional
soc_em_lin_mod <- lm(social_emotional ~ SESgroups + birth_weight + gest_age, data = dt)
coeftest(soc_em_lin_mod, vcov = vcovHC(soc_em_lin_mod, type = "HC1"))
#Average SES is associated with a ~6.76 point higher social emotional score - not sig 
#Affluent SES is associated with a ~14.23 point higher social emotional score.

