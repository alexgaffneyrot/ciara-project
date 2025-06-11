# Logistic regression - binary outcome
#compNDIdeath
model1 <- glm(compNDIdeath ~ firs_exposed + gest_age + birth_weight_cat, 
              data = dt, 
              family = "binomial")

summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))

odds_ratios <- exp(coef(model1))
print(model1)

conf_int <- confint(model1)      # Default is 95% CI
or_conf_int <- exp(conf_int)
print(or_conf_int)

results <- cbind(
  OR = exp(coef(model1)),
  Lower_CI = exp(confint(model1)[, 1]),
  Upper_CI = exp(confint(model1)[, 2])
)
print(results)

tidy_model <- tidy(model1, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model)

vif(model1)

# intercept: This is the expected value of the outcome when all other variables are at their reference levels (e.g., baseline birth weight, FIRS present, gestational age = 0).
# Significant baseline odds. Very wide CI suggests potential overfitting or sparse data (probably because those values won't be 0, realistically)

#FIRS
#OR = 1.62: Subjects with No FIRS have 62% higher odds of the outcome than those with FIRS.
#Not statistically significant but this is still strange?


# GEST AGE
#OR = 0.713: Each additional week of gestation is associated with a 28.7% decrease in odds of the outcome (1 - 0.713).
#Significant effect: Strong evidence that increasing gestational age is protective.
#Clinical implication: Prematurity increases risk.

# BIRTH WEIGHT
#Compares Low Birth Weight (LBW) to Very Low Birth Weight (VLBW)).
#OR = 0.273: LBW infants have about 73% lower odds of the outcome than VLBW infants.
#Statistically significant: Suggests LBW is better (less risky) than VLBW, as expected.
#Interpretation: Odds of the outcome increase with lower birth weight.
# only on infant in NBW which is why that's all weird


############

# MULTIVARIATE LOG REGRESSION
# if two outcomes are correlated
# Create a contingency table
dt_outcomes <- table(dt$compNDIdeath, dt$compsevNDIdeath)
# Perform Chi-square test
chisq.test(dt_outcomes)
# check using lgoistic regression
model_corr <- glm(compNDIdeath ~ compsevNDIdeath, data = dt, family = binomial)
summary(model_corr)

# restructure data to long format

library(tidyr)
library(dplyr)
library("geepack")

dt_long <- dt %>%
  mutate(id = study_num) %>%                # create unique ID for each subject
  pivot_longer(
    cols = c(compNDIdeath, compsevNDIdeath),
    names_to = "outcome",
    values_to = "value"
  )

dt_long$value <- as.numeric(as.character(dt_long$value))

gee_model <- geeglm(
  value ~ firs_exposed + clinical_chorio + gest_age + birth_weight_cat,
  id = id,
  data = dt_long,
  family = binomial,
  corstr = "exchangeable"  # assumes correlation between the two outcomes within each subject
)

summary(gee_model)

# Add interaction terms so predictor effects can differ by outcome
gee_model_int <- geeglm(
  value ~ outcome * (firs_exposed + clinical_chorio + gest_age + birth_weight_cat),
  id = id,
  data = dt_long,
  family = binomial,
  corstr = "exchangeable"
)

summary(gee_model_int)

# predicted probabilities 
newdata <- expand.grid(
  outcome = unique(dt_long$outcome),
  firs_exposed = c("Yes FIRS", "No FIRS"),
  clinical_chorio = c("Yes", "No"),
  gest_age = c(30, 35, 40),
  birth_weight_cat = c("Normal", "LBW")
)

# Predict on the link scale (log-odds)
newdata$pred_logit <- predict(gee_model_int, newdata, type = "link")

# Convert to probability scale
newdata$pred_prob <- plogis(newdata$pred_logit)

head(newdata)

