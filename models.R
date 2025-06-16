library("ResourceSelection")
library("pROC")
library("ggeffects")
library(mgcv)
library(broom)

################################################################################
dt$firs_exposed <- relevel(dt$firs_exposed, ref = "FIRS")  # set "FIRS" as reference

# compNDIdeath
compNDIdeath_mod <-
  glm(
    compNDIdeath ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compNDIdeath_mod)

# tidy results
compNDIdeath_tidymod <- tidy(compNDIdeath_mod
                             ,
                             exponentiate = TRUE,
                             conf.int = TRUE)

# Create tidy model with significance labels
compNDIdeath_tidyMod_forest <- compNDIdeath_tidymod %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

# Forest plot
ggplot(compNDIdeath_tidyMod_forest, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compNDIdeath"
  ) +
  theme_minimal()

##############
# Motor only #
##############

# compNDIdeath - motor
compNDIdeath_mod_motor <-
  glm(
    compNDIdeath_Motor ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compNDIdeath_mod_motor)

# tidy results
compNDIdeath_tidymod_motor <- tidy(compNDIdeath_mod_motor
                             ,
                             exponentiate = TRUE,
                             conf.int = TRUE)

# Create tidy model with significance labels
compNDIdeath_tidyMod_forest_motor <- compNDIdeath_tidymod_motor %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

# Forest plot
ggplot(compNDIdeath_tidyMod_forest_motor, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compNDIdeath Motor"
  ) +
  theme_minimal()

##################
# Cognitive only #
##################

compNDIdeath_mod_cog <-
  glm(
    compNDIdeath_Cognitive ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compNDIdeath_mod_cog)

# tidy results
compNDIdeath_tidymod_cog <- tidy(compNDIdeath_mod_cog
                             ,
                             exponentiate = TRUE,
                             conf.int = TRUE)

# Create tidy model with significance labels
compNDIdeath_tidyMod_forest_cog <- compNDIdeath_tidymod_cog %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

# Forest plot
ggplot(compNDIdeath_tidyMod_forest_cog, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compNDIdeath Cognitive"
  ) +
  theme_minimal()

#################
# Language only #
#################

compNDIdeath_mod_lang <-
  glm(
    compNDIdeath_Language ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compNDIdeath_mod_lang)

# tidy results
compNDIdeath_tidymod_lang <- tidy(compNDIdeath_mod_lang
                             ,
                             exponentiate = TRUE,
                             conf.int = TRUE)

# Create tidy model with significance labels
compNDIdeath_tidyMod_forest_lang <- compNDIdeath_tidymod_lang %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

# Forest plot
ggplot(compNDIdeath_tidyMod_forest_lang, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compNDIdeath Language"
  ) +
  theme_minimal()


################################################################################

## compsevNDIdeath
compsevNDIdeath_mod <-
  glm(
    compsevNDIdeath ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )
    
# check for multicollinearity 
vif(compsevNDIdeath_mod)

# tidy results
compsevNDIdeath_tidymod <- tidy(compsevNDIdeath_mod
                                    ,
                                    exponentiate = TRUE,
                                    conf.int = TRUE)


# Create tidy model with significance labels
compsevNDIdeath_tidyMod_forest <- compsevNDIdeath_tidymod %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))


# Forest plot
ggplot(compsevNDIdeath_tidyMod_forest, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compsevNDIdeath"
  ) +
  theme_minimal()


##############
# Motor only #
##############


compsevNDIdeath_mod_Motor <-
  glm(
    compsevNDIdeath_Motor ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compsevNDIdeath_mod_Motor)

# tidy results
compsevNDIdeath_tidymod_Motor <- tidy(compsevNDIdeath_mod_Motor
                                ,
                                exponentiate = TRUE,
                                conf.int = TRUE)


# Create tidy model with significance labels
compsevNDIdeath_tidyMod_forest_motor <- compsevNDIdeath_tidymod_Motor %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))


# Forest plot
ggplot(compsevNDIdeath_tidyMod_forest_motor, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compsevNDIdeath Motor"
  ) +
  theme_minimal()

##################
# Cognitive only #
##################

compsevNDIdeath_mod_cog <-
  glm(
    compsevNDIdeath_Cognitive ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compsevNDIdeath_mod_cog)

# tidy results
compsevNDIdeath_tidymod_cog <- tidy(compsevNDIdeath_mod_cog
                                ,
                                exponentiate = TRUE,
                                conf.int = TRUE)


# Create tidy model with significance labels
compsevNDIdeath_tidyMod_forest_cog <- compsevNDIdeath_tidymod_cog %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))


# Forest plot
ggplot(compsevNDIdeath_tidyMod_forest_cog, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compsevNDIdeath Cognitive"
  ) +
  theme_minimal()


#################
# Language only #
#################
compsevNDIdeath_mod_lang <-
  glm(
    compsevNDIdeath_Language ~ firs_exposed + gest_age + birth_weight,
    data = dt,
    family = "binomial"
  )

# check for multicollinearity 
vif(compsevNDIdeath_mod_lang)

# tidy results
compsevNDIdeath_tidymod_lang <- tidy(compsevNDIdeath_mod_lang
                                ,
                                exponentiate = TRUE,
                                conf.int = TRUE)


# Create tidy model with significance labels
compsevNDIdeath_tidyMod_forest_lang <- compsevNDIdeath_tidymod_lang %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))


# Forest plot
ggplot(compsevNDIdeath_tidyMod_forest_lang, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(estimate, 2), sig)), hjust = -0.2) +
  scale_x_log10() +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Forest Plot of Odds Ratios with 95% CI - compsevNDIdeath Language"
  ) +
  theme_minimal()






################################################################################

# interaction terms
# Including the interaction between gestational age and birth_weight_cat tests whether the effect of one depends on the other

# Model with interaction term
model_interaction <- glm(
  compsevNDIdeath ~ firs_exposed + clinical_chorio + gest_age * birth_weight_cat,
  data = dt,
  family = "binomial"
)

tidy(model_interaction, exponentiate = TRUE, conf.int = TRUE)


# model diagnostics 
#ROC Curve and AUC
roc_obj <- roc(dt$compNDIdeath, fitted(compNDIdeath_mod))
plot(roc_obj, main = "ROC Curve - Logistic Model with Interaction")
auc(roc_obj)

# # Generate predicted probabilities for a grid of gest_age and birth_weight_100g
# pred <- ggpredict(compNDIdeath_mod, terms = c("gest_age", "birth_weight_cat [all]"))
# 
# library(ggplot2)
# ggplot(pred, aes(x = x, y = predicted, color = group)) +
#   geom_line(size=1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
#   labs(
#     x = "Gestational Age (weeks)",
#     y = "Predicted Probability of compsevNDIdeath",
#     color = "Birth Weight (per 100g)",
#     fill = "Birth Weight (per 100g)",
#     title = "Predicted Probability by Gestational Age and Birth Weight"
#   ) +
#   theme_minimal()

dt$length_fu

# GAM model
gam_model <- gam(compsevNDIdeath ~ firs_exposed + clinical_chorio + s(gest_age) + birth_weight_cat, family=binomial, data=dt)
summary(gam_model)
tidy(gam_model, exponentiate = TRUE, conf.int = TRUE)
plot(gam_model, shade = TRUE, rug = TRUE)

