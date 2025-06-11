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

compNDIdeath_tidymod <- tidy(compNDIdeath_mod
                             ,
                             exponentiate = TRUE,
                             conf.int = TRUE)

compNDIdeath_tidyMod_forest <- compNDIdeath_tidymod %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

vif(compNDIdeath_mod)

################################################################################

## compsevNDIdeath
compsevNDIdeath_mod <-
  tidy(
    glm(
      compsevNDIdeath ~ firs_exposed + clinical_chorio + gest_age + birth_weight_cat,
      data = dt,
      family = "binomial"
    )
    ,
    exponentiate = TRUE,
    conf.int = TRUE
  )


# Create tidy model with significance labels
compsevNDIdeath_tidyModel <- compNDIdeath_tidyModel %>%
  filter(term != "(Intercept)") %>%
  mutate(sig = ifelse(p.value < 0.05, "*", ""))

################################################################################

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
    title = "Forest Plot of Odds Ratios with 95% CI"
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

