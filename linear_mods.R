
# Motor
lm(motor~firs_exposed+birth_weight+gest_age, data=dt)

# Intercept (79.37): The expected motor score for a reference infant (with FIRS exposure = Yes, birth_weight=0, gest_age=0). Since birth_weight and gest_age aren’t zero in practice, the intercept is mostly a baseline.
# 
# firs_exposedNo FIRS (-4.58): Infants not exposed to FIRS have a motor score lower by about 4.58 points compared to infants exposed to FIRS, holding birth weight and gestational age constant. (Assuming "No FIRS" is the level being compared; check your factor reference level!)
# 
# birth_weight (0.00112): For each additional gram of birth weight, the motor score increases by about 0.0011 points. This suggests a small positive effect.
# 
# gest_age (0.72): For each additional week of gestational age, motor score increases by 0.72 points.

# Language
lm(language~firs_exposed+birth_weight+gest_age, data=dt)



# Cognitive
lm(cognitive~firs_exposed+birth_weight+gest_age, data=dt)
