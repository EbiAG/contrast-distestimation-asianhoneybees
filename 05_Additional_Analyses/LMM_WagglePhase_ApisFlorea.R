library(dplyr) # For data manipulation
library(fitdistrplus) # Fitting distributions
library(nlme) # Fitting LMM
library(ggplot2) # Plotting Predictions
library(cowplot) # Saving ggplot

# 0. Loading and Cleaning Dataframes -------------------------------------------------------
# Florea
florea <- read.csv("02_Datasets/ApisFlorea.csv")
str(florea)
florea$bee <- as.factor(florea$bee) # Changing Bee to a factor
florea$condition <- as.factor(florea$condition) # Changing condition to a factor

# Getting mean of runs per dance
flo <- florea %>% 
  group_by(condition, dist, bee, dance) %>% 
  dplyr::summarise(n.circuits = n(), wag_phase=mean(wag_phase)/1000,)

# In these data frames, the condition column has 2 values: 
# BG (Botanical Garden) which is the Dense vegetation condition or OF (Open Field) which is the Sparse vegetation Condition
# The waggle phase duration is in milliseconds, and the mean is divided by 1000 to get the duration value in seconds.


# 1. LMM on waggle phase duration -----------------------------------------
# Model with interaction
wp_lmm_inter <- lme(wag_phase~dist*condition, random=~1|bee, data=d.runs, method="ML")
# Model without interaction
wp_lmm_nointer <- lme(wag_phase~dist+condition, random=~1|bee, data=d.runs, method="ML")

anova(wp_lmm_nointer, wp_lmm_inter)
# Model with interaction is significantly different


# 2. Model diagnostics ----------------------------------------------------
plot(wp_lmm_inter)
# Looks fine
qqnorm(resid(wp_lmm_inter))
qqline(resid(wp_lmm_inter))
# Seems to fit really well


# 3. Model results --------------------------------------------------------
summary(wp_lmm_inter)
# Slope is significantly different, p = 0.0026

# Output summary as CSV
# wp_ef <- as.data.frame(summary(wp_lmm_inter)$tTable)
# wp_ef$lci <- intervals(wp_lmm_inter)$fixed[,1]
# wp_ef$uci <- intervals(wp_lmm_inter)$fixed[,3]
# write.csv(wp_ef, "Summary_Florea_WagglePhase_LMM.csv")


# 4. Predictions -------------------------------------------------------------
wp_pred1 <- ggpredict(wp_lmm_inter, terms=c("dist [80:520]", "condition"), type="fe")
plot(wp_pred1)

# write.csv(wp_pred1, "Predictions_Florea_WagglePhase_LMM.csv", row.names = F)