library(dplyr) # For data manipulation
library(stringr) #For string mnipulation
library(fitdistrplus) # Fitting distributions
library(glmmTMB) # Fitting negative binomial distribution
library(DHARMa) # Checking Model assumptions for glmmTMB
library(insight) # Obtaining predictions using ggeffcts
library(ggeffects) # Obtaining predictions from lmm and glmm models
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
  dplyr::summarise(n.circuits = n())

# cerana
cerana <- read.csv("02_Datasets/ApisCerana.csv")
str(cerana)
cerana$bee <- as.factor(cerana$bee) # Changing Bee to a factor
cerana$condition <- as.factor(cerana$condition) # Changing condition to a factor

# Getting mean of runs per dance
cer <- cerana %>% 
  group_by(condition, dist, bee, dance) %>% 
  dplyr::summarise(n.circuits = n())


# In these data frames, the condition column has 2 values: 
# BG (Botanical Garden) which is the Dense vegetation condition or OF (Open Field) which is the Sparse vegetation Condition


# 1. Determining the distribution ------------------------------------------
# Since this is a number, a discrete probability distribution will be used for the error structure

# Florea
fit.flo.dc1 <- fitdist(flo$n.circuits, "pois")
plot(fit.flo.dc1)
fit.flo.dc2 <- fitdist(flo$n.circuits, "nbinom")
plot(fit.flo.dc2)
fit.flo.dc3 <- fitdist(flo$n.circuits, "geom")
plot(fit.flo.dc3)
comp.flo.dc <- gofstat(list(fit.flo.dc1, fit.flo.dc2, fit.flo.dc3))
comp.flo.dc
# The AIC values clearly indicate that the nbinom distribution is better

# Cerana
fit.cer.dc1 <- fitdist(cer$n.circuits, "pois")
plot(fit.cer.dc1)
fit.cer.dc2 <- fitdist(cer$n.circuits, "nbinom")
plot(fit.cer.dc2)
fit.cer.dc3 <- fitdist(cer$n.circuits, "geom")
plot(fit.cer.dc3)
comp.cer.dc <- gofstat(list(fit.cer.dc1, fit.cer.dc2, fit.cer.dc3))
comp.cer.dc
# The AIC values clearly indicate that the nbinom distribution is better


# 2. Fitting the negative binomial GLMM ---------------------------------------------------------

# Florea
flo_dc_nb <- glmmTMB(n.circuits~dist*condition + (1|bee),  data=flo, family = nbinom1(link="log"))

# Cerana
cer_dc_nb <- glmmTMB(n.circuits~dist*condition + (1|bee),  data=cer, family = nbinom1(link="log"))


# 3. Model Diagnostics --------------------------------------------------------

# Florea
sim_flo <- simulateResiduals(fittedModel = flo_dc_nb, n = 1000)
plot(sim_flo)
# The deviation in QQ plot is not significant. The residuals versus predicted plot shows some slight skew
testDispersion(sim_flo)
# The dispersion value is 1.03. Very close to 1, and p is 0.8. No significant dispersion

# Cerana
sim_cer <- simulateResiduals(fittedModel = cer_dc_nb, n = 1000)
plot(sim_cer)
# The deviation in QQ plot is not significant. The residuals versus predicted plot shows some slight skew in the middle
testDispersion(sim_cer)
# The dispersion value is 1.07. Very close to 1, and p is 0.4. No significant dispersion


# 4_1. Model Results -------------------------------------------------------------

# Florea
summary(flo_dc_nb)
# No significant difference between intercept and slope for the two conditions
# Open and Dense conditions have similar number of dance circuits

# Florea
summary(cer_dc_nb)
# No significant difference between intercept and slope for the two conditions
# Open and Dense conditions have similar number of dance circuits


# Output summary as csv
# dc_flo <- as.data.frame(summary(flo_dc_nb)$coefficients$cond)
# dc_flo$lci <- confint(flo_dc_nb)[-5,][,1]
# dc_flo$uci <- confint(flo_dc_nb)[-5,][,2]
# dc_flo$spc <- "Apis florea"
# dc_cer <- as.data.frame(summary(cer_dc_nb)$coefficients$cond)
# dc_cer$lci <- confint(cer_dc_nb)[-5,][,1]
# dc_cer$uci <- confint(cer_dc_nb)[-5,][,2]
# dc_cer$spc <- "Apis cerana"
# 
# dc_ef <- rbind(dc_flo, dc_cer)
# write.csv(dc_ef,"GLMMResults_DanceCircuits.csv")


# 4_2.Predictions -------------------------------------------------------------

# Florea
flo_dc_pred <- ggpredict(flo_dc_nb, terms=c("dist [80:520]", "condition"), type="fe")
plot(flo_dc_pred)

# Cerana
cer_dc_pred <- ggpredict(cer_dc_nb, terms=c("dist [80:520]", "condition"), type="fe")
plot(cer_dc_pred)

# Output CSVs
# write.csv(flo_dc_pred, "GLMMFits_DanceCircuits_Florea.csv", row.names = F)
# write.csv(cer_dc_pred, "GLMMFits_DanceCircuits_Cerana.csv", row.names = F)