library(dplyr) #For summarising datasets
library(nlme) #For nonlinear models
library(aomisc) #For logarithmic function
library(MuMIn) #For AICc
library(ggplot2) #For plotting
library(gghalves) #For half geoms in ggplot2


# Loading Dataframes -------------------------------------------------------
# Florea
florea <- read.csv("02_Datasets/ApisFlorea.csv")
str(florea)
florea$bee <- as.factor(florea$bee) # Changing Bee to a factor
florea$condition <- as.factor(florea$condition) # Changing condition to a factor

# Getting mean of runs per dance
flo <- florea %>% 
  group_by(condition, dist, bee, dance) %>% 
  dplyr::summarise(n.runs = n(), wag_phase=mean(wag_phase)/1000)

# cerana
cerana <- read.csv("02_Datasets/ApisCerana.csv")
str(cerana)
cerana$bee <- as.factor(cerana$bee) # Changing Bee to a factor
cerana$condition <- as.factor(cerana$condition) # Changing condition to a factor

# Getting mean of runs per dance
cer <- cerana %>% 
  group_by(condition, dist, bee, dance) %>% 
  dplyr::summarise(n.runs = n(), wag_phase=mean(wag_phase)/1000)


# In these data frames, the condition column has 2 values: 
# BG (Botanical Garden) which is the Dense vegetation condition or OF (Open Field) which is the Sparse vegetation Condition
# The waggle phase duration is in milliseconds, and the mean is divided by 1000 to get the duration value in seconds.


# Non linear curve determination ------------------------------------------

# The logarithmic function is a good fit for how the waggle phase duration changes with distance
# The function is of the form:
# Y ~ a + b*log(X)

# The aomisc package has a self-starting function that can be used with nls() called NLS.logCurve()
# https://www.statforbiology.com/2020/stat_nls_usefulfunctions/#logarithmic-function
# Once starting values are obtained from this function, we can then fit it to the nlme function with random effects



# 1. Obtain starting values for the NLMM ---------------------------------------------------------

# Starting values will be obtained by using nls() for the model without interaction (single a and b value) 
# and nlsList() for the model with interaction (separate a and b values for each vegetation condition.

# Florea
nls.flo <- nls(wag_phase~NLS.logCurve(dist,a,b), data = flo) # For model without interaction
summary(nls.flo) # a=-3.270, b=0.869
nls.flo.int <- nlsList(wag_phase~NLS.logCurve(dist,a,b)|condition, data = flo) # For model with interaction
summary(nls.flo.int) # BG: a=-4.000, b=1.032; OF: a=-2.443, b=0.692


# Cerana
nls.cer <- nls(wag_phase~NLS.logCurve(dist,a,b), data = cer) # For model without interaction
summary(nls.cer) # a=-4.555, b=1.046
nls.cer.int <- nlsList(wag_phase~NLS.logCurve(dist,a,b)|condition, data = cer) # For model with interaction
summary(nls.cer.int) # BG: a=-5.096, b=1.129; OF: a=-4.014, b=0.964



# 2. Fit the NLMMs using nlme --------------------------------------------------------

# Florea
# No covariate of condition
nlmm.flo.loga <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~1, random=b~1|bee, data = flo, start=c(-3.2, 0.8), control = nlmeControl(opt="nlminb", msMaxIter = 100))
# Both a and b vary with condition
nlmm.flo.loga.int <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~condition, random=b~1|bee, data = flo, start=c(-4, 1, -2.4, 0.69), control = nlmeControl(opt="nlminb", msMaxIter = 100))
# The random effects of a and b are highly correlated. Hence only using one of them in the model
anova(nlmm.flo.loga, nlmm.flo.loga.int)
# Interaction model is significantly better

# Cerana
# No covariate of condition
nlmm.cer.loga <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~1, random=b~1|bee, data = cer, start=c(-4.500, 1), control = nlmeControl(opt="nlminb", msMaxIter = 100))
# Interaction
nlmm.cer.loga.int <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~condition, random=b~1|bee, data = cer, start=c(-5, 1.1, -4, 1), control = nlmeControl(opt="nlminb", msMaxIter = 100))
# The random effects of a and b are highly correlated. Hence only using one of them in the model
anova(nlmm.cer.loga, nlmm.cer.loga.int)
# Model with interaction is not significantly better


# 3_1. Model Diagnostics -------------------------------------------------------------

# Florea - Logarithmic Regression - Interaction
plot(nlmm.flo.loga.int) # Variation looks reasonably spread out
qqnorm(nlmm.flo.loga.int$residuals)
qqline(nlmm.flo.loga.int$residuals) # Residuals seem more or less normally distributed.
nlmm.flo.loga.int.ranef <- ranef(nlmm.flo.loga.int)$b
qqnorm(nlmm.flo.loga.int.ranef)
qqline(nlmm.flo.loga.int.ranef) # Random effects also seem normally distributed.


# Cerana - Logarithmic Regression - No interaction
plot(nlmm.cer.loga) # Variation is skewed. Same issue as the linear models
qqnorm(nlmm.cer.loga$residuals)
qqline(nlmm.cer.loga$residuals) # Residuals seem more or less normally distributed.
nlmm.cer.loga.ranef <- ranef(nlmm.cer.loga)$b
qqnorm(nlmm.cer.loga.ranef)
qqline(nlmm.cer.loga.ranef) # Random effects also seem normally distributed.



# 3_2.Models to deal with heteroscedasticity in cerana nlmm -------------------
# To deal with the heteroscedasticity in the cerana dataset, a different variance relation would be used
# This is implemented through the weights option. The best amongst the various defined function for this data is varPower
# This considers that the variance changes with the covariate (distance) as distance^(2*theta)
# Theta is provided through trial and error, by visually inspecting the variance plots after each value of theta.
# A value of 10 seems to provide a good variance plot

nlmm.cer.loga.2 <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~1, random=b~1|bee, 
                        data = cer, start=c(-4.5, 1), weights = varPower(10,~dist), 
                        control = nlmeControl(opt="nlminb", msMaxIter = 100))

# plot(nlmm.cer.loga.cov.2)
nlmm.cer.loga.int.2 <- nlme(wag_phase~NLS.logCurve(dist,a,b), fixed=a+b~condition, random=b~1|bee, 
                          data = cer, start=c(-5, 1.1, -0.4, 1), weights = varPower(10,~dist), 
                          control = nlmeControl(opt="nlminb", msMaxIter = 100))


anova(nlmm.cer.loga.2, nlmm.cer.loga.int.2)
# The interaction model is significantly better


# 3_3.Diagnostics for cerana models with power variance -----------------------

# Cerana - Logarithmic Regression - Interaction - Power Variance structure
plot(nlmm.cer.loga.int.2) # Variation is much improved. Slight skew
qqnorm(nlmm.cer.loga.int.2$residuals)
qqline(nlmm.cer.loga.int.2$residuals) # Residuals seem more or less normally distributed.
nlmm.cer.loga.int.2.ranef <- ranef(nlmm.cer.loga.int.2)$b
qqnorm(nlmm.cer.loga.int.2.ranef)
qqline(nlmm.cer.loga.int.2.ranef) # The random effect plot is much improved compared to the earlier model



# 4_1. Model results -----------------------------------------------------------

# Florea
summary(nlmm.flo.loga.int)

# Cerana

# Output summary as csv
# wp_flo <- as.data.frame(summary(nlmm.flo.loga.int)$tTable)
# wp_flo$lci <- intervals(nlmm.flo.loga.int, which="fixed")$fixed[,1]
# wp_flo$uci <- intervals(nlmm.flo.loga.int, which="fixed")$fixed[,3]
# wp_flo$spc <- "Apis florea"
# wp_cer <- as.data.frame(summary(nlmm.cer.loga.int.2)$tTable)
# wp_cer$lci <- intervals(nlmm.cer.loga.int.2, which="fixed")$fixed[,1]
# wp_cer$uci <- intervals(nlmm.cer.loga.int.2, which="fixed")$fixed[,3]
# wp_cer$spc <- "Apis cerana"
# 
# wp_ef <- rbind(wp_flo, wp_cer)
# write.csv(wp_ef,"NLMMResults_WagglePhase.csv")


# 4_2.Predictions -------------------------------------------------------------

# Prediction dataframe
dist <- rep(seq(90,520, by=1),2)
condition = rep(c("BG","OF"), each=431)
pred <- as.data.frame(cbind(dist,condition))
str(pred)
pred$dist <- as.numeric(pred$dist)
pred$condition <- as.factor(pred$condition)

# Predicted values
# Florea
flo.loga.fits <- cbind(pred, fit=predict(nlmm.flo.loga.int, newdata = pred, level=0))
# Cerana
cer.loga.fits <- cbind(pred, fit=predict(nlmm.cer.loga.int.2, newdata = pred, level=0))

# Output CSVs
# write.csv(flo.loga.fits, "NLMMFits_WagglePhase_Florea.csv", row.names = F)
# write.csv(cer.loga.fits, "NLMMFits_WagglePhase_Cerana.csv", row.names = F)