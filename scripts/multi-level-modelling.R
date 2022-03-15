# Multilevel Modelling in R
# Tutorial Video 1: Unconditional Means Model 
# (https://www.youtube.com/watch?v=cr9RpSgRYVw)

# Research Q: Can we reject the NULL hyp. that the 
# grand mean for the intercept is equal to zero?

library(tidyverse)

# STEP 1: Read in dataset 
apt <- read.csv("data/2021-09-30_SLI_aptitude_data.csv")
# Make sure file is a .csv and not a .xls worksheet
# Make sure data is in long format 
# i.e. multiple rows per ppt per session.

# !!! BEGIN TIMEPOINTS WITH 0, not 1 !!!
# else you would be skewing your results.
names(apt)
apt <- rename(apt, session = "ï..session")
apt <- apt %>% mutate(across(c(session, ppt), as.factor))
apt$session <- plyr::revalue(apt$session, c("1" = "0", 
                                            "2" = "1", 
                                            "3" = "2"))
levels(apt$session)

apt$ppt <- gsub("HW|WV", "", apt$ppt) # replace HW or WV code with
levels(apt$ppt)
apt$ppt <- as.numeric(apt$ppt) # as numeric
apt$ppt

# STEP 2: Open {nlme} package
# nlme: linear and nonlinear mixed effects models
library(nlme)
# there are other packages available incl. {lme4}

# STEP 3: Examine head of data
head(apt)

# STEP 4: Run unconditional means model (UMM) w/ summary,
# using ppt as the nested variable/random effect.
# So ppts are nested within themselves compared at diff time points
# Some might think to use e.g. group or uni as the random effect
# since it is another level of clustering, but this is a 
# longitudinal design, so must be nested based on individuals.
# The first ~1 refers to no slope/random effects at this point

nback1 <- apt %>% filter(nback_lett != "NA") # first remove NA rows

mod1 <- lme(nback_lett~1, random=~1|ppt, data=nback1, method="ML")
summary(mod1)


# the logLik value is 95.4666. Ideally this gets smaller as the model
# builds up as the model improves in fit. Then we can test if the 
# decrease is different from original model.
# Random effects StdDev Intercept & Residuals help us calculate the
# intra-class correlation coefficient: how much clustering could exist 
# within data.
# Fixed effects: p value is 0 (significant). We have an intercept (Value),
# which is 0.7165711, significantly different from 0: can reject the null. 
# Number of Groups: 33 is the no of participants
# Number of Observations: 56 is the total times task was completed 

# IMPORTANT NOTES ABOUT THIS MODEL
# No predictors in the model (no independent variables)
# This is the null model (beginning of intercept only)
# Time is not being measured within this model
# Measuring the grand mean here

# STEP 5: Run intervals for the unconditional means model
intervals(mod1)
# Fixed effects est. is the same as Value from before (0.7165711)
# We also get lower and upper estimates. We know the confidence 
# interval is significant from the p-value being 0 above.
# Under random effects, it also has CIs. These suggest that individuals
# do vary. Important to make sure that lower value only is not below 0.
# Otherwise suggests the CI is not significant. So we can say that there
# is individual variability to be measured within our model

# STEP 6: Calculate intra-class correlation coefficient for the UMM
# ID
(0.02088573^2) / ((0.02088573^2) + (0.03927826^2)) # have to manually calculate
# We get 0.2204218. So 22% variability exists. So a bit of clustering is taking 
# place. We need to flush it out by selecting a proper slope and then by using
# our predictors to see how much of that variability we can account for.
# If this value were <.05, would suggest no clustering is happening and MLM
# would be less appropriate.

# So here we're not testing any hypothesis at this point. Just looking to see
# how much the interval differs from 0. We can use the I-CCC to see if we should
# proceed with multi-level modelling. If the value is 