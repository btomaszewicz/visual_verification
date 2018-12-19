# clear workspace
rm(list=ls())

setwd("~/research/visual_verification/Polish_English_Math")

library(plyr)
library(ggplot2)

# read in just the FIXATION data exported from EyeLink Data Viewer
dat  <- read.csv("PL_fixation2.csv")


############################################## MEAN FIXATION DURATION

#add 2 columns with mean fixation duration for each participant and each trial
#log
dat <- ddply(dat, c("RECORDING_SESSION_LABEL", "TRIAL_INDEX"), transform, mean_log_fix_dur = mean(logCURRENT_FIX_DURATION))
#raw
dat <- ddply(dat, c("RECORDING_SESSION_LABEL", "TRIAL_INDEX"), transform, mean_raw_fix_dur = mean(CURRENT_FIX_DURATION))

#dat <- rename(dat, c(RECORDING_SESSION_LABEL = "subject"))
dat <- rename(dat, c(visual = "item"))


# write.csv(dat, file = "PL_fixation3.csv")
# dat  <- read.csv("PL_fixation3.csv")

str(dat)

dat$PolarityCode = ifelse(dat$polarity == "Positive", .5, -.5)
dat$CongruenceCode = ifelse(dat$congruence == "Incongruent", -.5, .5)


dat <- unique(dat[,c("subject", "RESPONSE_ACCURACY.1.", "RESPONSE_TIME.1.", "TRIAL_FIXATION_TOTAL",
                      "TRIAL_INDEX", "block", "button_expected", "correct", "pic_category", 
                      "sentence", "sentence_type", "item", "polarity", "language", "congruence",
                      "Imagewinner", "Ratio", "PxSize", "mean_log_fix_dur", "mean_raw_fix_dur", 
                      "PolarityCode", "CongruenceCode")])
str(dat)

# write.csv(dat, file = "PL_fixation4.csv")
# dat  <- read.csv("PL_fixation4.csv")


#PLOT mean_log_fix_dur
library(ggplot2) 
source(file = "Helper_functions-within-subjects-se.R")

#Polish - Polarity and Congruence
PL0 <- subset(dat, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="mean_log_fix_dur", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=mean_log_fix_dur, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean_log_fix_dur-se, ymax=mean_log_fix_dur+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
PL0 <- subset(dat, language=="Math")
datac <- summarySEwithin(PL0, measurevar="mean_log_fix_dur", withinvars=c("polarity","congruence"), idvar="subject")
#same graph code as above

#PLOT mean_raw_fix_dur
PL0 <- subset(dat, language=="Polish")
PL0 <- subset(dat, language=="Math")
datac <- summarySEwithin(PL0, measurevar="mean_raw_fix_dur", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=mean_raw_fix_dur, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean_raw_fix_dur-se, ymax=mean_raw_fix_dur+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


# =========> plots already show NO EFFECTS on mean fixation duration

#****************SIGNIFICANCE TESTS
library(languageR)
library(lmerTest)
library(lme4)

#****************check distribution of residuals
#Bayen book: 
library("latticeExtra", lib.loc="~/R/win-library/3.2")
qqmath(~mean_raw_fix_dur|subject, data=dat) #thick right tails with some participants
qqmath(~mean_log_fix_dur|subject, data=dat) #lines much more straight

str(dat)
#contrasts
dat$PolarityCode = ifelse(dat$polarity == "Positive", .5, -.5)
dat$CongruenceCode = ifelse(dat$congruence == "Incongruent", -.5, .5)

#excluding incorrect responses
#dat <- droplevels(subset(dat, RESPONSE_ACCURACY.1. != "incorrect"))

dat_PL <- subset(dat, language=="Polish")
dat_M <- subset(dat, language=="Math")


############### PL
dat_reduced <- dat_PL[,c("mean_log_fix_dur", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(mean_log_fix_dur ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                  5.715e+00  9.997e-02  3.270e+01  57.169   <2e-16 ***
#  PolarityCode                -7.606e-04  2.434e-02  1.180e+03  -0.031    0.975    
#  CongruenceCode              -1.579e-02  2.435e-02  1.180e+03  -0.649    0.517    
#  PolarityCode:CongruenceCode -2.302e-02  8.199e-02  1.910e+01  -0.281    0.782

#no effects also when incorrect responses excluded

############### Math
dat_reduced <- dat_M[,c("mean_log_fix_dur", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(mean_log_fix_dur ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                  5.753e+00  9.811e-02  3.360e+01  58.632   <2e-16 ***
#  PolarityCode                -2.561e-03  2.557e-02  1.125e+03  -0.100    0.920    
#  CongruenceCode              -3.238e-02  2.606e-02  3.120e+01  -1.243    0.223    
#  PolarityCode:CongruenceCode -1.060e-02  1.004e-01  2.120e+01  -0.106    0.917

#no effects also when incorrect responses excluded







################################################## TOTAL GAZE DURATION (Sum of fixation durations)

#add 2 columns with mean fixation duration for each participant and each trial
#log
dat <- ddply(dat, c("subject", "TRIAL_INDEX"), transform, total_log_fix_dur = sum(logCURRENT_FIX_DURATION))
#raw
dat <- ddply(dat, c("subject", "TRIAL_INDEX"), transform, total_raw_fix_dur = sum(CURRENT_FIX_DURATION))


# write.csv(dat, file = "PL_fixation5.csv")
# dat  <- read.csv("PL_fixation5.csv")

str(dat)
dat <- unique(dat[,c("subject", "RESPONSE_ACCURACY.1.", "RESPONSE_TIME.1.", "TRIAL_FIXATION_TOTAL",
                       "TRIAL_INDEX", "block", "button_expected", "correct", "pic_category", 
                       "sentence", "sentence_type", "item", "polarity", "language", "congruence",
                       "Imagewinner", "Ratio", "PxSize", "mean_log_fix_dur", "mean_raw_fix_dur", 
                       "PolarityCode", "CongruenceCode", "total_log_fix_dur", "total_raw_fix_dur")])
str(dat)

# write.csv(dat, file = "PL_fixation6.csv")
# dat  <- read.csv("PL_fixation6.csv")


#PLOT total_log_fix_dur

#Polish - Polarity and Congruence
PL0 <- subset(dat, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="total_log_fix_dur", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=total_log_fix_dur, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=total_log_fix_dur-se, ymax=total_log_fix_dur+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
PL0 <- subset(dat, language=="Math")
datac <- summarySEwithin(PL0, measurevar="total_log_fix_dur", withinvars=c("polarity","congruence"), idvar="subject")



#****************SIGNIFICANCE TESTS
#these should correlate with RTs


#****************check distribution of residuals
qqmath(~total_log_fix_dur|subject, data=dat) #dunno


#contrasts
dat$PolarityCode = ifelse(dat$polarity == "Positive", .5, -.5)
dat$CongruenceCode = ifelse(dat$congruence == "Incongruent", -.5, .5)

#excluding incorrect responses
#dat <- droplevels(subset(dat, RESPONSE_ACCURACY.1. != "incorrect"))

dat_PL <- subset(dat, language=="Polish")
dat_M <- subset(dat, language=="Math")


############### PL
dat_reduced <- dat_PL[,c("total_log_fix_dur", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(total_log_fix_dur ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject)+ (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)                   23.4853     1.8558   37.5000  12.655    4e-15 ***
#  PolarityCode                  -0.8812     0.4970 1148.9000  -1.773 0.076525 .  
#  CongruenceCode                -2.2419     0.5130   30.6000  -4.370 0.000132 ***
#  PolarityCode:CongruenceCode   -1.3369     2.6131   19.2000  -0.512 0.614766

#same with incorrects excluded

############### Math
dat_reduced <- dat_M[,c("total_log_fix_dur", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(total_log_fix_dur ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1+PolarityCode*CongruenceCode||item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                 24.23615    2.00367 38.53000  12.096 1.09e-14 ***
#  PolarityCode                -1.91308    0.60952 31.06000  -3.139   0.0037 ** 
#  CongruenceCode              -2.58866    0.55287 30.46000  -4.682 5.53e-05 ***
#  PolarityCode:CongruenceCode -0.03977    3.01005 19.24000  -0.013   0.9896    
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
format(5.53e-05, scientific=FALSE)
#0.0000553
#same with incorrects excluded


############################################### NUMBER OF FIXATIONS

str(dat)

#PLOT TRIAL_FIXATION_TOTAL

#Polish - Polarity and Congruence
PL0 <- subset(dat, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="TRIAL_FIXATION_TOTAL", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=TRIAL_FIXATION_TOTAL, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=TRIAL_FIXATION_TOTAL-se, ymax=TRIAL_FIXATION_TOTAL+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
PL0 <- subset(dat, language=="Math")
datac <- summarySEwithin(PL0, measurevar="TRIAL_FIXATION_TOTAL", withinvars=c("polarity","congruence"), idvar="subject")



#****************SIGNIFICANCE TESTS
#is there a significant difference for MATH???


#****************check distribution of residuals
qqmath(~TRIAL_FIXATION_TOTAL|subject, data=dat) #dunno

#excluding incorrect responses
#dat <- droplevels(subset(dat, RESPONSE_ACCURACY.1. != "incorrect"))

dat_PL <- subset(dat, language=="Polish")
dat_M <- subset(dat, language=="Math")

############### PL
dat_reduced <- dat_PL[,c("TRIAL_FIXATION_TOTAL", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(TRIAL_FIXATION_TOTAL ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#(Intercept)                    4.32655    0.37014   37.40000  11.689 4.80e-14 ***
#  PolarityCode                  -0.16205    0.09625 1179.90000  -1.684   0.0925 .  
#  CongruenceCode                -0.39857    0.09626 1180.00000  -4.141 3.71e-05 ***
#  PolarityCode:CongruenceCode   -0.21294    0.51288   19.20000  -0.415   0.6826
format(3.71e-05, scientific=FALSE)
# 0.0000371

#same when incorrects excluded

############### Math
dat_reduced <- dat_M[,c("TRIAL_FIXATION_TOTAL", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(TRIAL_FIXATION_TOTAL ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1+PolarityCode*CongruenceCode||item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                    4.44699    0.39759   38.90000  11.185 1.03e-13 ***
#  PolarityCode                  -0.34500    0.11309   31.10000  -3.051  0.00464 ** 
#  CongruenceCode                -0.45249    0.10681 1125.20000  -4.236 2.46e-05 ***
#  PolarityCode:CongruenceCode    0.02353    0.60658   19.00000   0.039  0.96946
format(2.46e-05, scientific=FALSE)
#0.0000246
#same when incorrects excluded
