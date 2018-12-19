# clear workspace
rm(list=ls())

setwd("~/research/visual_verification/Polish_English_Math")

library(plyr)
library(ggplot2)

# read in just the SACCADE data exported from EyeLink Data Viewer
dat  <- read.csv("PL_saccades2.csv")


############################################# Mean amplitude of saccades = Mean length of all saccades


#add column with mean amplitude for each participant and each trial
#(in degrees of visual angle)
dat <- ddply(dat, c("subject", "TRIAL_INDEX"), transform, mean_sac_ampli = mean(CURRENT_SAC_AMPLITUDE, na.rm=TRUE))
#note that there are NAs
#x <- c(1,2,NA,3)
#mean(x) # returns NA
#mean(x, na.rm=TRUE) # returns 2 


dat <- rename(dat, c(visual = "item"))

# write.csv(dat, file = "PL_saccades3.csv")
# dat  <- read.csv("PL_saccades3.csv")

str(dat)

dat$PolarityCode = ifelse(dat$polarity == "Positive", .5, -.5)
dat$CongruenceCode = ifelse(dat$congruence == "Incongruent", -.5, .5)

dat1 <- unique(dat[,c("subject", "TRIAL_INDEX", "TRIAL_SACCADE_TOTAL", "RESPONSE_TIME.1.", "RESPONSE_ACCURACY.1.", 
                       "item", "pic_category", "sentence", "sentence_type", "block", "button_expected", 
                       "polarity", "language", "congruence", "Imagewinner", "Ratio", "PxSize", 
                       "mean_sac_ampli", "PolarityCode", "CongruenceCode")])
str(dat1)

nrow(dat1)
#[1] 2017
row.has.na <- apply(dat1, 1, function(x){any(is.na(x))})
sum(row.has.na)
#[1] 4   (in dat there are 44)

dat14 <-  dat1[complete.cases(dat1),]
nrow(dat14)
#[1] 2013

# write.csv(dat14, file = "PL_saccades4.csv")
dat1 <- dat14
# dat1  <- read.csv("PL_saccades4.csv")


#PLOT mean_sac_ampli
library(ggplot2) 
source(file = "Helper_functions-within-subjects-se.R")

#Polish - Polarity and Congruence
PL0 <- subset(dat1, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="mean_sac_ampli", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=mean_sac_ampli, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean_sac_ampli-se, ymax=mean_sac_ampli+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white") #+
  #expand_limits(y= c(0, 18))                        # Expand y range
  
#Math - Polarity and Congruence
PL0 <- subset(dat1, language=="Math")
datac <- summarySEwithin(PL0, measurevar="mean_sac_ampli", withinvars=c("polarity","congruence"), idvar="subject")


#****************SIGNIFICANCE TESTS
#plots already show NO EFFECTS on mean saccade amplitude



########################################################  mean LOG saccade duration

#add column with mean LOG saccade duration for each participant and each trial
str(dat)
#
dat5 <- ddply(dat, c("subject", "TRIAL_INDEX"), transform, mean_log_sac_dur = mean(logCURRENT_SAC_DURATION, na.rm=TRUE))
#note that there are NAs
#x <- c(1,2,NA,3)
#mean(x) # returns NA
#mean(x, na.rm=TRUE) # returns 2 


dat5 <- rename(dat5, c(visual = "item"))

dat5$PolarityCode = ifelse(dat5$polarity == "Positive", .5, -.5)
dat5$CongruenceCode = ifelse(dat5$congruence == "Incongruent", -.5, .5)
str(dat5)

dat6 <- unique(dat5[,c("subject", "TRIAL_INDEX", "TRIAL_SACCADE_TOTAL", "RESPONSE_TIME.1.", "RESPONSE_ACCURACY.1.", 
                       "item", "pic_category", "sentence", "sentence_type", "block", "button_expected", 
                       "polarity", "language", "congruence", "Imagewinner", "Ratio", "PxSize", 
                       "mean_sac_ampli", "mean_log_sac_dur", "PolarityCode", "CongruenceCode")])
str(dat6)

#there are still 4 rows with NAs! (can see in the exported csv)
dat66 <-  dat6[complete.cases(dat6),]

# write.csv(dat66, file = "PL_saccades6.csv")
dat6 <- dat66
# dat6  <- read.csv("PL_saccades6.csv")


#PLOT mean_log_sac_dur


#Polish - Polarity and Congruence
PL0 <- subset(dat6, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="mean_log_sac_dur", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=mean_log_sac_dur, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean_log_sac_dur-se, ymax=mean_log_sac_dur+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white") #+
#expand_limits(y= c(0, 18))                        # Expand y range

#Math - Polarity and Congruence
PL0 <- subset(dat6, language=="Math")
datac <- summarySEwithin(PL0, measurevar="mean_log_sac_dur", withinvars=c("polarity","congruence"), idvar="subject")


#****************SIGNIFICANCE TESTS
#plots already show NO EFFECTS on mean saccade duration




############################################################  total number of saccades

str(dat6)

#PLOT TRIAL_SACCADE_TOTAL

#Polish - Polarity and Congruence
PL0 <- subset(dat6, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="TRIAL_SACCADE_TOTAL", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=TRIAL_SACCADE_TOTAL, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=TRIAL_SACCADE_TOTAL-se, ymax=TRIAL_SACCADE_TOTAL+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
PL0 <- subset(dat6, language=="Math")
datac <- summarySEwithin(PL0, measurevar="TRIAL_SACCADE_TOTAL", withinvars=c("polarity","congruence"), idvar="subject")



#****************SIGNIFICANCE TESTS
#is there a significant difference for MATH???

#****************check distribution of residuals
#Bayen book: 
library("latticeExtra", lib.loc="~/R/win-library/3.2")
qqmath(~TRIAL_SACCADE_TOTAL|subject, data=dat6) #no tails

#excluding incorrect responses
#dat6 <- droplevels(subset(dat6, RESPONSE_ACCURACY.1. != "incorrect"))

dat_PL <- subset(dat6, language=="Polish")
dat_M <- subset(dat6, language=="Math")

############### PL
dat_reduced <- dat_PL[,c("TRIAL_SACCADE_TOTAL", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(TRIAL_SACCADE_TOTAL ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1+PolarityCode*CongruenceCode||item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#(Intercept)                   3.9107     0.3074  38.0000  12.722 2.66e-15 ***
#  PolarityCode                 -0.1638     0.1107 937.9000  -1.480 0.139132    
#  CongruenceCode               -0.4288     0.1107 937.2000  -3.874 0.000115 ***
#  PolarityCode:CongruenceCode  -0.1977     0.5202  18.9000  -0.380 0.708113

#same when incorrects excluded

############### Math
dat_reduced <- dat_M[,c("TRIAL_SACCADE_TOTAL", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(TRIAL_SACCADE_TOTAL ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                   4.0132     0.3390 39.6900  11.840 1.33e-14 ***
#  PolarityCode                 -0.4278     0.1295 30.6600  -3.302 0.002443 ** 
#  CongruenceCode               -0.5255     0.1248 29.0700  -4.211 0.000224 ***
#  PolarityCode:CongruenceCode   0.1207     0.6059 19.5300   0.199 0.844223

#same when incorrects excluded




######################################################### total number of interROI saccades

str(dat)
#add a column for interROI saccades present
dat4 <- transform(dat, InterROI_sac = ifelse(CURRENT_SAC_START_INTEREST_AREA_INDEX==CURRENT_SAC_END_INTEREST_AREA_INDEX,
                                               0, 1))

#add a column for number of interROI saccades
dat4 <- ddply(dat4, c("subject", "TRIAL_INDEX"), transform, total_InterROI_sac = sum(InterROI_sac))

dat4 <- rename(dat4, c(visual = "item"))

dat4$PolarityCode = ifelse(dat4$polarity == "Positive", .5, -.5)
dat4$CongruenceCode = ifelse(dat4$congruence == "Incongruent", -.5, .5)
str(dat4)

dat5 <- unique(dat4[,c("subject", "TRIAL_INDEX", "TRIAL_SACCADE_TOTAL", "RESPONSE_TIME.1.", "RESPONSE_ACCURACY.1.", 
                       "item", "pic_category", "sentence", "sentence_type", "block", "button_expected", 
                       "polarity", "language", "congruence", "Imagewinner", "Ratio", "PxSize", 
                       "mean_sac_ampli", "total_InterROI_sac", "PolarityCode", "CongruenceCode")])
str(dat5)
#there are NAs
nrow(dat5)
#[1] 2017
row.has.na <- apply(dat5, 1, function(x){any(is.na(x))})
sum(row.has.na)
#[1] 38
dat6 <-  dat5[complete.cases(dat5),]
nrow(dat6)
#[1] 1979 (38 removed)

str(dat6)
# write.csv(dat6, file = "PL_saccades36.csv")



#PLOT total_InterROI_sac

#Polish - Polarity and Congruence
PL0 <- subset(dat6, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="total_InterROI_sac", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=total_InterROI_sac, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=total_InterROI_sac-se, ymax=total_InterROI_sac+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
PL0 <- subset(dat6, language=="Math")
datac <- summarySEwithin(PL0, measurevar="total_InterROI_sac", withinvars=c("polarity","congruence"), idvar="subject")



#****************SIGNIFICANCE TESTS


#****************check distribution of residuals
qqmath(~total_InterROI_sac|subject, data=dat6) #dunno

#excluding incorrect responses
#dat6 <- droplevels(subset(dat6, RESPONSE_ACCURACY.1. != "incorrect"))

dat_PL <- subset(dat6, language=="Polish")
dat_M <- subset(dat6, language=="Math")

############### PL
dat_reduced <- dat_PL[,c("total_InterROI_sac", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(total_InterROI_sac ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                   2.2055     0.1688  41.5000  13.069 2.22e-16 ***
#  PolarityCode                 -0.1446     0.0694 945.9000  -2.083  0.03752 *  
#  CongruenceCode               -0.2232     0.0694 945.9000  -3.217  0.00134 ** 
#  PolarityCode:CongruenceCode  -0.1548     0.3671  19.4000  -0.422  0.67791

#BUT when incorrects excluded:
#(Intercept)                   2.17402    0.16718  40.80000  13.004 4.44e-16 ***
#  PolarityCode                 -0.11193    0.06918 892.70000  -1.618  0.10603    
#  CongruenceCode               -0.22552    0.06921 891.40000  -3.259  0.00116 ** 
#  PolarityCode:CongruenceCode  -0.15096    0.35723  19.90000  -0.423  0.67713

############### Math
dat_reduced <- dat_M[,c("total_InterROI_sac", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(total_InterROI_sac ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                   2.171667   0.192787  39.600000  11.265 6.39e-14 ***
#  PolarityCode                 -0.210252   0.072978  29.800000  -2.881  0.00728 ** 
#  CongruenceCode               -0.183809   0.070813 896.600000  -2.596  0.00959 ** 
#  PolarityCode:CongruenceCode   0.001759   0.378782  20.200000   0.005  0.99634

#same when incorrects excluded







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  subsetting dat for blocks where Math or Polish first
str(dat)
#nrow(dat)
#[1] 8582

dat2 <- subset(dat, language == "Math" & TRIAL_INDEX < 41)
#nrow(dat2)
#[1] 2561
dat1 <- subset(dat, language == "Polish" & TRIAL_INDEX < 41)
#nrow(dat1)
#[1] 2152

#dat22 <- subset(dat, language == "Math" & TRIAL_INDEX > 40)
#dat12 <- subset(dat, language == "Polish" & TRIAL_INDEX > 40)
#nrow(dat2) + nrow(dat1) + nrow(dat22) + nrow(dat12)
#[1] 8582

