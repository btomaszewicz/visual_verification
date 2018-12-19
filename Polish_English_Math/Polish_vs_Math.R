# clear workspace
rm(list=ls())

setwd("~/research/visual_verification/Polish_English_Math")

library(plyr)
library(ggplot2)

ddir <- "~/research/visual_verification/Polish_English_Math/PL_Behavioral_Data"
fls <- list.files(ddir, "\\.txt$")

PL <- ldply(fls,
              function(filename, dir=ddir){
                dtx <- read.table(file.path(dir, filename))
                data.frame(file = filename, dtx)
              })
head(PL)

names(PL) <- c("subject",
                 "button_pressed",
                 "button_press_time",
                 "display_on_time",
                 "rt",
                 "response_accuracy",
                 "pic_file",
                 "pic_category",
                 "correct_judgment",
                 "sentence",
                 "sentence_type",
                 "block",
                 "button_expected"
)

head(PL)


# remove the rows with column names in each txt file - should be 31
str(PL)
nrow(PL)
#[1] 2511
PL2 <- droplevels(subset(PL, button_pressed != "BUTTON_PRESSED[1]"))
nrow(PL)-nrow(PL2)
#[1] 31

# Add a column indicating the position in the experiment - trial.
PL2 <- ddply(PL2, .(subject), function(dtx){
  dtx$trial <- 1:nrow(dtx)
  dtx
})
head(PL2)

# str(PL2)
# write.csv(PL2, file = "PL2.csv")
# PL2  <- read.csv("PL2.csv")


#remove the trials where no response
PL3 <- droplevels(subset(PL2, response_accuracy != "no_response"))
nrow(PL2)-nrow(PL3)
#[1] 14
1-nrow(PL3)/nrow(PL2) 
#[1] 0.005645161

#rt is a factor, should be a number
PL3$rt <- as.numeric(as.character(PL3$rt))

#add the logRT column (of RAW RTs)
PL3 <- transform(PL3, logRT = log(rt))



#********* REMOVE EXTREME OUTLIERS ==> no button presses <200ms 
 
# remove extreme outliers in RAW RTs
ggplot(PL3, aes(button_pressed, rt))+
  geom_jitter(alpha=1/10)+
  scale_y_log10()+
  geom_hline(yintercept=(possible.cutoff <- c(200)),col="darkgray")

PL4 <- subset(PL3, rt>min(possible.cutoff))
1-nrow(PL4)/nrow(PL3) 
#[1] 0
# no button presses <200ms 

min(PL3$rt)
#[1] 352


#**************add condition names
str(PL3)

# polarity
levels(PL3$sentence_type)
#[1] "1" "2" (2 is negative)
# create a new factor variable, polarity (now the same order as in EN - negative left in plots)
PL3 <- transform(PL3, polarity = ifelse(sentence_type=="1", "Positive", "Negative"))
str(PL3)


# add a column for language
levels(PL3$sentence)
PL3 <- transform(PL3, language = ifelse(sentence=="A < B" 
                                        | sentence=="A > B"
                                        | sentence=="B < A"
                                        | sentence=="B > A", "Math", "Polish"))

# add column for congruence
levels(PL3$correct_judgment)
#1 is congruent, 0 icongr.
PL3 <- transform(PL3, congruence = ifelse(correct_judgment==0, "Incongruent", "Congruent"))
                                        

# write.csv(PL3, file = "PL3_logRTs.csv")


##**************************** LogRTs ********************************

PL3  <- read.csv("PL3_logRTs.csv")
str(PL3)


source(file = "Helper_functions-within-subjects-se.R")

dat_expt1_logRT_pol <- 
  summarySEwithin(PL3, measurevar="logRT", 
                  withinvars=c("polarity","congruence","language"), idvar="subject")

levels(dat_expt1_logRT_pol$language)
dat_expt1_logRT_pol$language <- factor(dat_expt1_logRT_pol$language,
                       levels = c("Polish", "Math"))

#2-panel GRAPH
ggplot(dat_expt1_logRT_pol, aes(congruence, logRT, group=polarity, shape=polarity, color=polarity)) +
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=polarity), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ language) +
  xlab("Congruence") + ylab("Reaction time (log ms)")
#if wrong order of panels - change order of factors

##########################
#Check logRT for 2 subsets of participants

#***** set numerical coding for factors
PL3$PolarityCode = ifelse(PL3$polarity == "Positive", .5, -.5)
PL3$CongruenceCode = ifelse(PL3$congruence == "Incongruent", -.5, .5)
PL3$LanguageCode = ifelse(PL3$language == "Math", -.5, .5)

#***add item column
PL3$item <- PL3$pic_file
PL3$item <- gsub(".png", "", PL3$item)
PL3$item <- as.factor(PL3$item)
PL3$item <- gsub("fig2_", "", PL3$item)
PL3$item <- as.factor(PL3$item)

levels(PL3$item)

#MATH FIRST PEOPLE
dat361 <- subset(PL3, language == "Math" & trial < 41)
dat362 <- subset(PL3, language == "Polish" & trial > 41)
datM1st <- rbind(dat361, dat362)
str(PL3)
str(datM1st)

#POLISH FIRST
dat363 <- subset(PL3, language == "Polish" & trial < 41)
dat364 <- subset(PL3, language == "Math" & trial > 41)
datPL1st <- rbind(dat363, dat364)
str(datPL1st)


#MATH FIRST PEOPLE
dat_expt1_logRT_pol <- 
  summarySEwithin(datM1st, measurevar="logRT", 
                  withinvars=c("polarity","congruence","language"), idvar="subject")

#Polish FIRST PEOPLE
dat_expt1_logRT_pol <- 
  summarySEwithin(datPL1st, measurevar="logRT", 
                  withinvars=c("polarity","congruence","language"), idvar="subject")


#2-panel GRAPH
ggplot(dat_expt1_logRT_pol, aes(congruence, logRT, group=polarity, shape=polarity, color=polarity)) +
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=polarity), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ language) 


#SEPARATE GRAPHS

#POLISH FIRST PEOPLE
PL0 <- subset(dat363, language=="Polish")
#POLISH SECOND PEOPLE
PL0 <- subset(dat362, language=="Polish")

datac <- summarySEwithin(PL0, measurevar="logRT", withinvars=c("polarity","congruence"), idvar="subject")

ggplot(datac, aes(x=congruence, y=logRT, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=logRT-se, ymax=logRT+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence

#MATH FIRST PEOPLE
PL0 <- subset(dat361, language=="Math")
#MATH SECOND PEOPLE
PL0 <- subset(dat364, language=="Math")

datac <- summarySEwithin(PL0, measurevar="logRT", withinvars=c("polarity","congruence"), idvar="subject")
#same graph code as above



#****************SIGNIFICANCE TESTS
library(languageR)
library(lmerTest)
library(lme4)
#check version in Packages pane, now 1.1-10

# check distribution of residuals
# Bayen book: 
library("lattice", lib.loc="C:/R/R-3.2.1/library")
qqmath(~rt|subject, data=PL3) #thick right tails with some participants
qqmath(~logRT|subject, data=PL3) #lines much more straight
qqmath(~logRT_trim|subject, data=PL4) #very similar to above (trimming within subjects)

############### PL in PL 1st people
dat_reduced <- dat363[,c("logRT", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(logRT ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                   7.14820    0.06610  28.00000 108.139  < 2e-16 ***
#  PolarityCode                 -0.05729    0.02868  19.00000  -1.998   0.0603 .  
#CongruenceCode               -0.12176    0.02445 527.90000  -4.979 8.67e-07 ***
#  PolarityCode:CongruenceCode  -0.02264    0.10126  18.80000  -0.224   0.8255
format(8.67e-07, scientific=FALSE)


############### PL in Math 1st people
dat_reduced <- dat362[,c("logRT", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# non-maximal
me <- lmer(logRT ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                   7.06328    0.05797  29.00000 121.850   <2e-16 ***
#  PolarityCode                 -0.05482    0.02406 533.20000  -2.278   0.0231 *  
#  CongruenceCode               -0.11245    0.03291  24.70000  -3.417   0.0022 ** 
#  PolarityCode:CongruenceCode  -0.13196    0.09879  23.80000  -1.336   0.1943


############### Math in Math 1st people
dat_reduced <- dat361[,c("logRT", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(logRT ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                   7.21739    0.05759  28.40000 125.332  < 2e-16 ***
#  PolarityCode                 -0.10226    0.02535 565.40000  -4.034 6.25e-05 ***
#  CongruenceCode               -0.15712    0.02660  15.90000  -5.908 2.24e-05 ***
#  PolarityCode:CongruenceCode  -0.04769    0.08343  19.10000  -0.572    0.574
format(6.25e-05, scientific=FALSE)
format(2.24e-05, scientific=FALSE)


############### Math in Polish 1st people
dat_reduced <- dat364[,c("logRT", "PolarityCode", "CongruenceCode", "subject", "item")]
str(dat_reduced)

# run the model with maximal random effects structure
me <- lmer(logRT ~ PolarityCode * CongruenceCode + 
             (1+PolarityCode*CongruenceCode||subject) + (1|item), 
           data = dat_reduced, REML=FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
summary(me)
#Fixed effects:
#(Intercept)                  7.07067    0.05712 28.18600 123.778  < 2e-16 ***
#  PolarityCode                -0.08502    0.03623 25.89300  -2.346 0.026896 *  
#  CongruenceCode              -0.18646    0.04030 20.60600  -4.627 0.000152 ***
#  PolarityCode:CongruenceCode -0.06340    0.09890 18.46000  -0.641 0.529370





##**************************** Accuracy ********************************

str(PL3)
levels(PL3$response_accuracy)
#get PERCENT CORRECT for each participant
#[1] "correct"   "incorrect"
PL3 <- transform(PL3, response_accuracy1 = ifelse(response_accuracy=="correct", 1, 0))


#Accuracy for each participant across all conditions
per_cor <- ddply(PL3, .(subject), summarise,
                 N    = length(response_accuracy1),
                 mean = mean(response_accuracy1),
                 sd   = sd(response_accuracy1),
                 se   = sd / sqrt(N)
)
per_cor

min(per_cor$mean)
#[1] 0.9375
#lowest 94% 
mean(per_cor$mean)
#0.9707873

#Accuracy per participant per condition
per_cor_cond <- ddply(PL3, .(subject, language, polarity, congruence), summarise,
                      N    = length(response_accuracy1),
                      mean = mean(response_accuracy1),
                      sd   = sd(response_accuracy1),
                      se   = sd / sqrt(N)
)
head(per_cor_cond)

#library(ggplot2) 
#read in Helper functions WITHIN SUBJECTS SE.R

str(per_cor_cond)

#Polish - Polarity and Congruence
per_cor0 <- subset(per_cor_cond, language=="Polish")

datac <- summarySEwithin(per_cor0, measurevar="mean", withinvars=c("congruence","polarity"), idvar="subject")

=ggplot(datac, aes(x=congruence, y=mean, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


#Math - Polarity and Congruence
per_cor0 <- subset(per_cor_cond, language=="Math")

datac <- summarySEwithin(per_cor0, measurevar="mean", withinvars=c("congruence","polarity"), idvar="subject")

ggplot(datac, aes(x=congruence, y=mean, group=polarity, colour=polarity)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=.2) +
  geom_line(aes(linetype=polarity), size=1.2) +
  geom_point(size=4, shape=21, fill="white")




#Congruent - Polarity and Language
per_cor0 <- subset(per_cor_cond, congruence=="Congruent")

datac <- summarySEwithin(per_cor0, measurevar="mean", withinvars=c("polarity","language"), idvar="subject")

ggplot(datac, aes(x=polarity, y=mean, group=language, colour=language)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=.2) +
  geom_line(aes(linetype=language), size=1.2) +
  geom_point(size=4, shape=21, fill="white")

#Incongruent - Polarity and Language
per_cor0 <- subset(per_cor_cond, congruence=="Incongruent")

datac <- summarySEwithin(per_cor0, measurevar="mean", withinvars=c("polarity","language"), idvar="subject")

ggplot(datac, aes(x=polarity, y=mean, group=language, colour=language)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=1, width=.2) +
  geom_line(aes(linetype=language), size=1.2) +
  geom_point(size=4, shape=21, fill="white")


