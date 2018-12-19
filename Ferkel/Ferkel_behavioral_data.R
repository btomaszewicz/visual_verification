#clear workspace
rm(list=ls())

setwd("~/research/visual_verification/Ferkel")

library(plyr)
library(ggplot2)


######## read in the data ####

# 6 conditions, 2*2*2 (ParticleCode:FocCode:CongruenceCode)
# 36 participants
# 24 items, Latin square 4 lists
# per list 96 trials (and 6 practice) = a single participants saw:
# 48 fillers (24 yes fillers, 24 no fillers)
# 48 items:
# 12 most object (6 yes most object, 6 no most object)
# 12 most subject (6 yes most subject, 6 no most subject)
# 12 only object (6 yes only object, 6 no only object)
# 12 only subject (6 yes only subject, 6 no only subject)

#v1p1 <- read.table("./Ferkel_PL_results/v1p1/RESULTS_FILE.txt", header = T) -- single file
#v3p1 <- read.table("./Ferkel_PL_behavioral/RESULTS_FILE_v3p1.txt", header = T)

ddir <- "~/research/visual_verification/Ferkel/Ferkel_PL_behavioral"
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
                 "correct_judgment",
                 "button_expected",
                 "pic_file",
                 "particle"
)
head(PL)

#remove the rows with column names in each txt file - should be 34 b/c participant is a Factor w/ 34 levels
str(PL)
nrow(PL)
#[1] 3502
PL2 <- droplevels(subset(PL, button_pressed != "BUTTON_PRESSED"))
nrow(PL)-nrow(PL2)
#[1] 34

#Add a column indicating the position in the experiment - trial.
PL2 <- ddply(PL2, .(subject), function(dtx){
  dtx$trial <- 1:nrow(dtx)
  dtx
})
head(PL2)
PL2$trial <- as.factor(PL2$trial) 
str(PL2) #yes, 102 levels & 102 observations in a single file, e.g. v1p1



#read all SENTENCE files (because we can't recover 'most subject' and 'most object' condition on no-pics from just result files)
ddir2 <- "~/research/visual_verification/Ferkel_PL_sentences"
fls2 <- list.files(ddir2, "\\.dat$")
runtime <- ldply(fls2,
                 function(filename, dir=ddir2){
                   dtx <- read.table(file.path(dir, filename))
                   data.frame(file = filename, dtx)
                 })
head(runtime)
nrow(runtime)
#[1] 3468 (3468 + 34 = 3502 so the same as in all result files combined)
runtime$V6 <- NULL
runtime$V7 <- NULL
runtime$V8 <- NULL
runtime$V9 <- NULL
runtime$V10 <- NULL
runtime$V11 <- NULL
runtime$V12 <- NULL
runtime$V13 <- NULL
runtime$V14 <- NULL
runtime$V15 <- NULL
runtime$V16 <- NULL
runtime$V17 <- NULL
runtime$V18 <- NULL
runtime$V19 <- NULL
runtime$V20 <- NULL
runtime$V21 <- NULL
head(runtime)
str(runtime)
names(runtime) <- c("subject",
                    "version",
                    "pic_file",
                    "sentence",
                    "correct_judgment",
                    "button_expected"
)
#Add a column indicating the position in the experiment - trial.
runtime <- ddply(runtime, .(subject), function(dtx){
  dtx$trial <- 1:nrow(dtx)
  dtx
})
head(runtime)
runtime$trial <- as.factor(runtime$trial) 
str(runtime)

nrow(runtime)
nrow(PL2)

#subject column needs to be the same
runtime$subject <- gsub("actual_TRIAL_DataSource_Builder_Sounds_BLOCKTRIAL_", "", runtime$subject)
runtime$subject <- gsub(".dat", "", runtime$subject)
head(runtime)
runtime$subject <- as.factor(runtime$subject)
str(runtime)
runtime$version <- as.factor(runtime$version)
runtime$button_expected <- as.factor(runtime$button_expected)

PL2$subject <- gsub("RESULTS_FILE_", "", PL2$subject)
PL2$subject <- gsub(".txt", "", PL2$subject)
PL2$subject <- as.factor(PL2$subject)
str(PL2)

#levels(PL2$subject)
#levels(runtime$subject)


PL22 <- merge(PL2, runtime)
head(PL22)
str(PL22) #somehow now there 2 rows less!!! Both PL2 and runtime have 3468, but PL22 has 3466




#####preparing the data

#remove the trials where no response
PL3 <- droplevels(subset(PL22, response_accuracy != "no_response"))
nrow(PL22)-nrow(PL3)
#[1] 23
1-nrow(PL3)/nrow(PL22) 
#[1] 0.006632065
str(PL3)

#rt is a factor, should be a number
PL3$rt <- as.numeric(as.character(PL3$rt))

#add the logRT column (of RAW RTs)
PL3 <- transform(PL3, logRT = log(rt))



#********* REMOVE FAST / EXTREME OUTLIERS ==> no button presses <200ms (initiation of a sacade)
 
#remove extreme outliers in RAW RTs
ggplot(PL3, aes(button_pressed, rt))+
  geom_jitter(alpha=1/10)+
  scale_y_log10()+
  geom_hline(yintercept=(possible.cutoff <- c(200)),col="darkgray")

PL4 <- subset(PL3, rt>min(possible.cutoff))
1-nrow(PL4)/nrow(PL3) 
#[1] 0
#no button presses <200ms 

min(PL3$rt)
#[1] 388 (in ABlines: 352)


#**************add condition names
str(PL3)

#we have 'particle' (number estimation), we need 'focus type' and 'congruence' (from 'correct_judgment')

#where 'stot' in pic name - subject focus, yes-pics (same pic with both 'most' and 'only')
#where 'sfot' in pic name - object focus, yes-pics (same pic with both 'most' and 'only')
#where 'sfof' in pic name - subject 'most' and object 'most', no-pics ==> check in sentence for "animal."
#where 'subj-nichtnur' - subject 'only', no-pics
#where 'obj-nichtnur' - object 'only', no-pics
#==> for subject check in sentence for "animal.$"!!! # $ Asserts that we are at the end.

###focus_type => foc:

PL3$foc <- c(rep("-", nrow(PL3)))

for (i in 1:nrow(PL3)) {
  if (grepl("prosiaczek.$", PL3$sentence[i])) {    # $ Asserts that we are at the end.
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("osioł.$", PL3$sentence[i])) {
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("miś.$", PL3$sentence[i])) {
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("lew.$", PL3$sentence[i])) {
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("królik.$", PL3$sentence[i])) {
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("pies.$", PL3$sentence[i])) {
    PL3$foc[i] <- "subj"
  }
}

for (i in 1:nrow(PL3)) {
  if (grepl("-", PL3$foc[i])) {
    PL3$foc[i] <- "obj"
  }
}

PL3$foc <- as.factor(PL3$foc)

#congruence
levels(PL3$correct_judgment)
PL3 <- transform(PL3, congruence = ifelse(correct_judgment=="yes", "congruent", "incongruent"))
str(PL3)


# write.csv(PL3, file = "Ferkel_PL_behavioral.csv")
# PL3  <- read.csv("Ferkel_PL_behavioral.csv")

PL3$trial <- as.factor(PL3$trial)
PL3$version <- as.factor(PL3$version)
str(PL3)
#$ trial            : Factor w/ 102 levels: 96 trials (and 6 practice)

#sentence         : Factor w/ 246 levels "Balony złapał nie tylko królik.",..
# that's for both items and fillers, so 246-6practice=240
# after removing fillers:
#sentence         : Factor w/ 192 levels - there are 96 trials (and 6 practice) so 96*2=192


##**************************** PLOTTING ********************************

PL3  <- read.csv("Ferkel_PL_behavioral.csv")
levels(PL3$particle)
PL3 <- droplevels(subset(PL3, particle != "practice"))
PL3 <- droplevels(subset(PL3, particle != "filler"))

str(PL3)

source(file = "Helper_functions-within-subjects-se.R")

datac <- 
  summarySEwithin(PL3, measurevar="logRT", 
                  withinvars=c("particle","foc","congruence"), idvar="subject")


#2-panel GRAPH = congruent and incongruent separate panels
labels <- c(congruent = "YES-screens", incongruent = "NO-screens") #labels <- c(Female = "Women", Male = "Men") http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#modifying-facet-label-text

ggplot(datac, aes(foc, logRT, group=particle, shape=particle, color=particle)) +
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=particle), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ congruence, labeller=labeller(congruence = labels)) +
  xlab("Congruence") + ylab("Reaction time (log ms)")
####we see that 'most' no different on incongruent screens

#Plot RTs only for the 7 'successful' participants:
#[1] "v1p7" "v2p3" "v3p1" "v3p3" "v3p6" "v4p6" "v4p8"
PL3success <- subset(PL3, subject == "v1p7" | subject == "v2p3" | subject == "v3p1" | 
                                  subject == "v3p3" | subject == "v3p6" | subject == "v4p6" | 
                                  subject == "v4p8")
str(PL3success)
#$ subject          : Factor w/ 34 levels
PL3success <- droplevels(PL3success)

datac <- 
  summarySEwithin(PL3success, measurevar="logRT", 
                  withinvars=c("particle","foc","congruence"), idvar="subject")

####now we see that the successful people were SLOWER: with MostObject (8.05) and OnlyObject (8.0)
#while WHOLE group also had MostObject (8.05) BUT OnlyObject (7.95)


#PLOT Accuracy

str(PL3)
levels(PL3$response_accuracy)
#get PERCENT CORRECT for each participant
#[1] "correct"   "incorrect"
PL3 <- transform(PL3, response_accuracy1 = ifelse(response_accuracy=="correct", 1, 0))

#Accuracy for each participant across all conditions
correctness <- ddply(PL3, .(subject), summarise,
                 N    = length(response_accuracy1),
                 mean = mean(response_accuracy1),
                 sd   = sd(response_accuracy1),
                 se   = sd / sqrt(N)
)
head(correctness)

min(correctness$mean)
#[1] 0.75
mean(correctness$mean)
#0.8611506

#Accuracy condition means
correctness_mean <- ddply(PL3, .(particle, foc, congruence), summarise,
                          N    = length(response_accuracy1),
                          mean = mean(response_accuracy1),
                          sd   = sd(response_accuracy1),
                          se   = sd / sqrt(N)
)
correctness_mean


#Accuracy per participant per condition
correctness_cond <- ddply(PL3, .(subject, particle, foc, congruence), summarise,
                      N    = length(response_accuracy1),
                      mean = mean(response_accuracy1),
                      sd   = sd(response_accuracy1),
                      se   = sd / sqrt(N)
)
head(correctness_cond)

#the plot will show low accuaracy in obj-most=yes, so check participants:
correctness_obj_most_yes <- subset(correctness_cond, particle == "most" & foc == "obj" & congruence == "congruent")
head(correctness_obj_most_yes)
min(correctness_obj_most_yes$mean)
#[1] 0
mean(correctness_obj_most_yes$mean)
#0.3333333
sortedlook <- correctness_obj_most_yes[order(correctness_obj_most_yes$mean),]
sortedlook
#there are 7 participants above 83%, the next is at 66%

str(correctness_obj_most_yes)
failed_obj_most_yes <- droplevels(subset(correctness_obj_most_yes, mean == 0))
nrow(failed_obj_most_yes)
#[1] 14
chance_obj_most_yes <- droplevels(subset(correctness_obj_most_yes, mean > 0 & mean < .7))
nrow(chance_obj_most_yes)
#[1] 13
success_obj_most_yes <- droplevels(subset(correctness_obj_most_yes, mean > .7))
nrow(success_obj_most_yes)
#[1] 7
levels(success_obj_most_yes$subject)
#[1] "v1p7" "v2p3" "v3p1" "v3p3" "v3p6" "v4p6" "v4p8"
success_obj_most_yes
#look all trials of v2p3
v2p3 <- droplevels(subset(PL3, subject == "v2p3"))
# trial 93
# Najwięcej prosiaczek wygrał serduszek.
# trial 71
# Prosiaczek zjadł nie tylko ogórki.



str(correctness_cond)
datac <- 
  summarySEwithin(correctness_cond, measurevar="mean", 
                  withinvars=c("particle","foc","congruence"), idvar="subject")

#2-panel GRAPH = congruent and incongruent separate panels
ggplot(datac, aes(foc, mean, group=particle, shape=particle, color=particle)) +
  geom_errorbar(aes(ymin=(mean-se), ymax=(mean+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=particle), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ congruence, labeller=labeller(congruence = labels)) +
  xlab("Congruence") + ylab("Accuracy")

# #COLORS with 2-panel GRAPH = congruent and incongruent separate panels
# colors <- c(most = "blue", only = "orange")
# group.colors <- c(A = "#333BFF", B = "#CC6600", C ="#9633FF", D = "#E2FF33", E = "#E3DB71")
# 
# ggplot(datac, aes(foc, mean, group=particle, shape=particle, color=particle)) +
#   geom_errorbar(aes(ymin=(mean-se), ymax=(mean+se)), size=.75, width=0.2) + 
#   scale_colour_manual(values=colors)
#   geom_line(aes(linetype=particle), size=.75) + 
#   geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
#   theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
#   facet_grid(. ~ congruence, labeller=labeller(congruence = labels)) +
#   xlab("Congruence") + ylab("Accuracy")






#****************SIGNIFICANCE TESTS
library(languageR)
library(lmerTest)
library(lme4)

str(PL3)
PL3$trial <- as.factor(PL3$trial)
PL3$version <- as.factor(PL3$version)
#sentence         : Factor w/ 192 levels - there are 96 trials (and 6 practice) so 96*2=192

PL3$ParticleCode = ifelse(PL3$particle == "most", .5, -.5)
PL3$CongruenceCode = ifelse(PL3$congruence == "incongruent", -.5, .5)
PL3$FocCode = ifelse(PL3$foc == "obj", -.5, .5)


#all 34 participants
dat_reduced <- PL3[,c("logRT", "ParticleCode", "FocCode", "CongruenceCode", "subject", "sentence")]


me <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode*FocCode*CongruenceCode||subject) + 
             (1+ParticleCode*FocCode*CongruenceCode||sentence), 
           data = dat_reduced, REML=FALSE)
#Model failed to converge

me <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode||subject) + 
             (1+ParticleCode+FocCode+CongruenceCode||sentence), 
           data = dat_reduced, REML=FALSE)
#Model is nearly unidentifiable: large eigenvalue ratio

#somehow in Kliegl's examples the zero correlations models, || converge
#and then they get extended by using | - and if then no convergence, keep ||:
#"In my opinion, LRTs are only meaningful for the comparison of non-degenerate models. Thus, if extending the reduced model with
#correlation parameters leads to overparameterization, I would stay with the reduced model."
#LRT - likelihood ratio tests in the anova for comparing models
# although not in this case (M1 is ok): https://stat.ethz.ch/pipermail/r-sig-mixed-models/2015q3/023774.html


me <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode|subject) + 
             (1+ParticleCode+FocCode+CongruenceCode|sentence), 
           data = dat_reduced, REML=FALSE)
summary(me)
#without correlations: print(summary(me), corr=FALSE)   

# Linear mixed model fit by maximum likelihood t-tests use Satterthwaite approximations to degrees of freedom [
#   lmerMod]
# Formula: logRT ~ ParticleCode * FocCode * CongruenceCode + (1 + ParticleCode +  
#                                                               FocCode + CongruenceCode | subject) + (1 + ParticleCode +      FocCode + CongruenceCode | sentence)
# Data: dat_reduced
# 
# AIC      BIC   logLik deviance df.resid 
# 1130.3   1286.7   -536.1   1072.3     1599 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -6.1161 -0.6090 -0.0102  0.6370  3.9315 
# 
# Random effects:
# Groups   Name           Variance  Std.Dev. Corr             
# sentence (Intercept)    0.0023614 0.04859                   
#          ParticleCode   0.0051542 0.07179  -0.23            
#          FocCode        0.0090618 0.09519   0.49  0.13      
#          CongruenceCode 0.0090763 0.09527   0.41  0.32  0.45
# subject  (Intercept)    0.0688895 0.26247                   
#          ParticleCode   0.0068080 0.08251  -0.61            
#          FocCode        0.0038347 0.06193   0.52 -0.93      
#          CongruenceCode 0.0008825 0.02971   0.09  0.35 -0.03
# Residual                0.0974668 0.31220                   
# Number of obs: 1628, groups:  sentence, 192; subject, 34
###In Kliegl_Adv_LMM_UNIP2017_final/Session_10A-11A/Session_10A_4.html:
#"None of the variance components are very close to zero and none of the correlation parameters 
# are at the boundary (i.e., assume values of +1 or -1)." 
#if they were: model degenerate
#and he also has -0.03 (and 0.02), so not so close to zero
#Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015). Parsimonious mixed models. arXiv preprint arXiv:1506.04967.
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)                           7.955575   0.046139  35.030000 172.426  < 2e-16 ***
#   ParticleCode                          0.058584   0.024711  54.830000   2.371 0.021296 *  
#   FocCode                               0.020361   0.022873  60.420000   0.890 0.376911    
#   CongruenceCode                       -0.006227   0.020889  63.950000  -0.298 0.766583    
#   ParticleCode:FocCode                 -0.097009   0.040514 175.340000  -2.394 0.017699 *  
#   ParticleCode:CongruenceCode           0.139266   0.040515 175.350000   3.437 0.000734 ***
#   FocCode:CongruenceCode               -0.196554   0.040514 175.340000  -4.852 2.69e-06 ***
#   ParticleCode:FocCode:CongruenceCode   0.246333   0.081028 175.340000   3.040 0.002727 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) PrtclC FocCod CngrnC PrC:FC PrC:CC FcC:CC
# ParticleCod -0.351                                          
# FocCode      0.260 -0.230                                   
# CongruencCd  0.041  0.094  0.086                            
# PrtclCd:FcC  0.005  0.095 -0.036  0.000                     
# PrtclCd:CnC  0.013  0.079  0.000 -0.040  0.103              
# FcCd:CngrnC  0.023  0.000  0.085  0.112  0.057  0.023       
# PrtcC:FC:CC  0.000  0.085  0.051  0.023  0.096  0.116 -0.041

#this model is 'slightly' overparameterized b/c of the low variance at CongruenceCode $subject
#i.e. the data in this experiment do not support this variance component (subjects)
#but dropping this component does not increase the goodness of fit (or how to say it?)
#anova me and m1 is not sig. so we prefer me to m1: The goodness of fit for m1 is not significantly different from me
#Cf. "The significant increase in goodness of fit when going from LMM m2 to LMM m3, 
#suggests that there is significant information associated with the ensemble of correlation parameters."


#'Parsimonious Mixed Models' by Douglas Bates, Reinhold Kliegl, Shravan Vasishth, Harald Baayen
#(Submitted on 16 Jun 2015) https://arxiv.org/abs/1506.04967
#The analysis of experimental data with mixed-effects models requires decisions about the specification of 
#the appropriate random-effects structure. Recently, Barr et al. (2013) recommended fitting 'maximal' models 
#with all possible random effect components included. Estimation of maximal models, however, may not converge. 
#We show that failure to converge typically is not due to a suboptimal estimation algorithm, but is a consequence of 
#attempting to fit a model that is too complex to be properly supported by the data, irrespective of whether 
#estimation is based on maximum likelihood or on Bayesian hierarchical modeling with uninformative or 
#weakly informative priors. Importantly, even under convergence, overparameterization may lead to uninterpretable 
#models. We provide diagnostic tools for detecting overparameterization and guiding model simplification. 
#Finally, we clarify that the simulations on which Barr et al. base their recommendations are atypical for real data. A detailed example is provided of how subject-related attentional fluctuation across trials may further qualify statistical inferences about fixed effects, and of how such nonlinear effects can be accommodated within the mixed-effects modeling framework.

#Is the me model overparameterized/degenerate?
#list the percentages of variance associated with the 4 components.
library(RePsychLing)
summary(rePCA(me))
# $subject
# Importance of components%s:
#                           [,1]    [,2]   [,3]     [,4]
# Standard deviation     0.8647 0.25718 0.1055 0.000383
# Proportion of Variance 0.9063 0.08016 0.0135 0.000000
# Cumulative Proportion  0.9063 0.98650 1.0000 1.000000
#bad, we get 0 for component 4 with subjects ->
#-> this seems to fit with the SD 0.02971 and Corr -0.3 for CongruenceCode $subject
#"The svd analysis indicates that the maxLMM is overparameterized. The Cholesky factor decomposition yields
#two columns with zero values. Thus, there is clear evidence for singularity."

#Why use it? See this note:
#how many random effects can reasonably be included in a mixed model for a given data set. 
#I was intrigued by the use of pca to assess rank deficiency in mixed-models as suggested in "Parsimonous mixed models" by Doug Bates et al. (http://arxiv.org/abs/1506.04967). As suggested there, I wanted to check the vignette(s) for RePsychLing on how to use the rePCA function.

#"Does the goodness of fit decrease significantly if we assume that the ensemble of correlation parameters is zero?"
#This model doesn't even converge
zme <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode||subject) + 
             (1+ParticleCode+FocCode+CongruenceCode||sentence), 
           data = dat_reduced, REML=FALSE, control=lmerControl(optCtrl=list(maxfun=10000L)))
#Model is nearly unidentifiable: large eigenvalue ratio

#Drop variance components?
#"Please note that taking a correlation parameter or a variance component out of a model does not mean that it is
#zero, but that the data do not support a model that assumes that it is different from zero."
#since CongruenceCode $subject has 0 proportion of variance, drop it for subjects
me1 <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode|subject) + 
             (1+ParticleCode+FocCode+CongruenceCode|sentence), 
           data = dat_reduced, REML=FALSE)
#Model is nearly unidentifiable: large eigenvalue ratio
#funny, if the full model converged... so drop it for sentecens/items too:
me1 <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
              (1+ParticleCode+FocCode|subject) + 
              (1+ParticleCode+FocCode|sentence), 
            data = dat_reduced, REML=FALSE)
print(summary(me1), corr=FALSE) 
###same p-values as above:
# Linear mixed model fit by maximum likelihood t-tests use Satterthwaite approximations to degrees of freedom [
#   lmerMod]
# Formula: logRT ~ ParticleCode * FocCode * CongruenceCode + (1 + ParticleCode +  
#                                                               FocCode | subject) + (1 + ParticleCode + FocCode | sentence)
# Data: dat_reduced
# 
# AIC      BIC   logLik deviance df.resid 
# 1116.1   1229.4   -537.1   1074.1     1607 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -6.2864 -0.5958 -0.0131  0.6367  3.9341 
# 
# Random effects:
#   Groups   Name         Variance Std.Dev. Corr       
# sentence (Intercept)  0.002485 0.04985             
# ParticleCode 0.003884 0.06232  -0.60      
# FocCode      0.018551 0.13620   0.25 -0.12
# subject  (Intercept)  0.068918 0.26252             
# ParticleCode 0.006754 0.08218  -0.62      
# FocCode      0.003989 0.06316   0.52 -0.92
# Residual              0.097711 0.31259             
# Number of obs: 1628, groups:  sentence, 192; subject, 34
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                           7.955481   0.046145  35.010000 172.403  < 2e-16 ***
#   ParticleCode                          0.058517   0.024653  53.190000   2.374 0.021260 *  
#   FocCode                               0.020297   0.022944  56.440000   0.885 0.380094    
#   CongruenceCode                       -0.006244   0.020226 182.940000  -0.309 0.757878    
#   ParticleCode:FocCode                 -0.097108   0.040450 182.930000  -2.401 0.017368 *  
#   ParticleCode:CongruenceCode           0.139242   0.040451 182.940000   3.442 0.000715 ***
#   FocCode:CongruenceCode               -0.196730   0.040450 182.930000  -4.864 2.48e-06 ***
#   ParticleCode:FocCode:CongruenceCode   0.246127   0.080901 182.930000   3.042 0.002693 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(rePCA(me1))
# $subject
# Importance of components%s:
#   [,1]    [,2]    [,3]
# Standard deviation     0.8641 0.25420 0.06330
# Proportion of Variance 0.9158 0.07926 0.00491
# Cumulative Proportion  0.9158 0.99509 1.00000
##now 3 goes down
anova(me, me1)
#no sig. difference: prefer me to me1 

#so here keeping it maximal (almost b/c without the interactions in random effect structure)
#is justified

###in R version 3.5.0 (2018-04-23) -- "Joy in Playing" on April 30, ****none of the above converge****
#lme4 1.1017
#lmerTest 3.0-1
me2 <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
              (1+ParticleCode+FocCode||subject) + 
              (1+ParticleCode+FocCode||sentence), 
            data = dat_reduced, REML=FALSE)
print(summary(me2), corr=FALSE) 
#similar p-values


library("simr")
#the SIMR package conducts simulations to estimate power for multilevel models
#if there is an error due to missing dat: dfdata2 <- na.omit(dfdata)
fixef(me2)
fixef(me2)["ParticleCode"] <- 0.05
fixef(me2)["FocCode"] <- 0.02
fixef(me2)["CongruenceCode"] <- -0.01
fixef(me2)["ParticleCode:FocCode"] <- -0.1
fixef(me2)["ParticleCode:CongruenceCode"] <- 0.1
fixef(me2)["FocCode:CongruenceCode"] <- -0.2
fixef(me2)["ParticleCode:FocCode:CongruenceCode"] <- 0.2
powerSim(me2, fixed("ParticleCode:FocCode:CongruenceCode", "lr"), nsim = 50)
# Power for predictor 'ParticleCode:FocCode:CongruenceCode', (95% confidence interval):===================================|
#   78.00% (64.04, 88.47)
# 
# Test: Likelihood ratio
# Effect size for ParticleCode:FocCode:CongruenceCode is 0.20
# 
# Based on 50 simulations, (47 warnings, 0 errors)
# alpha = 0.05, nrow = 1628
# 
# Time elapsed: 0 h 0 m 57 s


######just the 7 'good' participants
str(PL3success)
PL3success$ParticleCode = ifelse(PL3success$particle == "most", .5, -.5)
PL3success$CongruenceCode = ifelse(PL3success$congruence == "incongruent", -.5, .5)
PL3success$FocCode = ifelse(PL3success$foc == "obj", -.5, .5)
#look only at the Yes-Screens
PL3success2 <- droplevels(subset(PL3success, congruence != "incongruent"))
str(PL3success2)

dat_reduced <- PL3success2[,c("logRT", "ParticleCode", "FocCode", "subject", "sentence")]
# 168 obs of 5 variables

me7subjectsYes <- lmer(logRT ~ ParticleCode * FocCode + 
                      (1|subject) + 
                      (1|sentence), 
                    data = dat_reduced, REML=FALSE)
summary(me7subjectsYes)
# Linear mixed model fit by maximum likelihood  
# t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
# Formula: logRT ~ ParticleCode * FocCode + (1 | subject) + (1 | sentence)
# Data: dat_reduced
# 
# AIC      BIC   logLik deviance df.resid 
# 120.1    141.9    -53.0    106.1      161 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.22005 -0.61169  0.00215  0.61783  2.41085 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# sentence (Intercept) 0.01472  0.1213  
# subject  (Intercept) 0.04569  0.2137  
# Residual             0.08569  0.2927  
# Number of obs: 168, groups:  sentence, 96; subject, 7
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
#   (Intercept)           7.98219    0.08499  7.16000  93.917 2.45e-12 ***
#   ParticleCode          0.11397    0.05253 67.04000   2.170   0.0336 *  
#   FocCode              -0.09014    0.05253 67.04000  -1.716   0.0908 .  
#   ParticleCode:FocCode  0.08252    0.10506 67.04000   0.785   0.4349    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) PrtclC FocCod
# ParticleCod 0.000               
# FocCode     0.000  0.000        
# PrtclCd:FcC 0.000  0.000  0.000
VarCorr(me7subjectsYes)
#When using lme4, singularity is most obviously detectable in the output of summary.merMod() or VarCorr.merMod() when a variance is estimated as 0 (or very small, i.e. orders of magnitude smaller than other variance components) or when a correlation is estimated as exactly ±1
#. However, as pointed out by D. Bates, Kliegl, et al. (2015), singularities in larger variance-covariance matrices can be hard to detect: checking for small values among the diagonal elements of the Cholesky factor is a good start.
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#zero-inflation
# Barr et al. (2013) suggest always starting with the maximal model (i.e. the most random-effects component of the model that is theoretically identifiable given the experimental design) and then dropping terms when singularity or non-convergence occurs (please see the paper for detailed recommendations …)
# Matuschek et al. (2017) and D. Bates, Kliegl, et al. (2015) strongly disagree, suggesting that models should be simplified a priori whenever possible; they also provide tools for diagnosing and mitigating singularity.

#After fitting a model (any model), the model should always be validated to ensure that it is appropriate and has fit the data well. At the very least, this should involve an exploration of the residuals.
plot(me7subjectsYes)
qqnorm(resid(me7subjectsYes))

library("simr")
#the SIMR package conducts simulations to estimate power for multilevel models
#if there is an error due to missing dat: dfdata2 <- na.omit(dfdata)
fixef(me7subjectsYes)
fixef(me7subjectsYes)["ParticleCode"] <- 0.1
fixef(me7subjectsYes)["FocCode"] <- -0.1
fixef(me7subjectsYes)["ParticleCode:FocCode"] <- 0.1

#Main effects should not be tested when they appear in an interaction term. Using the fcompare function, we can specify a comparison with a simpler model (without having to re-type the random effects specification).
# doTest(me7subjectsYes, compare(~ ParticleCode + FocCode + (1|subject) + (1|sentence)))
# Error in anova.merMod(model1, model2, test = "Chisq") : 
#   models were not all fitted to the same size of dataset
#but this works:
powerSim(me7subjectsYes, compare(~ ParticleCode + FocCode + (1|subject) + (1|sentence)))
# Power for model comparison, (95% confidence interval):==================================================================|
#   18.70% (16.33, 21.26)
# 
# Test: Likelihood ratio
# Comparison to ~ParticleCode + FocCode + (1 | subject) + (1 | sentence)
# 
# Based on 1000 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 168
# 
# Time elapsed: 0 h 2 m 15 s
powerSim(me7subjectsYes, fixed("ParticleCode:FocCode", "lr"), nsim = 50)
# Power for predictor 'ParticleCode:FocCode', (95% confidence interval):==================================================|
#   20.00% (10.03, 33.72)
# 
# Test: Likelihood ratio
# Effect size for ParticleCode:FocCode is 0.10
# 
# Based on 50 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 168
# 
# Time elapsed: 0 h 0 m 5 s

#This is quite low, we would like to see power above 80%, so let’s see how increasing sample size affects that.
#http://environmentalcomputing.net/power-analysis/
# Increasing the size within groups https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12504#mee312504-sec-0003-title 
# The data set is representative of environmental monitoring data, with a response variable z (e.g. bird abundance) measured at 10 levels of the continuous fixed effect variable x (e.g. study year) for three groups g (e.g. study site). 
# Each group has only one observation at each level of x and g. 
# We can extend this to five observations per site per year as follows:
# model4 <‐ extend(model1, within="x+g", n=5)

#How can I extend for subjects keeping items?
#168 observations: 4 conditions * 6 items * 7 participants
#864 observations: 4 conditions * 6 items * 36 participants
moresubjects <- extend(me7subjectsYes, within=("ParticleCode+FocCode"), n=36) #but how does it know there are 6 items?
powerSim(moresubjects, compare(~ ParticleCode + FocCode + (1|subject) + (1|sentence)))
# No, it doesn't know! nrow = 144
# Power for model comparison, (95% confidence interval):==================================================================|
#   13.30% (11.26, 15.56)
# 
# Test: Likelihood ratio
# Comparison to ~ParticleCode + FocCode + (1 | subject) + (1 | sentence)
# 
# Based on 1000 simulations, (1 warning, 0 errors)
# alpha = 0.05, nrow = 144
# 
# Time elapsed: 0 h 1 m 48 s
powerSim(moresubjects, fixed("ParticleCode:FocCode", "lr"), nsim = 50)
# Power for predictor 'ParticleCode:FocCode', (95% confidence interval):==================================================|
#   12.00% ( 4.53, 24.31)
# 
# Test: Likelihood ratio
# Effect size for ParticleCode:FocCode is 0.10
# 
# Based on 50 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 144
# 
# Time elapsed: 0 h 0 m 5 s

#36 * 6?
moresubjects <- extend(me7subjectsYes, within=("ParticleCode+FocCode"), n=216)
powerSim(moresubjects, fixed("ParticleCode:FocCode", "lr"), nsim = 50)
# Power for predictor 'ParticleCode:FocCode', (95% confidence interval):==================================================|
#   36.00% (22.92, 50.81)
# 
# Test: Likelihood ratio
# Effect size for ParticleCode:FocCode is 0.10
# 
# Based on 50 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 864 ---> that's ok! but does it know which are items, which are subjects? no
# 
# Time elapsed: 0 h 0 m 6 s

# THIS IS BETTER:
moresubjects <- extend(me7subjectsYes, along="subject", n=36)
powerSim(moresubjects, fixed("ParticleCode:FocCode", "lr"), nsim = 50)
# Power for predictor 'ParticleCode:FocCode', (95% confidence interval):==================================================|
#   44.00% (29.99, 58.75) (with 50, but with 1000: 34.20%)
# 
# Test: Likelihood ratio
# Effect size for ParticleCode:FocCode is 0.10
# 
# Based on 50 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 864
# 
# Time elapsed: 0 h 0 m 8 s



# If pilot data is not available, simr can be used to create lme4 objects from scratch as a starting point. This requires more paramters to be specified by the user. Values for these parameters might come from the literature or the user’s own knowledge and experience.
#power <- powerSim(me7subjectsYes, test=fcompare(. ~ParticleCode * FocCode), nsim=7)
#power <- powerSim(me7subjectsYes, fixed("ParticleCode:FocCode", "z"), nsim=7)
# You need to specify the level of the effect that you want to be able to detect, if you use the estimated level from a pilot study, this is called an “ observed power” calculation, and is not a valid use of power analysis.
#  Retrospective 'observed power' calculations, where the target effect size comes from the data, give misleading results (Hoenig & Heisey 2001).




#look at both Yes and No screens
dat_reduced <- PL3success[,c("logRT", "ParticleCode", "FocCode", "CongruenceCode", "subject", "sentence")]

me7subjects <- lmer(logRT ~ ParticleCode * FocCode * CongruenceCode + 
                         (1|subject) + 
                         (1|sentence), 
                       data = dat_reduced, REML=FALSE)
summary(me7subjects)
#only interactions are significant


#####COMPARE 2 GROUPS
#add a grouping variable

#first get two data sets
PL3success$group      <- 1
str(PL3success)

PL3nosuccess <- droplevels(subset(PL3, !(subject == "v1p7" | subject == "v2p3" | subject == "v3p1" | 
                                subject == "v3p3" | subject == "v3p6" | subject == "v4p6" | 
                                subject == "v4p8")))
str(PL3nosuccess)
#$ subject          : Factor w/ 27 levels
#> 1292 + 336 = 1628 which is all observations, see str(total)
PL3nosuccess$group      <- 0

#combine the groups
total <- rbind(PL3success, PL3nosuccess)
str(total)
total$group <- as.factor(total$group)

#drop incongruent
total.congruent <- droplevels(subset(total, congruence != "incongruent"))
str(total.congruent)
#815 obs.

#plotting
source(file = "Helper_functions-within-subjects-se.R")

datac <- 
  summarySEwithin(total.congruent, measurevar="logRT", 
                  withinvars=c("particle","foc", "group"), idvar="subject")

#2-panel GRAPH = groups on separate panels
labels <- c('1' = "Successful", '0' = "Unsuccessful") #labels <- c(Female = "Women", Male = "Men") http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#modifying-facet-label-text

ggplot(datac, aes(foc, logRT, group=particle, shape=particle, color=particle)) +
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=particle), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ group, labeller=labeller(group = labels)) +
  xlab("Congruence") + ylab("Reaction time (log ms)")

####now we see better than the successful people were slower with MostObject (8.05) and OnlyObject (8.0)
#### while unsuccessful had OnlyObject (7.9) ---> the errorbars almost overlap 7.9 and 8 for OnlyObj.
#(remember WHOLE group also had MostObject (8.05) BUT OnlyObject (7.95))
#---->indeed, no significant difference between groups


dat_reduced <- total.congruent[,c("logRT", "ParticleCode", "FocCode", "subject", "sentence", "group")]


me.total <- lmer(logRT ~ ParticleCode * FocCode * group + 
                   (ParticleCode+FocCode|subject) + 
                   (1|sentence), 
                 data = dat_reduced, REML=FALSE)
summary(me.total)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   7.944821   0.053110  35.672446 149.593  < 2e-16 ***
# ParticleCode                  0.131201   0.037590  46.890536   3.490  0.00106 ** 
# FocCode                      -0.075410   0.038153  47.180996  -1.977  0.05396 .   --> focus marginal
# group1                        0.035468   0.115175  33.465554   0.308  0.76003    
# ParticleCode:FocCode          0.007813   0.064133 118.639916   0.122  0.90324    
# ParticleCode:group1          -0.014577   0.071383  33.172779  -0.204  0.83944    
# FocCode:group1               -0.012618   0.072813  31.746624  -0.173  0.86352    --> BUT NO INTERACTION
# ParticleCode:FocCode:group1   0.089459   0.113468 663.025222   0.788  0.43074
# "If subgroup analysis is performed, and the question of interest is 
# whether a particular treatment performs differently in subgroup X as compared to subgroup Y, 
# it is important to realize that the analytical translation of this question refers to 
# testing statistical interaction."


#######################################################
#Accuracy
str(PL3)
PL3$Accuracy = ifelse(PL3$response_accuracy=="correct", 1, 0)

dat_reduced <- PL3[,c("Accuracy", "ParticleCode", "FocCode", "CongruenceCode", "subject", "sentence")]

gm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode*FocCode*CongruenceCode||subject) + 
             (1+ParticleCode*FocCode*CongruenceCode||sentence), 
           data = dat_reduced, family = binomial)
#  Model failed to converge with max|grad| = 0.371008 (tol = 0.001, component 1)

gm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode||subject) + 
             (1+ParticleCode+FocCode+CongruenceCode||sentence), 
           data = dat_reduced, family = binomial)
#  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

gm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode|subject) + 
             (1+ParticleCode+FocCode+CongruenceCode|sentence), 
           data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(gm)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Accuracy ~ ParticleCode * FocCode * CongruenceCode + (1 + ParticleCode +  
#                                                                  FocCode + CongruenceCode | subject) + (1 + ParticleCode +      FocCode + CongruenceCode | sentence)
# Data: dat_reduced
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 883.3   1034.4   -413.7    827.3     1600 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -11.0171   0.0737   0.1401   0.2375   2.4653 
# 
# Random effects:
#   Groups   Name           Variance Std.Dev. Corr             
#   sentence (Intercept)    0.12506  0.3536                    
#            ParticleCode   1.86415  1.3653   -0.65            
#            FocCode        0.95798  0.9788    0.05  0.73      
#            CongruenceCode 0.05573  0.2361    0.97 -0.82 -0.21
#   subject  (Intercept)    0.46614  0.6827                    
#            ParticleCode   2.00209  1.4150    0.45            
#            FocCode        0.51957  0.7208   -0.85 -0.47      
#            CongruenceCode 2.98048  1.7264   -0.09 -0.31 -0.38  ===> again, this is not bad
# Number of obs: 1628, groups:  sentence, 192; subject, 34
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.2948     0.2841  11.597  < 2e-16 ***
#   ParticleCode                         -0.5472     0.5263  -1.040 0.298525    
#   FocCode                              -0.0221     0.4503  -0.049 0.960857    
#   CongruenceCode                       -2.2147     0.5103  -4.340 1.43e-05 ***
#   ParticleCode:FocCode                  2.3194     0.8267   2.805 0.005025 ** 
#   ParticleCode:CongruenceCode          -3.7926     1.0228  -3.708 0.000209 ***
#   FocCode:CongruenceCode                2.4391     0.8737   2.792 0.005242 ** 
#   ParticleCode:FocCode:CongruenceCode   6.1316     1.2566   4.879 1.06e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) PrtclC FocCod CngrnC PrC:FC PrC:CC FcC:CC
# ParticleCod  0.065                                          
# FocCode     -0.301  0.200                                   
# CongruencCd -0.375 -0.218  0.083                            
# PrtclCd:FcC  0.225 -0.201 -0.051  0.097                     
# PrtclCd:CnC -0.313 -0.418  0.014  0.029  0.106              
# FcCd:CngrnC  0.114 -0.001 -0.427 -0.123 -0.267  0.233       
# PrtcC:FC:CC  0.194  0.167 -0.208 -0.038 -0.290 -0.216  0.115
summary(rePCA(gm))
#===> but this looks bad:
# $sentence
# Importance of components%s:
#                           [,1]   [,2]      [,3] [,4]
# Standard deviation     1.5937 0.6805 0.0001463    0
# Proportion of Variance 0.8458 0.1542 0.0000000    0
# Cumulative Proportion  0.8458 1.0000 1.0000000    1
# 
# $subject
# Importance of components%s:
#                           [,1]   [,2]    [,3] [,4]
# Standard deviation     1.8536 1.4277 0.70294    0
# Proportion of Variance 0.5757 0.3415 0.08279    0
# Cumulative Proportion  0.5757 0.9172 1.00000    1

#zero correlations model
zgm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
              (1+ParticleCode+FocCode+CongruenceCode||subject) + 
              (1+ParticleCode+FocCode+CongruenceCode||sentence), 
            data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

##Drop variance components?
#for sentences FocCode&CongruenceCode, and for  subjects CongruenceCode
gm1 <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
               (1+ParticleCode+FocCode|subject) + 
               (1+ParticleCode|sentence), 
             data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(gm1)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Accuracy ~ ParticleCode * FocCode * CongruenceCode + (1 + ParticleCode +  
#                                                                  FocCode | subject) + (1 + ParticleCode | sentence)
# Data: dat_reduced
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 888.5    980.2   -427.3    854.5     1611 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -14.6083   0.1027   0.1903   0.2679   2.2809 
# 
# Random effects:
#   Groups   Name         Variance Std.Dev. Corr       
# sentence  (Intercept)  0.1861   0.4314              
#           ParticleCode 0.7446   0.8629   -1.00      
# subject   (Intercept)  0.4539   0.6737              
#           ParticleCode 1.2664   1.1253    0.36      
#           FocCode      0.6464   0.8040   -0.89 -0.74
# Number of obs: 1628, groups:  sentence, 192; subject, 34
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.8419     0.2118  13.417  < 2e-16 ***
#   ParticleCode                         -0.6942     0.4090  -1.697   0.0896 .  
#   FocCode                              -0.1543     0.3365  -0.459   0.6465    
#   CongruenceCode                       -2.0259     0.2846  -7.119 1.09e-12 ***
#   ParticleCode:FocCode                  1.4005     0.5900   2.374   0.0176 *  
#   ParticleCode:CongruenceCode          -2.9439     0.5677  -5.186 2.15e-07 ***
#   FocCode:CongruenceCode                2.6363     0.5679   4.642 3.45e-06 ***
#   ParticleCode:FocCode:CongruenceCode   5.8701     1.1362   5.166 2.39e-07 ***
summary(rePCA(gm1))
# $sentence
# Importance of components%s:
#                           [,1] [,2]
# Standard deviation     0.9647    0
# Proportion of Variance 1.0000    0
# Cumulative Proportion  1.0000    1
# 
# $subject
# Importance of components%s:
#                           [,1]   [,2] [,3]
# Standard deviation     1.368 0.7034    0
# Proportion of Variance 0.791 0.2090    0
# Cumulative Proportion  0.791 1.0000    1
anova(gm, gm1)
#sig. 0.004287 ** so prefer gm1 to  gm

#given that svd analysis indicates singularity in gm1, further drop variance components
gm2 <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
               (1+ParticleCode|subject) + 
               (1|sentence), 
             data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(gm2)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Accuracy ~ ParticleCode * FocCode * CongruenceCode + (1 + ParticleCode |  
#                                                                  subject) + (1 | sentence)
# Data: dat_reduced
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 890.2    954.9   -433.1    866.2     1616 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -11.6943   0.1004   0.1975   0.2786   2.7436 
# 
# Random effects:
#   Groups   Name         Variance Std.Dev. Corr
#   sentence (Intercept)  0.2614   0.5113       
#   subject  (Intercept)  0.4502   0.6710       
#            ParticleCode 1.9508   1.3967   0.57
# Number of obs: 1628, groups:  sentence, 192; subject, 34
# 
# Fixed effects:
#                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.8102     0.2118  13.269  < 2e-16 ***
# ParticleCode                         -0.1961     0.3885  -0.505  0.61364    
# FocCode                               0.1534     0.2742   0.559  0.57594    
# CongruenceCode                       -2.0023     0.2777  -7.210 5.59e-13 ***
# ParticleCode:FocCode                  1.7390     0.5506   3.158  0.00159 ** 
# ParticleCode:CongruenceCode          -2.9315     0.5552  -5.280 1.29e-07 ***
# FocCode:CongruenceCode                2.5807     0.5495   4.697 2.64e-06 ***
# ParticleCode:FocCode:CongruenceCode   5.6705     1.1046   5.133 2.84e-07 ***

anova(gm1, gm2)
#sig. 0.03971 * so prefer gm2 to  gm1

#OK, so I follow Bates et al. in choosing the model, but obtain p-values by LmerTest
#this way I can see that for accuracy and justified random structure, there is no effect of Particle and Focus
#whereas with gm1 was marginal effect of Particle, and with gm one of the interactions was weaker

#####################################################

#  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

gm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
             (1+ParticleCode+FocCode+CongruenceCode|subject) + 
             (1|sentence), 
           data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#Model failed to converge with max|grad| = 0.163979 (tol = 0.001, component 1)

gm <- glmer(Accuracy ~ ParticleCode * FocCode * CongruenceCode + 
             (1|subject) + 
             (1|sentence), 
           data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#Model failed to converge with max|grad| = 0.147252 (tol = 0.001, component 1)
summary(me)


mg <- glmer(Accuracy ~ PolarityCode * CongruenceCode * LanguageCode + 
              (1+PolarityCode*CongruenceCode*LanguageCode||subject) + 
              (1+PolarityCode*CongruenceCode*LanguageCode||item), 
            data = dat_expt1, family = binomial, control = glmerControl(optimizer = "bobyqa"))


