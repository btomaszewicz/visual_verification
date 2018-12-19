setwd("~/Desktop/EXPT_1/")

library('plyr')
library('ggplot2')
library('extrafont')
library('lme4')
loadfonts()
source(file = "Helper_functions-within-subjects-se.R")

dat_expt1 <- read.csv("english_concat_dat_expt1.csv")

###############################################################################################
### Preprocessing stuff
###############################################################################################

# Filters only the desired dat_expt1a points relevant to analysis
dat_expt1 <- subset(dat_expt1, sentence != "")

# Removes dat_expt1a points with RTs < 200 ms
dat_expt1 <- subset(dat_expt1, (strtoi(rt) > 200))

# Creates appropriate column labels needed for the analysis
dat_expt1$judgment <- c(rep("yes", nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  if (dat_expt1$key_press[i] == "74") {
    dat_expt1$judgment[i] <- "no"
  }
}

dat_expt1$correct_judgment <- c(rep(NA, nrow(dat_expt1)))

case_a <- "B is shorter than A."
case_b <- "B is taller than A."
case_c <- "A is shorter than B."
case_d <- "A is taller than B."
case_e <- "A > B"
case_f <- "A < B"
case_g <- "B > A"
case_h <- "B < A"

#correct_judgment_check(case_a, rev, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_a) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_a, nor, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_a) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_b, rev, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_b) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_b, nor, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_b) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_c, rev, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_c) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_c, nor, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_c) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_d, rev, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_d) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_d, nor, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_d) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}

#correct_judgment_check(case_e, rev, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_e) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_e, nor, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_e) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_f, rev, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_f) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_f, nor, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_f) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_g, rev, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_g) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}
#correct_judgment_check(case_g, nor, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_g) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_h, rev, no)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_h) & (dat_expt1$Imagewinner[i] == "rev")) {
    dat_expt1$correct_judgment[i] <- "no"
  }
}
#correct_judgment_check(case_h, nor, yes)
for (i in 1:nrow(dat_expt1)) {
  if ((dat_expt1$sentence[i] == case_h) & (dat_expt1$Imagewinner[i] == "nor")) {
    dat_expt1$correct_judgment[i] <- "yes"
  }
}

dat_expt1$judgment_status <- c(rep("incorrect", nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  if (dat_expt1$judgment[i] == dat_expt1$correct_judgment[i]) {
    dat_expt1$judgment_status[i] <- "correct"
  }
}

dat_expt1$logRT <- c(rep(NA, nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  dat_expt1$logRT[i] <- log(strtoi(dat_expt1$rt[i]))
}

dat_expt1$Language <- c(rep("English", nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  if (grepl(">", dat_expt1$sentence[i])) {
    dat_expt1$Language[i] <- "Math"
  }
}

for (i in 1:nrow(dat_expt1)) {
  if (grepl("<", dat_expt1$sentence[i])) {
    dat_expt1$Language[i] <- "Math"
  }
}

dat_expt1$Congruence <- c(rep("Congruent", nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  if (dat_expt1$correct_judgment[i] == "no") {
    dat_expt1$Congruence[i] <- "Incongruent"
  }
}

dat_expt1$Polarity <- c(rep("Negative", nrow(dat_expt1)))

for (i in 1:nrow(dat_expt1)) {
  if (grepl(">", dat_expt1$sentence[i])) {
    dat_expt1$Polarity[i] <- "Positive"
  }
}

for (i in 1:nrow(dat_expt1)) {
  if (grepl("taller", dat_expt1$sentence[i])) {
    dat_expt1$Polarity[i] <- "Positive"
  }
}

for (i in 1:nrow(dat_expt1)) {
  dat_expt1$RT <- strtoi(dat_expt1$rt)
}

dat_expt1 <- transform(dat_expt1, Accuracy = ifelse(judgment_status=="correct", 1, 0))
dat_expt1$subject <- factor(dat_expt1$subject)
#################################################################################
# outlier removal (> 3 SDs)
dat_expt1$outlier_status <- c(rep("inlier", nrow(dat_expt1)))

new_dat <- subset(dat_expt1, outlier_status==0)

subject_list <- levels(dat_expt1$subject)

for (i in 1:length(subject_list)) {
  dat_sub_participant <- subset(dat_expt1, subject == subject_list[i])
  mean_RT <- mean(dat_sub_participant$RT)
  stdev_RT <- sd(dat_sub_participant$RT)
  stdev2x_RT <- (3*stdev_RT) + mean_RT
  stdev2x_RT.minus <- mean_RT-(3*stdev_RT)
  for (j in 1:nrow(dat_sub_participant)) {
    if ((dat_sub_participant$RT[j] > stdev2x_RT)|(dat_sub_participant$RT[j] < stdev2x_RT.minus)) {
      dat_sub_participant$outlier_status[j] <- "outlier"
    }}
  new_dat <- rbind(new_dat, dat_sub_participant)
}

dat_outliers_rm <- subset(new_dat, outlier_status=="inlier")
print("Percentage of data remaining after removing outliers >3 SDs")
print(nrow(dat_outliers_rm)/nrow(new_dat))

dat_expt1 <- dat_outliers_rm
write.csv(dat_outliers_rm, file = "EN_preprocessed.csv")

dat_expt1  <- read.csv("EN_preprocessed.csv")



###############################################################################################
# Data summary and plotting
###############################################################################################

dat_expt1_logRT_eng <- 
  summarySEwithin(dat_expt1, measurevar="logRT", 
                  withinvars=c("Polarity","Congruence","Language"), idvar="subject")
dat_expt1_RT_eng <- 
  summarySEwithin(dat_expt1, measurevar="RT", 
                  withinvars=c("Polarity","Congruence","Language"), idvar="subject")

dat_expt1_Accuracy_eng <- ddply(dat_expt1, .(Language, Polarity, Congruence), summarise,
                            N    = length(Accuracy),
                            mean = mean(Accuracy),
                            sd   = sd(Accuracy),
                            se   = sd / sqrt(N)
)

print(dat_expt1_logRT_eng)
print(dat_expt1_RT_eng)
print(dat_expt1_Accuracy_eng)

#2-panel GRAPH
ggplot(dat_expt1_logRT_eng, aes(Congruence, logRT, group=Polarity, shape=Polarity, color=Polarity)) +
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=Polarity), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ Language) +
  xlab("Congruence") + ylab("Reaction time (log ms)")

#this is not working on Windows, can't find fonts
a <- ggplot(dat_expt1_logRT_eng, aes(Congruence, logRT, group=Polarity, shape=Polarity, color=Polarity)) + 
  geom_errorbar(aes(ymin=(logRT-se), ymax=(logRT+se)), colour='black', size=.75, width=0.2) + 
  geom_line(aes(linetype=Polarity), size=.75, colour='black') + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ Language) + coord_cartesian(ylim = c(6.0, 7.5)) +
  theme(strip.background = element_rect(colour="white",fill="white"),
        strip.text.x = element_text(size=12)) +
  theme(text=element_text(size=12, family="CM Roman")) +
  xlab("Congruence") + ylab("Reaction time (log ms)")

ggsave("expt1_English_RTs.pdf", plot=a, width=7.25, height=5.25, units=c("in"))



#2-panel GRAPH
ggplot(dat_expt1_Accuracy_eng, aes(Congruence, 100*mean, group=Polarity, shape=Polarity, color=Polarity)) +
  geom_errorbar(aes(ymin=100*(mean-se), ymax=100*(mean+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype=Polarity), size=.75) + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ Language)+
  xlab("Congruence") + ylab("Percent correct")

b <- ggplot(dat_expt1_Accuracy_eng, aes(Congruence, 100*mean, group=Polarity, shape=Polarity, color=Polarity)) + 
  geom_errorbar(aes(ymin=100*(mean-se), ymax=100*(mean+se)), colour='black', size=.75, width=0.2) + 
  geom_line(aes(linetype=Polarity), size=.75, colour='black') + 
  geom_point(stat="identity", position="dodge", size=5) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ Language) + coord_cartesian(ylim=c(75,100)) +
  theme(strip.background = element_rect(colour="white",fill="white"),strip.text.x = element_text(size=12)) +
  theme(text=element_text(size=12, family="CM Roman")) +
  xlab("Congruence") + ylab("Percent correct") + ggtitle("")

ggsave("expt1_English_PercentCorrect.pdf", plot=b, width=7.25, height=5.25, units=c("in"))

###############################################################################################
# LMEMs
###############################################################################################

dat_expt1$PolarityCode = ifelse(dat_expt1$Polarity == "Positive", .5, -.5)
dat_expt1$CongruenceCode = ifelse(dat_expt1$Congruence == "Incongruent", -.5, .5)
dat_expt1$LanguageCode = ifelse(dat_expt1$Language == "Math", -.5, .5)

dat_expt1$item <- paste(dat_expt1$Imagewinner,dat_expt1$ratio1,dat_expt1$px)

me <- lmer(logRT ~ PolarityCode * CongruenceCode * LanguageCode + 
             (1+PolarityCode*CongruenceCode*LanguageCode||subject) + 
             (1+PolarityCode*CongruenceCode*LanguageCode||item), 
           data = dat_expt1, REML=FALSE, control = lmerControl(optimizer = "bobyqa"))

# model comparison 
ame1a <- anova(me,update(me,.~.-PolarityCode))
ame1b <- anova(me,update(me,.~.-CongruenceCode))
ame1c <- anova(me,update(me,.~.-LanguageCode))
ame2a <- anova(me,update(me,.~.-PolarityCode:CongruenceCode))
ame2b <- anova(me,update(me,.~.-PolarityCode:LanguageCode))
ame2c <- anova(me,update(me,.~.-LanguageCode:CongruenceCode))
ame3 <- anova(me,update(me,.~.-PolarityCode:CongruenceCode:LanguageCode))

# print the results
a <- ame1a[6:8[1[1]]]
b <- ame1b[6:8[1[1]]]
c <- ame1c[6:8[1[1]]]
d <- ame2a[6:8[1[1]]]
e <- ame2b[6:8[1[1]]]
f <- ame2c[6:8[1[1]]]
g <- ame3[6:8[1[1]]]

print("LMEM results - RTs")

print(a)
print(b)
print(c)
print(d)
print(e)
print(f)
print(g)

print(summary(me))

# LMEM - Accuracy

# run the model with maximal random effects structure
mg <- glmer(Accuracy ~ PolarityCode * CongruenceCode * LanguageCode + 
              (1+PolarityCode*CongruenceCode*LanguageCode||subject) + 
              (1+PolarityCode*CongruenceCode*LanguageCode||item), 
            data = dat_expt1, family = binomial, control = glmerControl(optimizer = "bobyqa"))

# model comparison 
ame1a <- anova(mg,update(mg,.~.-PolarityCode))
ame1b <- anova(mg,update(mg,.~.-CongruenceCode))
ame1c <- anova(mg,update(mg,.~.-LanguageCode))
ame2a <- anova(mg,update(mg,.~.-PolarityCode:CongruenceCode))
ame2b <- anova(mg,update(mg,.~.-PolarityCode:LanguageCode))
ame2c <- anova(mg,update(mg,.~.-LanguageCode:CongruenceCode))
ame3 <- anova(mg,update(mg,.~.-PolarityCode:CongruenceCode:LanguageCode))

# print the results
a <- ame1a[6:8[1[1]]]
b <- ame1b[6:8[1[1]]]
c <- ame1c[6:8[1[1]]]
d <- ame2a[6:8[1[1]]]
e <- ame2b[6:8[1[1]]]
f <- ame2c[6:8[1[1]]]
g <- ame3[6:8[1[1]]]

print("LMEM results - Accuracy")

print(a)
print(b)
print(c)
print(d)
print(e)
print(f)
print(g)

print(summary(mg))

###############################################################################################
# Statement constituent order analysis
###############################################################################################

# dat_expt1$comparison <- c(rep("TALLER(A,B)", nrow(dat_expt1)))
# for (i in 1:nrow(dat_expt1)) {
#   if (dat_expt1$Imagewinner[i]=="rev") {
#     dat_expt1$comparison[i] <- "TALLER(B,A)"
#   }
# }
# 
# dat_expt1$Sentence <- dat_expt1$sentence
# 
# # English dat_expt1a - polarity & congruence
# str(dat_expt1)
# eng_dat_expt1 <- subset(dat_expt1, language=="English")
# 
# dat_expt1_logRT_stim_eng <- 
#   summarySEwithin(eng_dat_expt1, measurevar="logRT", withinvars=c("Sentence","comparison"), idvar="subject")
# dat_expt1_RT_stim_eng <- 
#   summarySEwithin(eng_dat_expt1, measurevar="RT", withinvars=c("Sentence","comparison"), idvar="subject")
# 
# dat_expt1_logRT_stim_eng$language <- c(rep("English", nrow(dat_expt1_logRT_stim_eng)))
# 
# # Math dat_expt1a - polarity & congruence
# 
# math_dat_expt1 <- subset(dat_expt1, language=="Math")
# 
# dat_expt1_logRT_stim_math <- 
#   summarySEwithin(math_dat_expt1, measurevar="logRT", withinvars=c("sentence","comparison"), idvar="subject")
# dat_expt1_RT_stim_math <- 
#   summarySEwithin(math_dat_expt1, measurevar="RT", withinvars=c("sentence","comparison"), idvar="subject")
# 
# dat_expt1_logRT_stim_math$language <- c(rep("Math", nrow(dat_expt1_RT_stim_math)))
# 
# dat_expt1_plot <- rbind(dat_expt1_RT_stim_eng, dat_expt1_RT_stim_math)
# 
# # simple RT plot for English
# ggplot(dat_expt1_RT_stim_eng, aes(x=comparison, y=logRT, group=sentence, colour=sentence)) +
#   geom_errorbar(aes(ymin=logRT-se, ymax=logRT+se), size=1, width=.2) +
#   geom_line(aes(linetype=sentence), size=1.2) + theme_bw() +
#   geom_point(size=4.5, shape=21, fill="white") +
#   coord_cartesian(ylim = c(5.9,6.7)) +
#   xlab("Comparison") + ggtitle("English RTs (RTs > 200 ms)")
# 
# # simple RT plot for Math
# ggplot(dat_expt1_RT_stim_math, aes(x=comparison, y=logRT, group=sentence, colour=sentence)) +
#   geom_errorbar(aes(ymin=logRT-se, ymax=logRT+se), size=1, width=.2) +
#   geom_line(aes(linetype=sentence), size=1.2) + theme_bw() +
#   geom_point(size=4.5, shape=21, fill="white") + coord_cartesian(ylim = c(5.9,6.7)) +
#   xlab("Comparison") + ggtitle("Math RTs (RTs > 200 ms)")
# 
# ###############################################################################################
# # Statement constituent order analysis
# ###############################################################################################
# 
# str(dat_expt1)
# 
# dat_expt1_logRT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="logRT", withinvars=c("polarity"), idvar="subject")
# dat_expt1_RT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="RT", withinvars=c("polarity"), idvar="subject")
# 
# dat_expt1_logRT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="logRT", withinvars=c("congruence"), idvar="subject")
# dat_expt1_RT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="RT", withinvars=c("congruence"), idvar="subject")
# 
# dat_expt1_logRT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="logRT", withinvars=c("polarity"), idvar="subject")
# dat_expt1_RT_polarity <- 
#   summarySEwithin(dat_expt1, measurevar="RT", withinvars=c("polarity"), idvar="subject")
