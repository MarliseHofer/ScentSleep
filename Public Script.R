library(lme4)
library(lmerTest)
library(psych)
library(foreign)

setwd("~/Dropbox/research/Sleep Paper")
sleep<-read.csv("Public.csv")
summary(sleep)

###main table###
#run main sleep efficiency analysis
summary (empty <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep))
summary (one <- lmer (sleep.efficiency ~ 1 + scent.type + (1 + scent.type | participant), data=sleep))

#deviance reduction
anova(one, empty)

#run sleep efficiency analysis those unaware of scent order
summary (sleep_12 <- subset(sleep, sleep$correct.guess.shirt == 0))
summary (empty_12 <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep_12))
summary (one_12 <- lmer (sleep.efficiency ~ 1 + scent.type + (1 + scent.type | participant), data=sleep_12))

#deviance reduction
anova(one_12, empty_12)

#run rested and sleep quality composite analysis
# exclude variables participant 19 who completed incorrect index
summary (sleep_19 <- subset(sleep, sleep$participant != 19))
summary (empty3_19 <- lmer (perceived.sleep ~ 1 + (1 | participant), data=sleep_19))
summary (one3_19 <- lmer (perceived.sleep ~ 1 + scent.type + (1 + scent.type | participant), data=sleep_19))

#deviance reduction
anova(one3_19, empty3_19)

###supplementary table###
#run supplementary sleep efficiency analysis
summary (empty <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep))
sleep$stress<-(sleep$stress.today + sleep$stressed.tomorrow)/2
sleep$stress.cen<-sleep$stress-(tapply(sleep$stress, as.factor(sleep$participant), mean,na.rm=T) [as.factor(sleep$participant)])
summary (sleep$stress.cen)
summary(sleep$rel.sat.cen<-sleep$rel.sat-mean(sleep$rel.sat))
summary(sleep$rel.length.cen<-sleep$relation.length-mean(sleep$relation.length))
summary(sleep$AAQ_avoidance.cen<-sleep$AAQ_avoidance-mean(sleep$AAQ_avoidance))
summary(sleep$AAQ_ambivalence.cen<-sleep$AAQ_ambivalence-mean(sleep$AAQ_ambivalence))
summary (full <- lmer (sleep.efficiency ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep))

#deviance reduction
anova(full, empty)

#run sleep EFFICIENCY on HCP (high compliance participants)
# exclude variables participant 19, 11, 25, 26, 31, 38, 3, 5, 7, 15, 23)
summary (sleep_HCP <- subset(sleep, sleep$participant != 19 & sleep$participant != 11 & sleep$participant != 25 & sleep$participant != 26 & sleep$participant != 31 & sleep$participant != 38 & sleep$participant != 3 & sleep$participant != 5 & sleep$participant != 7 & sleep$participant != 15 & sleep$participant != 23))
sleep_HCP$stress<-(sleep_HCP$stress.today + sleep_HCP$stressed.tomorrow)/2
sleep_HCP$stress.cen <-sleep_HCP$stress-(tapply(sleep_HCP$stress, as.factor(sleep_HCP$participant), mean,na.rm=T) [as.factor(sleep_HCP$participant)])
summary (sleep_HCP$stress.cen)
summary (sleep_HCP$rel.sat.cen<-sleep_HCP$rel.sat-mean(sleep_HCP$rel.sat))
summary (sleep_HCP$rel.length.cen<-sleep_HCP$relation.length-mean(sleep_HCP$relation.length))
summary (sleep_HCP$AAQ_avoidance.cen<-sleep_HCP$AAQ_avoidance-mean(sleep_HCP$AAQ_avoidance))
summary (sleep_HCP$AAQ_ambivalence.cen<-sleep_HCP$AAQ_ambivalence-mean(sleep_HCP$AAQ_ambivalence))
summary (empty_HCP <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep_HCP))
summary (main_HCP <- lmer (sleep.efficiency ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep_HCP))

#deviance reduction
anova(main_HCP, empty_HCP)

#run supplementary rested and sleep quality composite analysis
# exclude variables participant 19 who completed incorrect index
summary (sleep_19 <- subset(sleep, sleep$participant != 19))
sleep_19$stress<-(sleep_19$stress.today + sleep_19$stressed.tomorrow)/2
sleep_19$stress.cen <-sleep_19$stress-(tapply(sleep_19$stress, as.factor(sleep_19$participant), mean,na.rm=T) [as.factor(sleep_19$participant)])
summary (sleep_19$stress.cen)
summary (sleep_19$rel.sat.cen<-sleep_19$rel.sat-mean(sleep_19$rel.sat))
summary (sleep_19$rel.length.cen<-sleep_19$relation.length-mean(sleep_19$relation.length))
summary (sleep_19$AAQ_avoidance.cen<-sleep_19$AAQ_avoidance-mean(sleep_19$AAQ_avoidance))
summary (sleep_19$AAQ_ambivalence.cen<-sleep_19$AAQ_ambivalence-mean(sleep_19$AAQ_ambivalence))
summary (empty3_19 <- lmer (perceived.sleep ~ 1 + (1 | participant), data=sleep_19))
summary (main3_19 <- lmer (perceived.sleep ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep_19))

#deviance reduction
anova(main3_19, empty3_19)


#run rested and sleep quality composite analysis on HCP (high compliance participants)
# exclude variables participant 19 (and 11, 25, 26, 31, 38, 3, 5, 7, 15, 23)
summary (empty_HCP <- lmer (perceived.sleep ~ 1 + (1 | participant), data=sleep_HCP))
summary (main_HCP <- lmer (perceived.sleep ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep_HCP))

#deviance reduction
anova(main_HCP, empty_HCP)

###other analyses###
#run rested and sleep quality seperately
summary (one4_19 <- lmer (Sleep.quality ~ 1 + scent.type + (1 + scent.type | participant), data=sleep_19))
summary (one5_19 <- lmer (rested ~ 1 + scent.type + (1 + scent.type | participant), data=sleep_19))

#run rested and sleep EFFICIENCY on LCP (low compliance participants)
# exclude variables participant 1, 2, 4, 8, 9, 12, 13, 14, 18, 20, 22, 24, 27, 32, 33, 34, 35, 37, 39, 40, 42, 43, 45, 46, 47, 48)
summary (sleep_LCP <- subset(sleep, sleep$participant != 1 & sleep$participant != 2 & sleep$participant != 4 & sleep$participant != 8 & sleep$participant != 9 & sleep$participant != 12 & sleep$participant != 13 & sleep$participant != 14 & sleep$participant != 18 & sleep$participant != 20 & sleep$participant != 22 & sleep$participant != 24 & sleep$participant != 27 & sleep$participant != 32 & sleep$participant != 33 & sleep$participant != 34 & sleep$participant != 35 & sleep$participant != 37 & sleep$participant != 39 & sleep$participant != 40 & sleep$participant != 42 & sleep$participant != 43 & sleep$participant != 45 & sleep$participant != 46 & sleep$participant != 47 & sleep$participant != 48))
sleep_LCP$stress<-(sleep_LCP$stress.today + sleep_LCP$stressed.tomorrow)/2
sleep_LCP$stress.cen <-sleep_LCP$stress-(tapply(sleep_LCP$stress, as.factor(sleep_LCP$participant), mean,na.rm=T) [as.factor(sleep_LCP$participant)])
summary (sleep_LCP$stress.cen)
summary (sleep_LCP$rel.sat.cen<-sleep_LCP$rel.sat-mean(sleep_LCP$rel.sat))
summary (sleep_LCP$rel.length.cen<-sleep_LCP$relation.length-mean(sleep_LCP$relation.length))
summary (sleep_LCP$AAQ_avoidance.cen<-sleep_LCP$AAQ_avoidance-mean(sleep_LCP$AAQ_avoidance))
summary (sleep_LCP$AAQ_ambivalence.cen<-sleep_LCP$AAQ_ambivalence-mean(sleep_LCP$AAQ_ambivalence))
summary (sleep_LCP$stress.4days.cen<-sleep_LCP$stress.4days-mean(sleep_LCP$stress.4days))
summary (empty_LCP <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep_LCP))
summary (one_LCP <- lmer (sleep.efficiency ~ 1 + scent.type + (1 + scent.type | participant), data=sleep_LCP))
summary (main_LCP <- lmer (sleep.efficiency ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + stress.4days.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep_LCP))

#run supplementary sleep efficiency analysis those unaware of scent order
summary (sleep_12 <- subset(sleep, sleep$correct.guess.shirt == 0))
sleep_12$stress.cen.12 <-sleep_12$stress-(tapply(sleep_12$stress, as.factor(sleep_12$participant), mean,na.rm=T) [as.factor(sleep_12$participant)])
summary (sleep_12$stress.cen.12)
summary (sleep_12$rel.sat.cen<-sleep_12$rel.sat-mean(sleep_12$rel.sat))
summary (sleep_12$rel.length.cen<-sleep_12$relation.length-mean(sleep_12$relation.length))
summary (sleep_12$AAQ_avoidance.cen<-sleep_12$AAQ_avoidance-mean(sleep_12$AAQ_avoidance))
summary (sleep_12$AAQ_ambivalence.cen<-sleep_12$AAQ_ambivalence-mean(sleep_12$AAQ_ambivalence))
summary (sleep_12$stress.4days.cen<-sleep_12$stress.4days-mean(sleep_12$stress.4days))
summary (empty_12 <- lmer (sleep.efficiency ~ 1 + (1 | participant), data=sleep_12))
summary (main_12 <- lmer (sleep.efficiency ~ 1 + scent.type + order + rel.length.cen + rel.sat.cen + AAQ_avoidance.cen + AAQ_ambivalence.cen + stress.cen + stress.4days.cen + birth.control + (1 + scent.type + stress.cen | participant), data=sleep_12))

#deviance reduction
anova(main_12, empty_12)