#This document is code used to generate numbers reported in
#The Scent of a Good Night’s Sleep: Olfactory Cues of a Romantic Partner Increase Sleep Efficiency

#load packages
library(effsize)	
library(lme4)
library(lmerTest)
library(psych)
library(foreign)
library(tidyverse)

#set directory so source file location
#session -> set working directory -> to source file location
options(warn = -1)

#load package for data set information
#if it is run a second time --> restart R (session / restart R menu) (otherwise error message)
install.packages("sleepsmellpackage_1.0.tar.gz", repos = NULL, type="source")
library(sleepsmellpackage)

#Information on data used in analysis
?sleepdata

#Information on unused variables within dataset
?sleepdata_unused

#load data
data <- read.csv("sleepandsmelldata.csv")
data_wide <- read.csv("wide-sleepandsmell.csv")

#disable scientific notation
options(scipen=999)

#create scent duration variable
data$scent.duration <- NA
data[which(data$night==1 | data$night==3), "scent.duration"] <- -1
data[which(data$night==2 | data$night==4), "scent.duration"] <- 1

#center predictor variable creation
data$RQ.overall_grandmean <- I(data$RS_overall - mean(data$RS_overall))
data$rel.length_grandmean <- I(data$rel.length - mean(data$rel.length))
data$AAQ_ambivalence_grandmean <- I(data$AAQ_ambivalence - mean(data$AAQ_ambivalence))
data$AAQ_avoidance_grandmean <- I(data$AAQ_avoidance - mean(data$AAQ_avoidance))
data$intensity.odor_grandmean <- data$intensity.odor - mean(data$intensity.odor)
data$erotic.odor_grandmean <- data$erotic.odor - mean(data$erotic.odor)
data$pleasantness.odor_grandmean <- data$pleasantness.odor - mean(data$pleasantness.odor,na.rm=T)
data$night_cen <- data$night - 2.5

#stress cluster centered. 
data$stress.2.avg <- tapply(data$stress.2, as.factor(data$participant.id), mean,na.rm=T) [as.factor(data$participant.id)]
data$stress.2.grandcen <- data$stress.2.avg - mean(data$stress.2.avg,na.rm=T)
data$stress.2.cen <- data$stress.2 - data$stress.2.avg

##Participant Ethnicity Makeup
data %>% 
  dplyr::group_by(participant.id) %>%
  dplyr::summarise(ethnicity = mean(ethnicity)) %>% 
  dplyr::group_by(ethnicity) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(Percentages = (count/sum(count))*100)

#Participant Information
info <- data %>% 
	distinct(data$participant.id, .keep_all = TRUE) %>% 
	select(age, sleep.efficiency, perceived.sleep.quality, stress.2, rel.length, 
				 RS_overall, AAQ_avoidance, AAQ_ambivalence, sex, condition, 
				 correct.guess, control) %>% 
	summarise_all(funs(sd(., na.rm = TRUE))) %>% 
	gather()

Participant.Demographics <- data %>% 
	distinct(data$participant.id, .keep_all = TRUE) %>% 
	select(age, sleep.efficiency, perceived.sleep.quality, stress.2, rel.length, 
				 RS_overall, AAQ_avoidance, AAQ_ambivalence, sex, condition, 
				 correct.guess, control) %>% 
	summarise_all(funs(mean(., na.rm = TRUE))) %>% 
	gather(Variable, Mean) %>% 
	mutate(StandardDeviation = info$value)

View(Participant.Demographics)

#clean environment
rm(info)

#correlation of perceived sleep quality items
#accounting for dependant nature of the data
library(rmcorr)
rmcorr(participant.id, sleep.quality.SR, rested.SR, data)

#correlation of perceived stress items
rmcorr(participant.id, stress.today, stress.tomorrow, data)

#Mean stress by night
detach(package:plyr)
data %>% 
  group_by(night) %>%
  summarise(mean = mean(perceived.sleep.quality, na.rm= TRUE))

#ANOVAs to check if demographics significantly differ between samples (they don't)
Sample.Differences <- data %>%
  distinct(data$participant.id, .keep_all = TRUE) %>%
  select(age, stress.2, rel.length,
         RS_overall, AAQ_avoidance, AAQ_ambivalence, study, ethnicity)
#Make study a factor
Sample.Differences$study.2 <- factor(Sample.Differences$study, labels = c ( "one", "two", "three"))
#check for demographic differences between samples
newFunc <- function(x) {
  summary(aov(x ~ study.2, data = Sample.Differences))
}

sapply(Sample.Differences[1:6], FUN = newFunc)

#reduce ethnicity to 3 categories (1=white, 2=asian, 3=other)
Sample.Differences$eth.reduced <- ifelse(Sample.Differences$ethnicity==12, 1,
                              ifelse(Sample.Differences$ethnicity==1, 3,
                                     ifelse(Sample.Differences$ethnicity==8, 3, ifelse(Sample.Differences$ethnicity==13, 3, 2))))
Sample.Differences$eth.reduced
tbl = table(Sample.Differences$study, Sample.Differences$eth.reduced)
chisq.test(tbl)
#no significant differences between samples on ethnicity

#clean envirnment
rm(tbl, newFunc, Sample.Differences)

##Main Analyses Described in Results Section##

#Sleep Efficiency Means & SDs
colMeans(data_wide[,10:11], na.rm=TRUE)
sd(data_wide$sleep.efficiency.other, na.rm=TRUE)
sd(data_wide$sleep.efficiency.partner, na.rm=TRUE)

#Calculate Cohen's d 
cohen.d(data_wide$sleep.efficiency.partner, data_wide$sleep.efficiency.other, na.rm=TRUE, 
				pooled = TRUE, paried = TRUE)

#Initial Sleep Efficiency Model & 95% CI
summary(sleep.efficiency <- lmer (sleep.efficiency ~ shirt	+ (1 + shirt | participant.id), data=data))
confint(sleep.efficiency, method="boot", nsim = 1000)

#Final Sleep Efficiency Model & 95% CI
summary(final <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt 
											 + (1 + shirt| participant.id), data=data))
confint(final, method="boot", nsim = 1000)

#ICC 
summary(lmer (sleep.efficiency ~ 1 +
								(1| participant.id), data=data))
(19.55/(19.55+76.45))
#0.20

#exploring interactions
#means for sleep efficiency separated by scent duration & scent
data %>% group_by(shirt, scent.duration) %>% 
	summarize(mean=mean(sleep.efficiency, na.rm=T), sd=sd(sleep.efficiency, na.rm=T))

#simple slope for sleep efficiency by sex
summary(sex <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex + sex*shirt +
											(1 + shirt| participant.id), data=data))
confint(sex, method="boot", nsim = 1000)

data$sex.2 <- ifelse(data$sex == 0, 1, 0)
summary(sex2 <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex.2 + sex.2*shirt + 
												(1 + shirt| participant.id), data=data))
confint(sex2, method="boot", nsim = 1000)
rm(sex,sex2)

#means for sleep efficiency separated by sex & scent
data %>% group_by(shirt, sex) %>% 
	summarize(mean=mean(sleep.efficiency, na.rm=T))

#clean up environment
rm(sleep.efficiency,SEO,SEP,final)

#Perceived Sleep Quality Means & SDs
colMeans(data_wide[,12:13], na.rm=TRUE)
sd(data_wide$perceived.sleep.quality.other, na.rm=TRUE)
sd(data_wide$perceived.sleep.quality.partner, na.rm=TRUE)

#Calculate Cohen's d 
cohen.d(data_wide$perceived.sleep.quality.partner, data_wide$perceived.sleep.quality.other, 
				na.rm=TRUE, pooled = TRUE, paried = TRUE)

#Intial Perceived Sleep Quality Model & 95% CI
summary(perceived.sleep.quality <- lmer (perceived.sleep.quality ~ shirt 
                                      + (1 + shirt| participant.id), data=data))
confint(perceived.sleep.quality, method="boot", nsim = 1000)

#Table 1. HLM Predicting Perceived Sleep Quality from Scent Type, Perceived Stress, and Weeknight.
summary(perceived.sleep.quality.2 <- lmer (perceived.sleep.quality ~ shirt + night_cen + stress.2.cen + 
                (1 + shirt| participant.id), data=data))
confint(perceived.sleep.quality.2, method="boot", nsim = 1000)

#ICC perceived sleep
summary(lmer (perceived.sleep.quality ~ 1 +
							 	(1| participant.id), data=data))
(0.23/(0.95+0.23))
#0.19

#clean up environment
rm(SQO,SQP, perceived.sleep.quality, perceived.sleep.quality.2)

#Correlation between perceived sleep quality and sleep efficiency
#accounting for dependant nature of the data
library(rmcorr)
rmcorr(participant.id,sleep.efficiency, perceived.sleep.quality, data)

##Table 2. Participants’ beliefs about scent exposure (shirt: 0 = control, 1 = partner)

#Beliefs when participants were smelling their partner's shirt
data %>%
  select(belief, shirt) %>% 
  arrange(belief) %>% 
  group_by(belief) %>% 
  tally(shirt == 1)

#Beliefs when participants were not smelling their partner's shirt
data %>%
  select(belief, shirt) %>% 
  arrange(belief) %>% 
  group_by(belief) %>% 
  tally(shirt == 0)

#Sleep Efficiency predicted from scent type and belief
summary(lmer (sleep.efficiency ~ shirt + belief + shirt:belief +
                (1 + shirt + belief | participant.id), data=data))

#remove non-significant interaction
summary(belief <- lmer (sleep.efficiency ~ shirt + belief +
								(1 + shirt + belief | participant.id), data=data))
confint(belief, method="boot", nsim = 1000)

#Perceived Sleep Quality predicted from scent type and belief
summary(lmer (perceived.sleep.quality ~ shirt + belief + shirt:belief +
                (1 + shirt + belief | participant.id), data=data))

#remove non-significant interaction
summary(belief2 <- lmer (perceived.sleep.quality ~ shirt + belief +
								(1 + shirt + belief | participant.id), data=data))
confint(belief2, method="boot", nsim = 1000)

#clean up environment
rm(belief2,belief,curWarnings)
#-----------------------------------------#