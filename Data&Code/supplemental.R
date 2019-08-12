#load packages
library(lme4)
library(lmerTest)
library(psych)
library(foreign)
library(tidyverse)
library(plyr)

#disable scientific notation
options(scipen=999)

#load data -- not necessary if already loaded for main paper's R file
data <- read.csv("sleepandsmelldata.csv")

#Information about the data (sleepandsmellpackage from main R file required)
?sleepdata

#Information on unused variables within dataset
?sleepdata_unused

#-------------------------#

#SOM-R

#Participant Info Study 1
data %>% 
  distinct(data$participant.id, .keep_all = TRUE) %>% 
  filter(study == 1) %>%
  select(rel.length, age) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))

##Ethnicity Makeup (Coding info in helpfile)
Sample1 <- subset(data, study == 1)
Sample1 %>% 
  distinct(participant.id, .keep_all = TRUE) %>%
  dplyr::group_by(participant.id) %>%
  dplyr::summarise(ethnicity = mean(ethnicity)) %>% 
  dplyr::group_by(ethnicity) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(Percentages = (count/sum(count))*100)

##Sample 1 dataset creation + results info
library(plyr)
d1 <- Sample1 %>% 
  select(participant.id, night.num, sleep.efficiency) %>%
  spread(night.num, sleep.efficiency) %>%
  rename(c("1" = "sleep.efficiency.s1", "2" = "sleep.efficiency.s2", 
           "3" = "sleep.efficiency.c1", "4" = "sleep.efficiency.c2"))

d2 <- Sample1 %>% 
  select(participant.id, night.num, perceived.sleep.quality) %>%
  spread(night.num, perceived.sleep.quality) %>%
  rename(c("1" = "perceived.sleep.quality.s1", "2" = "perceived.sleep.quality.s2", 
           "3" = "perceived.sleep.quality.c1", "4" = "perceived.sleep.quality.c2"))
data_wide_W1 <- merge(d1,d2,by=c("participant.id"))
rm(d1,d2,Sample1)

#sleep efficiency
data_wide_W1$sleep.efficiency.other <-rowMeans(data_wide_W1[,2:3], na.rm=TRUE)
data_wide_W1$sleep.efficiency.partner <-rowMeans(data_wide_W1[,4:5], na.rm=TRUE)
colMeans(data_wide_W1[,10:11], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W1$sleep.efficiency.other, data_wide_W1$sleep.efficiency.partner, 
       paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W1$sleep.efficiency.other, na.rm=TRUE)
sd(data_wide_W1$sleep.efficiency.partner, na.rm=TRUE)
corr.test(data_wide_W1$sleep.efficiency.partner, data_wide_W1$sleep.efficiency.other)

#perceived sleep quality
data_wide_W1$perceived.sleep.quality.other <-rowMeans(data_wide_W1[,6:7], na.rm=TRUE)
data_wide_W1$perceived.sleep.quality.partner <-rowMeans(data_wide_W1[,8:9], na.rm=TRUE)
colMeans(data_wide_W1[,12:13], na.rm=TRUE)

#one sample paired t-test
t.test(data_wide_W1$perceived.sleep.quality.other, data_wide_W1$perceived.sleep.quality.partner, 
       paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W1$perceived.sleep.quality.other, na.rm=TRUE)
sd(data_wide_W1$perceived.sleep.quality.partner, na.rm=TRUE)
corr.test(data_wide_W1$perceived.sleep.quality.partner, data_wide_W1$perceived.sleep.quality.other)

#-------------------------#
#Sample 2

#Participant Info
data %>% 
  distinct(data$participant.id, .keep_all = TRUE) %>% 
  filter(study == 2) %>%
  select(rel.length, age) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))

##Ethnicity Makeup (Coding info in helpfile)
Sample2 <- subset(data, study == 2)
Sample2 %>% 
  distinct(participant.id, .keep_all = TRUE) %>%
  dplyr::group_by(participant.id) %>%
  dplyr::summarise(ethnicity = mean(ethnicity)) %>% 
  dplyr::group_by(ethnicity) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(Percentages = (count/sum(count))*100)

##Sample 2 dataset creation + results info
d3 <- Sample2 %>% 
  select(participant.id, night.num, sleep.efficiency) %>%
  spread(night.num, sleep.efficiency) %>%
  rename(c("1" = "sleep.efficiency.s1", "2" = "sleep.efficiency.s2", 
           "3" = "sleep.efficiency.c1", "4" = "sleep.efficiency.c2"))
d4 <- Sample2 %>% 
  select(participant.id, night.num, perceived.sleep.quality) %>%
  spread(night.num, perceived.sleep.quality) %>%
  rename(c("1" = "perceived.sleep.quality.s1", "2" = "perceived.sleep.quality.s2", 
           "3" = "perceived.sleep.quality.c1", "4" = "perceived.sleep.quality.c2"))
data_wide_W2 <- merge(d3,d4,by=c("participant.id"))
rm(d3,d4,Sample2)

#sleep efficiency
data_wide_W2$sleep.efficiency.other <-rowMeans(data_wide_W2[,2:3], na.rm=TRUE)
data_wide_W2$sleep.efficiency.partner <-rowMeans(data_wide_W2[,4:5], na.rm=TRUE)
colMeans(data_wide_W2[,10:11], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2$sleep.efficiency.other, data_wide_W2$sleep.efficiency.partner, 
			 alternative = c("less"), paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W2$sleep.efficiency.other, na.rm=TRUE)
sd(data_wide_W2$sleep.efficiency.partner, na.rm=TRUE)
corr.test(data_wide_W2$sleep.efficiency.partner, data_wide_W2$sleep.efficiency.other)

#perceived sleep quality
data_wide_W2$perceived.sleep.quality.other <-rowMeans(data_wide_W2[,6:7], na.rm=TRUE)
data_wide_W2$perceived.sleep.quality.partner <-rowMeans(data_wide_W2[,8:9], na.rm=TRUE)
colMeans(data_wide_W2[,12:13], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2$perceived.sleep.quality.other, data_wide_W2$perceived.sleep.quality.partner, 
       paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W2$perceived.sleep.quality.other, na.rm=TRUE)
sd(data_wide_W2$perceived.sleep.quality.partner, na.rm=TRUE)
corr.test(data_wide_W2$perceived.sleep.quality.partner, data_wide_W2$perceived.sleep.quality.other)

##Sample 2 Info - Women Only
subsetW2W <- subset(data, study == 2 & sex == 0)
d5 <- subsetW2W %>% 
  select(participant.id, night.num, sleep.efficiency) %>%
  spread(night.num, sleep.efficiency) %>%
  rename(c("1" = "sleep.efficiency.s1", "2" = "sleep.efficiency.s2", 
           "3" = "sleep.efficiency.c1", "4" = "sleep.efficiency.c2"))
d6 <- subsetW2W %>% 
  select(participant.id, night.num, perceived.sleep.quality) %>%
  spread(night.num, perceived.sleep.quality) %>%
  rename(c("1" = "perceived.sleep.quality.s1", "2" = "perceived.sleep.quality.s2", 
           "3" = "perceived.sleep.quality.c1", "4" = "perceived.sleep.quality.c2"))
data_wide_W2W <- merge(d5,d6,by=c("participant.id"))
rm(d5,d6,subsetW2W)
#sleep efficiency
data_wide_W2W$sleep.efficiency.other <-rowMeans(data_wide_W2W[,2:3], na.rm=TRUE)
data_wide_W2W$sleep.efficiency.partner <-rowMeans(data_wide_W2W[,4:5], na.rm=TRUE)
colMeans(data_wide_W2W[,10:11], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2W$sleep.efficiency.other, data_wide_W2W$sleep.efficiency.partner, 
       alternative = c("less"), paired = TRUE)

#perceived sleep quality
data_wide_W2W$perceived.sleep.quality.other <-rowMeans(data_wide_W2W[,6:7], na.rm=TRUE)
data_wide_W2W$perceived.sleep.quality.partner <-rowMeans(data_wide_W2W[,8:9], na.rm=TRUE)
colMeans(data_wide_W2W[,12:13], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2W$perceived.sleep.quality.other, data_wide_W2W$perceived.sleep.quality.partner, 
       paired = TRUE)

##Sample 2 Info - Men Only
subsetW2M <- subset(data, study == 2 & sex == 1)
d7 <- subsetW2M %>% 
  subset(select = c(participant.id, night.num, sleep.efficiency)) %>%
  spread(night.num, sleep.efficiency) %>%
  rename(c("1" = "sleep.efficiency.s1", "2" = "sleep.efficiency.s2", 
           "3" = "sleep.efficiency.c1", "4" = "sleep.efficiency.c2"))
d8 <- subsetW2M %>% 
  subset(select = c(participant.id, night.num, perceived.sleep.quality)) %>%
  spread(night.num, perceived.sleep.quality) %>%
  rename(c("1" = "perceived.sleep.quality.s1", "2" = "perceived.sleep.quality.s2", 
           "3" = "perceived.sleep.quality.c1", "4" = "perceived.sleep.quality.c2"))
data_wide_W2M <- merge(d7,d8,by=c("participant.id"))
rm(d7,d8,subsetW2M)

#sleep efficiency
data_wide_W2M$sleep.efficiency.other <-rowMeans(data_wide_W2M[,2:3], na.rm=TRUE)
data_wide_W2M$sleep.efficiency.partner <-rowMeans(data_wide_W2M[,4:5], na.rm=TRUE)
colMeans(data_wide_W2M[,10:11], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2M$sleep.efficiency.other, data_wide_W2M$sleep.efficiency.partner, 
       paired = TRUE)

#perceived sleep quality
data_wide_W2M$perceived.sleep.quality.other <-rowMeans(data_wide_W2M[,6:7], na.rm=TRUE)
data_wide_W2M$perceived.sleep.quality.partner <-rowMeans(data_wide_W2M[,8:9], na.rm=TRUE)
colMeans(data_wide_W2M[,12:13], na.rm=TRUE)
#one sample paired t-test
t.test(data_wide_W2M$perceived.sleep.quality.other, data_wide_W2M$perceived.sleep.quality.partner, 
       paired = TRUE)

#-------------------------#
#Sample 3

#Participant Info
data %>% 
  distinct(data$participant.id, .keep_all = TRUE) %>% 
  filter(study == 3) %>%
  select(rel.length, age) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))

##Ethnicity Makeup (Coding info in helpfile)
Sample3 <- subset(data, study == 3)
Sample3 %>% 
  distinct(participant.id, .keep_all = TRUE) %>%
  dplyr::group_by(participant.id) %>%
  dplyr::summarise(ethnicity = mean(ethnicity)) %>% 
  dplyr::group_by(ethnicity) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(Percentages = (count/sum(count))*100)

#Sample 3 dataset creation + results info
d9 <- Sample3 %>% 
  select(participant.id, night.num, sleep.efficiency) %>%
  spread(night.num, sleep.efficiency) %>%
  rename(c("1" = "sleep.efficiency.s1", "2" = "sleep.efficiency.s2", 
           "3" = "sleep.efficiency.c1", "4" = "sleep.efficiency.c2"))
d10 <- Sample3 %>% 
  select(participant.id, night.num, perceived.sleep.quality) %>%
  spread(night.num, perceived.sleep.quality) %>%
  rename(c("1" = "perceived.sleep.quality.s1", "2" = "perceived.sleep.quality.s2", 
           "3" = "perceived.sleep.quality.c1", "4" = "perceived.sleep.quality.c2"))
data_wide_W3 <- merge(d9,d10,by=c("participant.id"))
rm(d9,d10)

data_wide_W3$sleep.efficiency.other <-rowMeans(data_wide_W3[,2:3], na.rm=TRUE)
data_wide_W3$sleep.efficiency.partner <-rowMeans(data_wide_W3[,4:5], na.rm=TRUE)
colMeans(data_wide_W3[,10:11], na.rm=TRUE)
#preform one sample paired t-test
t.test(data_wide_W3$sleep.efficiency.other, data_wide_W3$sleep.efficiency.partner, 
       alternative = c("less"), paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W3$sleep.efficiency.other, na.rm=TRUE)
sd(data_wide_W3$sleep.efficiency.partner, na.rm=TRUE)
corr.test(data_wide_W3$sleep.efficiency.partner, data_wide_W3$sleep.efficiency.other)


data_wide_W3$perceived.sleep.quality.other <-rowMeans(data_wide_W3[,6:7], na.rm=TRUE)
data_wide_W3$perceived.sleep.quality.partner <-rowMeans(data_wide_W3[,8:9], na.rm=TRUE)
colMeans(data_wide_W3[,12:13], na.rm=TRUE)
#preform one sample paired t-test
t.test(data_wide_W3$perceived.sleep.quality.other, data_wide_W3$perceived.sleep.quality.partner, 
       paired = TRUE)

#sd & correlation for meta-analysis
sd(data_wide_W3$perceived.sleep.quality.other, na.rm=TRUE)
sd(data_wide_W3$perceived.sleep.quality.partner, na.rm=TRUE)
corr.test(data_wide_W3$perceived.sleep.quality.partner, data_wide_W3$perceived.sleep.quality.other)

#remove individual study datasets
rm(data_wide_W1, data_wide_W2, data_wide_W2M, data_wide_W2W, data_wide_W3)

#Interaction with Pleasantness in Sample 3
Sample3$pleasantness.odor_grandmean <- Sample3$pleasantness.odor - 
  mean(Sample3$pleasantness.odor,na.rm=T)
summary(pleasant <- lmer(sleep.efficiency ~ shirt + pleasantness.odor_grandmean + 
												 	pleasantness.odor_grandmean*shirt + (1 + shirt|participant.id), data=Sample3))
#CI
confint(pleasant, method="boot", nsim = 1000)

#simple slope analysis with partner scent set to 0
Sample3$shirt.2 <- ifelse(Sample3$shirt == 0, 1, 0)
summary(pleasant2 <- lmer(sleep.efficiency ~ shirt.2 + pleasantness.odor_grandmean +
               pleasantness.odor_grandmean*shirt.2 + (1 + shirt.2| participant.id),
             data=Sample3))
#CI
confint(pleasant2, method="boot", nsim = 1000)

rm(pleasant, pleasant2, Sample3)

#-----------------------------------------#
#Combined Data - Relaxed Threshold
##Sleep Efficiency

#Table S2
summary(sex <-lmer (sleep.efficiency ~ shirt + sex + sex*shirt +
                      (1 + shirt| participant.id), data=data))
confint(sex, method="boot", nsim = 1000)

#ICC 
summary(lmer (sleep.efficiency ~ 1 +
								(1| participant.id), data=data))
(19.55/(19.55+76.45))
#0.20

#simple slope for sleep efficiency in supplemental
data$sex.2 <- ifelse(data$sex == 0, 1, 0)
summary(sex2 <- lmer (sleep.efficiency ~ shirt + sex.2 + sex.2*shirt + 
                        (1 + shirt| participant.id), data=data))
confint(sex2, method="boot", nsim = 1000)
rm(sex,sex2)

