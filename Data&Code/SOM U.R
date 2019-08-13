#This document is code used to generate numbers reported in the 
#Supplemental Online Material (SOM-U) for
#The Scent of a Good Night’s Sleep: Olfactory Cues of a Romantic Partner Increase Sleep Efficiency

#Results from each model tested to reach final model
#Results of Interim Models for forward stepping methods & backward stepping method 

#stress at level 1 (daily level)
summary(stress <-lmer (sleep.efficiency ~ shirt + stress.2.cen*shirt +
											 	(1 + shirt| participant.id), data=data))
confint(stress, method="boot", nsim = 1000)

#create scent duration variable
data$scent.duration <- NA
data[which(data$night==1 | data$night==3), "scent.duration"] <- -1
data[which(data$night==2 | data$night==4), "scent.duration"] <- 1

#scent duration
summary (duration <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt +
							 	(1 | participant.id), data=data))
confint(duration, method="boot", nsim = 1000)

#weeknight 
summary(weeknight <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt + night_cen*shirt + 
														(1 + shirt| participant.id), data=data))
confint(weeknight, method="boot", nsim = 1000)

#level 2 variables
#control scent type
summary(control <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt + control*shirt 
											+	(1 + shirt| participant.id), data=data))
confint(control, method="boot", nsim = 1000)

#AAQ
summary(attachment <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt 
													 + AAQ_ambivalence_grandmean*shirt + AAQ_avoidance_grandmean*shirt 
													 + (1 + shirt| participant.id), data=data))
confint(attachment, method="boot", nsim = 1000)

#sex
summary(sex <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt  
											 	+ (1 + shirt| participant.id), data=data))
confint(sex, method="boot", nsim = 1000)

#relationship length
summary(RL <-lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt 
									 + rel.length_grandmean*shirt + (1 + shirt| participant.id), data=data))
confint(RL, method="boot", nsim = 1000)

#Relationship quality 
summary(RQ <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt
										+ RQ.overall_grandmean*shirt + (1 + shirt| participant.id), data=data))
confint(RQ, method="boot", nsim = 1000)

#order of scent (condition)
summary(order <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt
											 + condition*shirt + (1 + shirt| participant.id), data=data))
confint(order, method="boot", nsim = 1000)

#stress at level 2 (trait level)
summary(st <- lmer (sleep.efficiency ~ shirt + scent.duration*shirt + sex*shirt 
							+ stress.2.grandcen*shirt + (1 + shirt| participant.id), data=data))
confint(st, method="boot", nsim = 1000)

#clean up
rm(st, order, RQ, RL, sex, attachment, control, weeknight, duration, stress)

#all predictors simultaneously (backwards stepwise method)
summary(lmer (sleep.efficiency ~ shirt  + scent.duration*shirt + night +
										 +	sex*shirt + shirt*condition  + RQ.overall_grandmean*shirt 
										 + rel.length_grandmean*shirt + AAQ_ambivalence_grandmean*shirt 
										 + AAQ_avoidance_grandmean*shirt + control*shirt + stress.2.cen*shirt 
										 + stress.2.grandcen*shirt + (1 + shirt| participant.id), data=data))

#reduced model
summary(final <-lmer (sleep.efficiency ~ shirt  + scent.duration*shirt 
											+	sex*shirt + rel.length_grandmean*shirt + control*shirt  
										 + (1 + shirt| participant.id), data=data))
confint(final, method="boot", nsim = 1000)

#Results from each model tested to reach final percieved sleep quality model

#stress at level 1 (daily level)
summary(stress <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen*shirt +
											 	(1 + shirt| participant.id), data=data))
confint(stress, method="boot", nsim = 1000)

#scent duration
summary (duration <- lmer (perceived.sleep.quality ~ shirt + stress.2.cen 
													 + scent.duration*shirt + (1 | participant.id), data=data))
confint(duration, method="boot", nsim = 1000)

#weeknight 
summary(weeknight <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen 
													+ night_cen*shirt + (1 + shirt| participant.id), data=data))
confint(weeknight, method="boot", nsim = 1000)

#weeknight remove interation
summary(weeknight2 <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen 
													+ night_cen + (1 + shirt| participant.id), data=data))
confint(weeknight2, method="boot", nsim = 1000)

#level 2 variables
#control scent type
summary(control <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
												+ control*shirt +	(1 + shirt| participant.id), data=data))
confint(control, method="boot", nsim = 1000)

#AAQ
summary(attachment <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
													 + AAQ_ambivalence_grandmean*shirt + AAQ_avoidance_grandmean*shirt 
													 + (1 + shirt| participant.id), data=data))
confint(attachment, method="boot", nsim = 1000)

#sex
summary(sex <- lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
											 + sex*shirt + (1 + shirt| participant.id), data=data))
confint(sex, method="boot", nsim = 1000)

#relationship length
summary(RL <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
									 + rel.length_grandmean*shirt + (1 + shirt| participant.id), data=data))
confint(RL, method="boot", nsim = 1000)

#Relationship quality 
summary(RQ <- lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
										+ RQ.overall_grandmean*shirt + (1 + shirt| participant.id), data=data))
confint(RQ, method="boot", nsim = 1000)

#order of scent (condition)
summary(order <- lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen 
											 + condition*shirt + (1 + shirt| participant.id), data=data))
confint(order, method="boot", nsim = 1000)

#stress at level 2 (trait level)
summary(stress2 <-lmer (perceived.sleep.quality ~ shirt + stress.2.cen + night_cen
							+ stress.2.grandcen*shirt + (1 + shirt| participant.id), data=data))
confint(stress2, method="boot", nsim = 1000)

#clean up
rm(order, RQ, RL, sex, attachment, control, weeknight, duration, stress, stress2)

#all predictors simultanioulsy (backwards stepwise method)
summary(lmer (perceived.sleep.quality ~ shirt  + scent.age*shirt + 
										 	+	sex*shirt + shirt*condition  + RQ.overall_grandmean*shirt 
										 + rel.length_grandmean*shirt + AAQ_ambivalence_grandmean*shirt 
										 + AAQ_avoidance_grandmean*shirt + control*shirt + stress.2.cen*shirt 
										 + stress.2.grandcen*shirt + (1 + shirt| participant.id), data=data))

summary(final <-lmer (perceived.sleep.quality ~ shirt	+ shirt*condition   
										 + stress.2.cen + (1 + shirt| participant.id), data=data))
confint(final, method="boot", nsim = 1000)

#simple slope for order predicting sleep quality 
data$condition2 <- ifelse(data$condition == 0, 1, 0)
summary(condition2 <- lmer (perceived.sleep.quality ~ shirt + condition2 + condition2*shirt 
											+	(1 + shirt| participant.id), data=data))
confint(condition2, method="boot", nsim = 1000)
#clean up
rm(condition2, final)
#-----------------------------------------#

#Three Level Model (Couple as a Third Level)

#add couple ID
#sample 2 women have IDs in the 100's and men have IDs in the 200's but the second two digits match for couples
#Subtract 100 from men's IDs and their ID will match their female partner's ID
data$couple.id <- ifelse(data$participant.id > 199 & data$participant.id < 300, data$participant.id-100, 
												 data$participant.id)

#Sleep Efficiency predicted from scent type 
options(digits=10)
summary(lmer (sleep.efficiency ~ shirt +
								(1 + shirt | participant.id) + 	(1 | couple.id), data=data))
#ICC Calculation Sleep Efficiency
summary(lmer (sleep.efficiency ~ 1 +
								(1 + shirt | participant.id) + 	(1 | couple.id), data=data))
#ICC = random effect of intercept variance at level 3 / (random intercept variance at level 3 + random intercept variance at level 2 + residual)
#ICC is zero

#ICC calculation Sleep Quality
summary(lmer (perceived.sleep.quality ~ 
								(1 + shirt | participant.id) + (1 | couple.id), data=data))
#ICC = random effect of intercept variance at level 3 / (random intercept variance at level 3 + random intercept variance at level 2 + residual)
0.1593488/(0.1593488+0.1433518+0.8956357)
#ICC is .13
#-----------------------------------------#

#Results Removing Sample 1 Data 
#exclude sample 1 (sample 1 participants have two digit IDs)
nosample1 <- subset(data, participant.id>99)
#Sleep Efficiency predicted from scent type 
summary(no1 <- lmer (sleep.efficiency ~ shirt	+ (1 + shirt | participant.id), 
										 data=nosample1))
confint(no1, method="boot", nsim = 1000)
rm(no1)
#-----------------------------------------#

#Further Sleep Outcomes (Onset Latency & WASO)	
########## Onset Latency & WASO ##########
library(plyr)
d1 <- data %>%
	subset(select = c(participant.id, night.num, WASO)) %>%
	spread(night.num, WASO) %>%
	rename(c("1" = "WASO.s1", "2" = "WASO.s2",
					 "3" = "WASO.c1", "4" = "WASO.c2"))
d2 <- data %>%
	subset(select = c(participant.id, night.num, onset.latency)) %>%
	spread(night.num, onset.latency) %>%
	rename(c("1" = "onset.latency.s1", "2" = "onset.latency.s2",
					 "3" = "onset.latency.c1", "4" = "onset.latency.c2"))
data_components <- merge(d1,d2,by=c("participant.id"))
rm(d1,d2)

data_components$onset.latency.other <-rowMeans(data_components[,6:7], na.rm=TRUE)
data_components$onset.latency.partner <-rowMeans(data_components[,8:9], na.rm=TRUE)
colMeans(data_components[,10:11], na.rm=TRUE)
#preform one sample paired t-test
t.test(data_components$onset.latency.other, data_components$onset.latency.partner,
			 paired = TRUE)

data_components$WASO.other <-rowMeans(data_components[,2:3], na.rm=TRUE)
data_components$WASO.partner <-rowMeans(data_components[,4:5], na.rm=TRUE)
colMeans(data_components[,12:13], na.rm=TRUE)
#preform one sample paired t-test
t.test(data_components$WASO.other, data_components$WASO.partner,
			 paired = TRUE)
rm(data_components)

#HLM analysis for onset latency
summary (onset.latency <- lmer (onset.latency ~ shirt
																+ (1 + shirt| participant.id), data=data))
confint(onset.latency, method="boot", nsim = 1000)

##remove three extreme outliers
Upper <- mean(data$onset.latency, na.rm=TRUE)+6*sd(data$onset.latency,
																									 na.rm=TRUE)
data$onset.latency.nooutliers <- ifelse(data$onset.latency < Upper,
																				data$onset.latency, NA)

summary (onset.latency.nooutliers <- lmer (onset.latency.nooutliers ~ shirt
																					 + (1 + shirt| participant.id), data=data))
confint(onset.latency.nooutliers ,method="boot", nsim = 1000)

rm(onset.latency, onset.latency.nooutliers, Upper)

#HLM analysis for WASO
summary (WASO <- lmer (WASO ~ shirt + (1 + shirt| participant.id), data=data))
confint(WASO, method="boot", nsim = 1000)

##remove extreme outlier
Upper.WASO <- mean(data$WASO, na.rm=TRUE)+6*sd(data$WASO, na.rm=TRUE)
data$WASO.nooutliers <- ifelse(data$WASO < Upper.WASO,
															 data$WASO, NA)

summary (WASO.nooutliers <- lmer (WASO.nooutliers ~ shirt
																	+ (1 + shirt| participant.id), data=data))
confint(WASO.nooutliers, method="boot", nsim = 1000)
rm(WASO, WASO.nooutliers, Upper.WASO)
#-----------------------------------------#

#Distribution of Relationship Quality & Attachment Style Data
#boxplot for Relationship Quality
RQ <- data %>% 
	group_by(participant.id) %>%
	summarize(mean=mean(RS_overall))
sort(RQ$mean)
boxplot(RQ$mean, main="Relationship Quality", ylim=c(1,7))

#boxplot for Ambivalent Attachment
AmA <- data %>% 
	group_by(participant.id) %>%
	summarize(mean=mean(AAQ_ambivalence))
sort(AmA$mean)
boxplot(AmA$mean, main="Ambivalent Attachment", ylim=c(1,7))

#boxplot for Avoidant Attachment
AvA <- data %>% 
	group_by(participant.id) %>%
	summarize(mean=mean(AAQ_avoidance))
sort(AvA$mean)
boxplot(AvA$mean, main="Avoidant Attachment", ylim=c(1,7))
#-----------------------------------------#

#Belief Accuracy about Scent Exposure 
##Belief

##Participants’ beliefs about scent exposure (shirt: 0 = control, 1 = partner)

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

correct.guesses <- subset(data, belief == 1 & shirt == 1 | belief == 0 & shirt == 0)
table(correct.guesses$shirt)

#Belief/Sleep Efficiency Model
summary(model <- lmer (sleep.efficiency ~ shirt +
											 	(1 + shirt| participant.id), data=correct.guesses))
confint(model, method="boot", nsim = 1000)


#Belief/Perceived Sleep Quality Model
summary(model2 <- lmer (perceived.sleep.quality ~ shirt 
												+ (1 + shirt | participant.id), data=correct.guesses))
confint(model2,method="boot", nsim = 1000)
rm(correct.guesses, model, model2)

