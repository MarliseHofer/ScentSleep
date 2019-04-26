#install.packages("metafor")
library(metafor)
#vignette("metafor") #to get the paper on this package
# info for how to use package including repeated measures data
#https://rdrr.io/cran/metafor/man/escalc.html

#to get numbers used below for internal meta analysis, see supplemental R file

#Figure 1 (sleep efficiency)
#input stats from each sample (Ns, correlations, means & SDs)
SE.meta<-data.frame(
	id = c("1","2","3"),
	name = c("Sample 1","Sample 2","Sample 3"),
	ni = c(37, 78, 40),
	ri = c(-0.07, 0.48,  0.43),
	m2i = c(84.67 , 85.45, 85.78),
	m1i = c(91.61, 86.92, 86.88),
	sd2i = c(10.41933, 10.97312, 8.777953),
	sd1i = c(6.118939, 5.971638, 6.79097)) 

SE.meta <- escalc(measure="MC", m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i, ni=ni,ri=ri,
							 data=SE.meta, slab=(paste(name)), 
							 ilab = cbind(SE.meta$ni, SE.meta$m2i, SE.meta$m1i))

#fixed meta-analysis
fixed<-rma(yi,vi,method="FE",data=SE.meta, weighted = TRUE, slab = paste(name))
summary(fixed)

#code to make Table 1
fixed %>% forest(cex = 0.9,  font = 2, xlim = c(-30, 28), 
          ilab = cbind(SE.meta$ni, SE.meta$m2i, SE.meta$m1i), 
			    ilab.xpos = c(-21, -15, -10))
op <- par(mar=c(8,2,0,3),font = 2)  
text(-30, 4.5, "Sample", pos = 4)
text(28, 4.5, "Mean Change [95% CI]", pos = 2)
text(c(-21, -15, -10), 4.5, c("N", "Control", "Partner"))
text(c(-12), 5.2, c("Mean Sleep Efficiency"))
par(op)
#zoom to see graph un-distorted

#random meta-analysis
random <- rma(yi,vi,method="ML",data=SE.meta, weighted = TRUE, slab = paste(name))
summary(random)

#clean environment
rm(op, random, fixed)

#Figure 2 (perceived sleep)
#input stats from each sample (Ns, correlations, means & SDs)
PS.meta<-data.frame(
	id = c("1","2","3"),
	name = c("Sample 1","Sample 2","Sample 3"),
	ni = c(36, 78, 40),
	ri = c(0.27, 0.37,  0.17),
	m2i = c(4.69, 4.59, 4.53),
	m1i = c(4.59, 4.73, 4.88),
	sd2i = c(0.9418352, 0.8523711, 0.8419824),
	sd1i = c(0.9451999, 0.7936632, 0.8510693))
	
PS.meta <- escalc(measure="MC", m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i, ni=ni,ri=ri,
							 data=PS.meta, slab=(paste(name)))

PS.fixed <-rma(yi,vi,method="FE",data=PS.meta, weighted = TRUE, slab = paste(name))
summary(PS.fixed)
PS.random <- rma(yi,vi,method="ML",data=PS.meta, weighted = TRUE, slab = paste(name))
summary(PS.random)

#Code to make Table 2
PS.fixed %>% forest(cex = 0.9,  font = 2, xlim = c(-3.7, 2.5), 
			 ilab = cbind(PS.meta$ni, PS.meta$m2i, PS.meta$m1i), 
			 ilab.xpos = c(-2.7, -2.1, -1.5))
op <- par(mar=c(5,5,5,5),font = 2)
text(-3.7, 4.5, "Sample", pos = 4)
text(2.5, 4.5, "Mean Change [95% CI]", pos = 2)
text(c(-2.7, -2.1, -1.5), 4.5, c("N", "Control", "Partner"))
text(c(-1.8), 5.2, c("Mean Perceived Sleep"))
par(op)
#zoom to view un-distorted

#clean environment
rm(op, PS.fixed, PS.random)
