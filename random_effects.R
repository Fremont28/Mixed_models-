#random and fixed effects 
#libraries 
library(MASS)
library(lme4)
library(mlmRev)
library(agridat)
library(MCMCglmm)

#import dataset 
draft=read.csv("NBA draft class.csv")

#probability distribtuion that best fits sample data 
draft$BPM.1<-draft$BPM+1
qqp(draft$BPM.1,"norm")

#lognormal distn
qqp(draft$BPM.1,"lnorm")

#random and fixed effects 
draft$Draft.class<-as.factor(draft$Draft.class)

# 1|Group -random effect 
lmm<-lmer(BPM~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,REML=FALSE)
summary(lmm)
Anova(lmm)

#assumptions
#1. linearity 
plot_model<-plot(resid(lmm),draft$BPM)
plot_model 
#2. homogeneous of variance
lmm_resid<-residuals(lmm)

#creates new column with abs value of residuals 
abs_residuals<-abs(lmm_resid)

#squares the abs values of the residuals to provide more robust estimate
abs_residuals_sq<-lmm_resid^2
abs_residuals_sq<-as.data.frame(abs_residuals_sq)
draft1=draft[0:622,]
draft1=cbind(draft1,abs_residuals_sq)

#anova of the squared residuals
levene<-lm(draft1$abs_residuals_sq~draft1$College)
anova(levene) #the variance of the residuals is equal
plot_lmm<-plot(lmm)
plot_lmm
hist(draft$BPM)


#3. the residuals of the model are normally distributed 
#qq plots provide an estimation of where standarized residuals lie wrt to 
#normal qunaities.. strong deviation from the line suggests residuals are
#not normally distn
require("lattice")
qqmath(lmm,id=0.06) #id=outliers

#what if the assumption is not normal and violated---transform 
draft$BPM.1<-draft$BPM+1
draft$BPM_log<-log10(draft$BPM.1)
draft$BPM_log[!is.finite(draft$BPM_log)] <- 0

lmm1<-lmer(BPM_log~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,REML=FALSE)
summary(lmm1) 
Anova(lmm1)
hist(draft$BPM_log)
shapiro.test(draft$BPM)
qqmath(lmm1,id=0.05) #mejor than lmm qqplot 

#transform with natural log
draft$BPM_nat_log<-log(draft$BPM)
draft$BPM_nat_log[!is.finite(draft$BPM_nat_log)] <- 0
lmm2<-lmer(BPM_nat_log~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,
           REML=FALSE) #AIC 783.5 
summary(lmm2)
hist(draft$BPM_nat_log)
