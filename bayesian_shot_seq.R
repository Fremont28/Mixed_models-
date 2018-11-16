#10/24/18 15-16 nba pbp
library(plyr)
library(dplyr)

nba_pbp=read.csv("(10-20-2015)-(06-20-2016)-combined-stats.csv")
head(nba_pbp)
table(nba_pbp['type']) 
#shot attempts
shot_att=subset(nba_pbp,shot_distance>=0)

#lag shots
lag_shots=lag(shot_att$player,1)
lag_shots1=lag(shot_att$player,2)
lag_shots2=lag(shot_att$player,3)

lag_shots=as.data.frame(lag_shots)
lag_shots1=as.data.frame(lag_shots1)
lag_shots2=as.data.frame(lag_shots2)

colnames(lag_shots)="lag_shot_one"
colnames(lag_shots1)="lag_shot_two"
colnames(lag_shots2)="lag_shot_three"

shots=shot_att['player']

final_shots=cbind(shots,lag_shots,lag_shots1,lag_shots2)

#early touches in the shot clock
#player map? clustering similar player type/team types based on possessions??
#arcor candy --r cheatsheet 
library(stringr)
head(shot_att)

#time posessions 
shot_att$poss_time=substr(shot_att$play_length,start=6,stop=8)
shot_att$poss_time=as.numeric(shot_att$poss_time)

head(shot_att$description) 

#leggero por favor: https://stackoverflow.com/questions/39903376/if-column-contains-string-then-enter-value-for-that-row
#assists vs. no assists??

shot_att$assists<-ifelse(grepl("AST", shot_att$description), 1, 0)
shot_att$block<-ifelse(grepl("BLK", shot_att$description), 1, 0)
shot_att$make<-ifelse(grepl("MISS", shot_att$description), 0, 1)
shot_att$stl<-ifelse(grepl("STL", shot_att$description), 1, 0)

shot_att$make=as.factor(shot_att$make)
shot_att$converted_x=as.numeric(shot_att$converted_x)
shot_att$converted_y=as.numeric(shot_att$converted_y)
shot_att$original_x=as.numeric(shot_att$original_x)
shot_att$original_y=as.numeric(shot_att$original_y)



write.csv(shot_att,file="shot_att_16.csv")







write.csv(shot_att,file="shot_att_16.csv")


### shot attempts early in shot clock
early=subset(shot_att,poss_time<=5)
dim(early)


ggplot(early,aes(x=converted_x,y=converted_y,color=stl))+geom_point()

#10/29/18************** leggero: https://www.cyclismo.org/tutorial/R/confidence.html
##food science ideas???
#a. 95% confidence interval (time of possession?)
shot_att$poss_time[is.na(shot_att$poss_time)] <- median(shot_att$poss_time, na.rm=TRUE)

mean=mean(shot_att$poss_time)
sd=sd(shot_att$poss_time)
n=dim(shot_att)[1]
error=qnorm(0.975)*sd/sqrt(n)

left=mean-error
right=mean+error


#b. chi-sq test
library(MASS)
tbl=table(survey$Smoke,survey$Exer)
tbl 

#test whether the smoking issue is independent of exercise??
chisq.test(tbl)
#avoid warnings??
ctbl=cbind(tbl[,"Freq"],tbl[,"None"]+tbl[,"Some"])
chisq.test(ctbl)



#11/4/18-----
#fixed effects https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
library(car)
library(MASS)
library(lme4)
library(mlmRev)
library(agridat)
library(MCMCglmm)

str(survey)
head(survey) 

# This is so that distributions that must be non-zero can make sense of my
# data
survey$Pulse.t <- survey$Pulse + 1
qqp(survey$Pulse.t, "norm")

#lnorm means lognormal
qqp(survey$Pulse.t,"lnorm")

#try mixed model??
str(survey)

#*******
#test out differences entre sex and smoke (random effect)---does data fit a normal distn?
head(survey)

lmm<-lmer(Pulse~Sex+Age+(1|Smoke),data=survey,REML=FALSE) #smoke binary 0,1 
summary(lmm)
Anova(lmm) #for p-values --how confident we are that the effect of sex and smoke on pulse?? using wald test???

#dealing with failure to converge error??
nosexlmm<-lmer(Pulse~Age+(1|Smoke),data=survey,REML=FALSE)
noagelmm<-lmer(Pulse~Sex+(1|Smoke),data=survey,REML=FALSE)
nofixedlmm<-lmer(Pulse~1+(1|Smoke),data=survey,REML=FALSE)
anova(nosexlmm,noagelmm,nofixedlmm)

###non normal data???? (set the distibution to log-normal??)
PQL<-glmmPQL(Pulse.t~Sex+Smoke,~1|Smoke/Clap,family=gaussian(link="log"),
             data=survey,verbose=FALSE)

summary(PQL)

#*******
#what if response variable is less than 5 or una binary response variable??
library("mlmRev")
data(bdf,package="mlmRev")

bdf<-subset(bdf,select = c(schoolNR, Minority, ses, repeatgr))
bdf$repeatgr[bdf$repeatgr==2]<-1
str(bdf)

#GHQ (random effect is schoolNR)
GHQ<-glmer(repeatgr~Minority+ses+ses*Minority+(1|schoolNR),
           data=bdf,family=binomial(link="logit"),nAGQ=25)

summary(GHQ)


#Laplace
Laplace<-glmer(repeatgr~Minority+ses+ses*Minority+(1|schoolNR),
               data=bdf,family=binomial(link="logit"))

summary(Laplace)

#overdispersion function???
overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

library(agridat)



####
draft=read.csv("NBA draft class.csv")
head(draft)

install.packages("MCMCglmm")
library(MCMCglmm)

prior = list(R = list(V = 1, n = 0, fix = 1), G = list(G1 = list(V = 1, n = 1),
                                                       G2 = list(V = 1, n = 1), G3 = list(V = 1, n = 1), G4 = list(V = 1, n = 1),
                                                       G5 = list(V = 1, n = 1)))

set.seed(103)
MCMC<-MCMCglmm(VORP.0~1,random=~AST+FG.+AST.1+TRB.1+FT.,
               data=draft,family="categorical",prior=prior,verbose=FALSE)

summary(MCMC) #trb.1 higher contributor to VORP??

#know the data??--- 
library(ggplot2)
ggplot(draft,aes(x=PTS))+geom_density()+facet_wrap()

#plot and assess lmm fit?? random scatter??
plot(fitted(lmm),xlab="Fitted Values",ylab="Residuals")
abline(h=0,lty=2)
lines(smooth.spline(fitted(lmm),residuals(lmm)))

####11/12/18 

#what probability distribtuion best fits your data?
draft$BPM.1<-draft$BPM+1
qqp(draft$BPM.1,"norm")

#lognormal distn
qqp(draft$BPM.1,"lnorm")

#random and fixed effects (tufts)-praciamente 
head(draft)
draft$Draft.class<-as.factor(draft$Draft.class)

# 1|Group -random effect 
lmm<-lmer(BPM~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,REML=FALSE)
summary(lmm)
Anova(lmm)

#random effects
ranef(lmm)

#assumptions ********** read: https://ademos.people.uic.edu/Chapter18.html
#1. linearity 
plot_model<-plot(resid(lmm),draft$BPM)
plot_model 
#2. homogeneous of variance
#extracts the residuals 
lmm_resid<-residuals(lmm)

#creates new column with abs value of residuals?
abs_residuals<-abs(lmm_resid)

#squares the abs values of the residuals to provide more robust estimate?
abs_residuals_sq<-lmm_resid^2
abs_residuals_sq<-as.data.frame(abs_residuals_sq)

draft1=draft[0:622,]
draft1=cbind(draft1,abs_residuals_sq)

#anova of the squared residuals
levene<-lm(draft1$abs_residuals_sq~draft1$College)
anova(levene) #the variance of the residuals is equal and therefore the 
#assumption of homoscedacity is not met (levene and bartlett tests?)

plot_lmm<-plot(lmm)
plot_lmm

hist(draft$BPM)


#3. the residuals of the model are normally distributed 
#qq plots provide an estimation of where standarized residuals lie wrt to 
#normal qunaities.. feurte deviation from the line suggests residuals are
#not normally distn
require("lattice")
qqmath(lmm,id=0.06) #id=outliers

#what if the assumption is not normal and violated? --transform!!
draft$BPM.1<-draft$BPM+1
draft$BPM_log<-log10(draft$BPM.1)
# read: https://stackoverflow.com/questions/30990961/replace-inf-nan-and-na-values-with-zero-in-a-dataset-in-r
draft$BPM_log[!is.finite(draft$BPM_log)] <- 0

lmm1<-lmer(BPM_log~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,REML=FALSE)
summary(lmm1) #AIC=10
Anova(lmm1)
hist(draft$BPM_log)
shapiro.test(draft$BPM)

#qqplot of new model? (test for normality?)
qqmath(lmm1,id=0.05) #mejor than lmm qqplot 

#transform with natural log? ****
draft$BPM_nat_log<-log(draft$BPM)
draft$BPM_nat_log[!is.finite(draft$BPM_nat_log)] <- 0
lmm2<-lmer(BPM_nat_log~Draft.class+FG.+X3P.+TRB.1+AST.1+(1|College),data=draft,
           REML=FALSE) #AIC 783.5 

summary(lmm2)
hist(draft$BPM_nat_log)
#II. Dharma read: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#####nueva -simulates residuals 
install.packages("DHARMa")
library(DHARMa)

#calculating scaled residuals?
sim_output=simulateResiduals(fittedModel=lmm1,n=100)
sim_output$scaledResiduals
plot(sim_output)


#cafeine baseline?? 
###formal goodnes-of fit tests on the scaled residuals llm, llm1, llm2 
testUniformity(simulationOutput = sim_output) #a character string whether test if obs are "greater", "less", or "two-sided" compared to Ho 
testOverdispersion(simulationOutput = sim_output)
#simulate residuals 
sim_output=simulateResiduals(fittedModel=lmm1,refit=T)
sim_output 

#random effect simulations 

#overdispersion------ 
testData=createData(sampleSize = 500, overdispersion=2,family=poisson())
fittedModel=glmer(observedResponse~Environment1+(1|group),family="poisson",data=testData)
sim_output=simulateResiduals(fittedModel=fittedModel)
plot(sim_output)


#underdispersion (designer shoes??) **------
testData=createData(sampleSize = 500,intercept=0,fixedEffects = 2,
                    overdispersion=0)
  
fittedModel=glmer(observedResponse~Environment1+(1|group),
                  family="poisson",data=testData)

summary(fittedModel)
sim_output<-simulateResiduals(fittedModel=fittedModel)
plot(sim_output)





#11/12/18 asada??
#nba assist hot streak ------ 
head(shot_att)
head(final_shots)

shots_col=final_shots[,1]
shots_col=as.data.frame(shots_col)

#check for shots?
shot_streak<-apply(shots_col,1,function(x) any (x %in% "Andre Drummond"))


#11/15/18--schedule fatigue??
library(dplyr)
sch=read.csv("warriors_sch.csv")
oldnames=c("X.2","X.3")
newnames=c("Home","Result")
sch<-sch %>% rename_at(vars(oldnames),~newnames)

#convert date to numeric
sch$new_date=sch$Date
sch$month=sch$Date
sch$month=substr(sch$Date,start=6,stop=9)

#strip text 
sch$new_date=substr(sch$new_date,start=10,stop=12)
sch$new_date=as.numeric(gsub(",","",sch$new_date))

#combine dataframes
sch_new=subset(sch,select=c("new_date","month"))
sch_new=na.omit(sch_new)
sch_new=as.data.frame(sch_new)


#difference between dates

#October 
oct=sch_new[grep("Oct",sch_new$month),]
oct$lag_date=lag(oct$new_date,1)
oct$rest=oct$new_date-oct$lag_date

#November 
nov=sch_new[grep("Nov",sch_new$month),]
nov$lag_date=lag(nov$new_date,1)
nov$rest=nov$new_date-nov$lag_date

#December
dec=sch_new[grep("Dec",sch_new$month),]
dec$lag_date=lag(dec$new_date,1)
dec$rest=dec$new_date-dec$lag_date









































#test for over and underdispersion
testDispersion(sim_output)
















#--
eur_pol=read.csv("group_final_eur.csv")

#side by side boxplot (political)
library(ggplot2)
ggplot(eur_pol,aes(x=group,y=polintr))+geom_boxplot() #political interest 
ggplot(eur_pol,aes(x=group,y=trstprl))+geom_boxplot() #trust parliament 

#ANOVA signifcant differences between groups
levels(eur_pol$group)

#anova
res_aov=aov(poldcs~group,data=eur_pol)
summary(res_aov)
TukeyHSD(res_aov)



#new dataset
eur_pol1=read.csv("pol_final.csv")
head(eur_pol1)

ggplot(eur_pol1,aes(x=trstlgl,y=stfdem,color=country))+geom_point()+
  xlab("Trust Legislation")+ylab("Democracy Faith")

cols=c("LU")
plot1=ggplot(eur_pol1, aes(x = reorder(country, -trst_lgl_ratio), y = trst_lgl_ratio,fill=trst_lgl_ratio,
                     show.legend = FALSE)) +
  geom_bar(stat = "identity")+ylab("Legislative Trust Ratio")+xlab("")+
  ggtitle("Danes Trust Their Legislation the Most in Europe")+
  theme(plot.title = element_text(hjust = 0.5))
plot1+ guides(fill=FALSE)

#demographics 
dems=read.csv("dem.csv")
dems$diff=dems$uemp12m-dems$uempla #12 month vs. 7-day unemployment?
mean(dems$diff) 

plot2=ggplot(dems, aes(x=reorder(country, diff), y=diff)) +
  geom_bar(stat='identity') +
  coord_flip()+geom_hline(yintercept=8.73,color="red")+
  ylab("Difference")+xlab("")+ggtitle("There is a High Discrepancy Between \n Surveyed Short-Term and Long-Term Unemployment")+
  theme(plot.title = element_text(hjust = 0.5))              



#---------------------------
avg=read.csv("avg.csv")
avg1=avg[0:7,]

plot3=ggplot(avg1,aes(x=reorder(index, average), y=average))+geom_bar(stat="identity")+
  xlab("")+ylab("Average")

plot3+ theme(axis.text.x = element_text(angle = 45, hjust = 1))




