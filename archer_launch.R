##6/4/18
#chris archer's launch speed 
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(merTools)

archer=read.csv("archer.csv")

#transform variable types 
archer$plate_x=as.numeric(as.character(archer$plate_x))
archer$plate_z=as.numeric(as.character(archer$plate_z))
archer$launch_speed=as.numeric(as.character(archer$launch_speed))
archer$launch_angle=as.numeric(as.character(archer$launch_angle))  
archer$hit_distance_sc=as.numeric(as.character(archer$hit_distance_sc))
archer$release_speed=as.numeric(as.character(archer$release_speed))
archer$release_extension=as.numeric(as.character(archer$release_extension))
archer$release_spin_rate=as.numeric(as.character(archer$release_spin_rate))

#count pitch types 
count(archer,pitch_type)
ggplot(archer,aes(x=plate_x,y=plate_z))+geom_point() 

#i.pitch types
ggplot(archer,aes(x=plate_x,y=plate_z,colour=pitch_name))+geom_point() 

#ii.early vs. later in the game 
archer$at_bat_marker=ifelse(archer$at_bat_number<=20,1,0)
archer$at_bat_marker=as.factor(archer$at_bat_marker)

#launch speed 
archer1=subset(archer,select=c("pitch_type","at_bat_marker","launch_speed","launch_angle","hit_distance_sc"))
archer2=na.omit(archer1)

# how does archer do early vs. later in the game? 
archer_metrics=ddply(archer2,.(at_bat_marker),plyr::summarize,launch_speed=mean(launch_speed),
                     launch_angle=mean(launch_angle),hit_distance=mean(hit_distance_sc))

#launch angle and launch speed by pitch 
archer_pitch_eff=ddply(archer2,.(pitch_type),plyr::summarize,launch_speed=mean(launch_speed),
                       launch_angle=mean(launch_angle),hit_distance=mean(hit_distance_sc))

#iii.
ggplot(archer,aes(x=plate_x,y=plate_z,colour=launch_speed))+geom_point() 

#iv.
names(archer)
ggplot(archer,aes(x=plate_x,y=plate_z,colour=release_speed))+geom_point() 

#v. release extension by pitch type 
archer_sub=subset(archer,select=c("release_spin_rate","pitch_type"))

#fill in missing values(release ext. on column mean)
for(i in 1:ncol(archer_sub)){
  archer_sub[is.na(archer_sub[,i]), i] <- mean(archer_sub[,i], na.rm = TRUE)
}

rel_ext=ddply(archer_sub,.(pitch_type),plyr::summarize,rel_ext=mean(release_spin_rate))

#7.  
#predict launch speed
#glm 
regr_launch=glm(launch_speed~release_speed+release_extension+
                  stand+balls+strikes+plate_x+plate_z+release_spin_rate+ptich_type,data=archer)
AIC(regr_launch) #2529.25

#how does archer pitch to left vs. right-handed hitters?  
table(archer$pitch_type,archer$stand)

# Mixed Model 
#(1|pitch_type)--> model fits a linear model with a varying-intercept group effect 
#using the variable pitch type
regr_launch3=lmer(launch_speed~release_speed+release_extension+
                    balls+strikes+plate_x+plate_z+release_spin_rate+(1|pitch_type),
                  data=archer)

confint(regr_launch3)

#estimates of the random effects 
ranef(regr_launch3)$pitch_type %>% head(5)
coef(regr_launch3)$pitch_type %>% head(5)

#account for the uncertainty for each pitch type and its interval estimate 
predictInterval(regr_launch3)
REsim(regr_launch3) #mean, median, and sd of the random effects
plotREsim(REsim(regr_launch3)) #plot pitch type interval estimates 

#predictions (standard predictions vs. cluster specific predictions) 
predict_glm=predict(regr_launch)
predict_mixed=predict(regr_launch3)

#histograms (predicted launch speed)
hist(predict_glm,col="orange",main="Linear Regression Predicted Launch Speed",xlab="Launch Speed (mph)")
hist(predict_mixed,col="dark green",main="Mixed Model Predicted Launch Speed",xlab="Launch Speed (mph)")
