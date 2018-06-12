# Mixed_models-
Using mixed models in R 

Chris Archer has been a premier starter for the Tampa Bay Rays since entering the big leagues. Beginning in 2014, Archer’s fielding independent pitching (FIP) has been below the 4.00 mark. 

With the advancement of MLB pitch tracking we can now estimate Archer’s allowed launch speed using pitch data from the 2018 season. First off, we will build a generalized linear regression and mixed regression model for estimating Archer’s launch speed based on his pitch type.

In 2018, Archer is throwing a four-seam fastball (46%), slider (43.9%), and changeup (10%) over 1233 pitches in 2018. He is currently on the disabled list with an abdominal injury. 

One interesting trend is that Archer has tended to pitch better later in the game this season at least for some advanced metrics. After facing the 20th hitter in the lineup, Archer’s allowed launch speed and hit distance drops 3.95 miles per hour (mph) and 5.6 feet on average. 

Traditional and Mixed Regression Models  

We first used a mixed regression model for predicting the launch speed (off of the bat) based on Archer’s release speed, release extension, balls, strikes, pitch type, and the pitch location coordinates (x and z coordinates of the pitch). 

Unlike a simple linear regression, mixed models allow us to account for pitch type specific effects. In the model, the (1|pitch_type) means that we are allowing the intercept, represented by 1, to vary by the pitch type

