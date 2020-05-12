# MLR fitting examples

################################
# INTRO
################################
par(mfrow=c(1,1))
# first, lets recall last week, our linear model for blood alcohol level vs. # 
#  of beers drank
# read in data
BB <- read.csv("http://www.math.montana.edu/courses/s217/documents/beersbac.csv")
#print(BB)

Beers = BB$Beers
BAC = BB$BAC
plot(Beers, BAC, pch=16, col=30)

# it might be useful to know the sex or weight or age of the subjects in the Beers vs BAC 
# study to account for more of the variation in the responses
#  then maybe we can say more with this dataset -> i.e. what is the 
#  predicted BAC for a woman of a certain age?

# so, lets make our models a little more complicated and see
#  what fun things we can learn with a more complex model!


################################
# MLR - Beginning
################################

#  we can think of a suite of characteristics of observations 
#  that might be related to a single response variable

#install.packages("psych") # run once
require(psych)

# An example of snow depths at some high elevation locations on a *single* day in April
# A random sample of n = 25 MT (montana) locations  were obtained from the 
# Natural Resources Conversation Service’s website 
# (http://www.wcc.nrcs.usda.gov/snotel/Montana/montana.html) a few years ago. 

# Information on the snow depth (Snow.Depth) in inches, daily Minimum 
# and Maximum Temperatures (Min.Temp and Max.Temp) in F
# and elevation of the site (Elevation) in feet. A snow science researcher 
# (or spring back-country skier) might be interested in understanding Snow depth as a 
# function of Minimum Temperature, Maximum Temperature, and Elevation. 
# One might assume that colder and higher places will have more snow, 
# but using just one of the predictor variables might leave out some important predictive information.

# so lets read in this dataset and take a look!

#snotel_s = read.csv("http://www.math.montana.edu/courses/s217/documents/snotel_s.csv")
snotel_s = read.csv("~/Downloads/snotel_s.csv")
snotel2 = snotel_s[,c(1:2,4:6,3)] #Reorders columns for nicer pairs.panel display
pairs.panels(snotel2[,-c(1:2)], ellipse=F,
             main="Scatterplot matrix of SNOTEL Data", lm=TRUE)
# the -c(1:2) takes out the ID & Station columns
#  these are just labels essentially so we don't want to plot them

# lm = TRUE fits lines instead of , I think, splines

# also, go over what the axis labels are
#  So max temp -> down is on the bottom
#  Min temp -> up is the top
#  elevation -> down is labels
#  snow depth -> up is the labels

#Adapted from the help page for pairs, pairs.panels shows a 
# scatter plot of matrices (SPLOM), with bivariate scatter 
# plots below the diagonal, histograms on the diagonal, and the 
# Pearson correlation (R values) above the diagonal. Useful for descriptive 
# statistics of small data sets. If lm=TRUE, linear regression fits 
# are shown for both y by x and x by y. Correlation ellipses are also shown. 
# Points may be given different colors depending upon some grouping 
# variable. Robust fitting is done using lowess or loess regression. 
# Confidence intervals of either the lm or loess are drawn if requested.

# NOTE: the Pearson coeff is what we have been calling the "R" variable
#  i.e. cov(x,y)

# NOTE: this is *not* R_adj^2, just the R2 cofficients for individual linear fits 
# of variables vs. other variables

# Ok, so looking at our data, we can see there are *many* strong linear looking
#  relationships!  neat!

# For example: temperature & elevation are correlated
#  this is not surprizing of course - it tends to get colder
#  if we go up in elevation.
# BUT: this creates a problem if we want to use both 
#  of them in a model to explain the snow depth
#  this is the issue of *multcollinearity* or *collinearity*

# Let's start by naively assuming we can model snow depth with
# one of the variables, and look at this more closely
par(mfrow=c(3,1))
m1 = lm(Snow.Depth~Elevation, data=snotel2)
m2 = lm(Snow.Depth~Min.Temp, data=snotel2)
m3 = lm(Snow.Depth~Max.Temp, data=snotel2)

plot(snotel2$Elevation, snotel2$Snow.Depth, xlab='Elevation in feet', ylab='Snow Depth in inches')
abline(m1, col="blue")

plot(snotel2$Min.Temp, snotel2$Snow.Depth, xlab='Min. Temp. in F', ylab='Snow Depth in inches')
abline(m2, col="red")

plot(snotel2$Max.Temp, snotel2$Snow.Depth, xlab='Max. Temp. in F', ylab='Snow Depth in inches')
abline(m3, col="green")

# so, naively, we could think that any one of these variables could be
# a good predictor of snow depth

# doing single linear fits to start with does give us a "feel" for our
# data - i.e. now snow depth changes with any one of these
# variables **TALK ABOUT THEM**

# we can also look at summaries of each of these fits
print(summary(m1))
print(summary(m2))
print(summary(m3))

# Looking at the pvalues for each of these slopes - it looks like any 
# of them are good predictors of snow depth, so if we had applied
# SLR naively we'd get "good" fits, but any SLR is leaving out 
# the information that would come from fitting with the 
# other parameters.

#  But, using all of them should, in theory, give us a better 
#  estimation of snow depth!  But lets find out!

# so how do we do that?  Well the call in R is relatively simple:
mfull = lm(Snow.Depth~Elevation+Min.Temp+Max.Temp, data=snotel2)
print(summary(mfull))

# so, lets look at some plots!
#install.packages("effects") # run once

# NOTE: you might not be able to install this, but it's not 
#  crucial, we can just look at it here and get a sense of 
#  what is going on
require(effects)
plot(allEffects(mfull), main="MLR model with Elev, Min & Max Temps")

# What does this "fanny" part tell us?
# Basically, its giving us a measure of how uncertain we can be in all
#   variables.
# Let's look at the high end of elevation -
#  if the elevation is high, then the snow depth will still be 
#  relatively high, even if the min & max daily temperature is high (or low)
#  this fanning out is effected by the collinarity.

# These plots are another way to visualize the results of the "summary", but again, 
#   not crucial.

# We can compare slopes between the SLR and MLR
# ***print out the summaries***
# we notice that while they are the same sign, they change in actual value
print(summary(m3))
print(summary(mfull))

##################################################
### BACK TO SLIDES ####
### COME BACK AFTER CONDITIONS ARE DISCUSSED ###
##################################################

par(mfrow=c(2,2))#, oma=c(0,0,2,0))
plot(mfull, sub.caption='Diagnostics for our MLR Fit') # may not need sub.caption!

# Let's apply some of our thinking from last lecture with SLR to look for interesting points.

# We see a little bit of a dip in the residuals vs Fitted plot
#   and also a bit in the qq-plot, but otherwise, we see that 
#   residuals look pretty normal too.

# Checking out the residuals vs. leverage
#   we see that there is possibly 1 leveraging outlier, point - point 9.
# We can print out this point to see what it is:
print(snotel2[c(9),])
# lets compare to what the result should be
print(mfull)
intercept = mfull$coefficients[1]
elevation = mfull$coefficients[2]
minTemp = mfull$coefficients[3]
maxTemp = mfull$coefficients[4]

# Let's grab the data:
outlier_9th = snotel2[c(9),]

# And also what would we predict with our model?
result_9th = intercept + elevation*outlier_9th$Elevation + 
  minTemp*outlier_9th$Min.Temp + maxTemp*outlier_9th$Max.Temp

# Let's calculate our residual by hand:
residual_9th = result_9th - outlier_9th$Snow.Depth
print(residual_9th)
# So we see that this particular point misses the snow depth by almost 30 inches!

#NOTE: in the HW you used the "predict" function to figure out new points
#  here, we are doing it by hand for illustritive purposes

# While we should be very very careful about removing outliers, let us 
#   proceed in good faith that this is indeed a wonky point and remove it & refit
mfull_take2 <- lm(Snow.Depth~Elevation+Min.Temp+Max.Temp, data=snotel2[-9,])
print(summary(mfull_take2))
# ok, lets take a look
plot(allEffects(mfull_take2), main="MLR model with NE Ent. Removed")

# Ok, now something super weird looking is happening:
# it looks like warmer temperatures are related to higher snow depths?
# This seems counter intuative.
# HOWEVER: what is going on is that this can be explained 
# by interpreting them as the estimated change in the 
# response for changes in temperature 
# *after we control for the impacts of elevation*.
# So: elevation, with its small p value, is controlling much
# of what is going on, but perhaps places where it is high, and 
#  its just snowed, the temperature is higher? or maybe 
# this is just a weird day.

# We already checked linearity with our every-by-everything plot
# but lets re-check to remind our selves of how linear things are
# we can even take out our 9th point
pairs.panels(snotel2[-9,-c(1:2)], ellipse=F,
             main="Scatterplot matrix of SNOTEL Data", lm=TRUE)

# As for collinearity - we already knew this was a bit of an 
# issue as we have very high correlations
# between -0.75 & -0.91

# lets look at this in a bit more detail with a pretty graph
#install.packages("corrplot") # run once
require(corrplot)
par(mfrow=c(1,1), oma=c(0,0,1,0))
corrplot.mixed(cor(snotel2[-9,-c(1:2)]), upper.col=c(1, "orange"),
               lower.col=c(1, "orange"))
round(cor(snotel2[-c(9,22),3:6]),2)

# Here the size of each dot is telling us how correlated one 
#   of the variables is with another and black means negative correlation
#   while orange means positive.
# There are very large and very small values of R, the corr. coeff
#  meaning, there are moderate to strong relationships.

# While there is nothing we can do about it at this stage, we can
# get an estimate for how big of a problem it is for 
# any given slope estimate using the "Varience Inflation Factor" - VIF.

# Essentially a VIF is a measure of how collinear a varialble is
#   so, large VIF is bad and the rule of thumb is that VIF > 5 or 10 is bad.

# Let's look at vifs of our last fit
#install.packages("car") # run once
require(car)
print(vif(mfull_take2))

# This shows the biggest problems for Elevation being collinear with min temp
#    which we can also see in our plot where min temp and elevation cross.

# We can also get a measure for how big the standard errors (SE) are 
#   because of collinarity:
print(sqrt(vif(mfull_take2)))

# This tells us that the SE for elevation is about 2.8 times LARGER 
#   than it should be because of multicollinairity with other variables
#   in the model.

# We can see this further by regressing elevation based on 
#   the other varaibles:
elev = lm(Elevation~Min.Temp+Max.Temp, data=snotel2[-9,])
print(summary(elev))

# We see our R2adj ~ 0.86, meaning that a high percentage 
#   of Elevation can be explained by a linear model using 
#   the min & max temperatures.

# We can also re-derive the VIF calculation from the 
#   output of this regression
print(vif(mfull_take2)[1])
vifEl = 1.0/(1.0-summary(elev)$r.squared)
print(vifEl)
# so I *think* we use R2 and not R2adj just as a basis of definition?

###########################################
### GO BACK TO DISCUSSING COLLINEARITY & MODEL SELECTION ####
###########################################


###########################################
# CLASS EXERCISE
###########################################

# Q1: Now that we know that elevation and max and min temp are related,
#     how do our fits change if we only regress on the max and min temp?
#     Or elevation and one of the temperatures?
#     Based on forward selection what would fits on 1 paramter look like?  Which one should we choose?
#     What about 2 parameters? (1 = elevation, 2= maxtemp, 3 = min temp)
# A1:
m_less <- lm(Snow.Depth~Min.Temp+Max.Temp, data=snotel2)
print(summary(m_less))

m_less2 <- lm(Snow.Depth~Elevation+Max.Temp, data=snotel2)
print(summary(m_less2))

# Q2: Redo the full fit analysis for high elevation data:
snotel3 = subset(snotel2, snotel2$Elevation < 6500)
# what changes?  Does this make sense with your intuition?
#  use pairs.panels & corrplot to visualize this
#
# A2:
pairs.panels(snotel3[,-c(1:2)], ellipse=F,
             main="High elevation SNOTEL Data", lm=TRUE)


par(mfrow=c(1,1), oma=c(0,0,1,0))
corrplot.mixed(cor(snotel3[,-c(1:2)]), upper.col=c(1, "orange"),
               lower.col=c(1, "orange"))


m_highEle <- lm(Snow.Depth~Elevation+Min.Temp+Max.Temp, data=snotel3)
print(summary(m_highEle))
#Call:
#  lm(formula = Snow.Depth ~ Elevation + Min.Temp + Max.Temp, data = snotel3)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-17.01 -11.13  -2.51   9.95  30.65 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)  89.342923 169.634414   0.527    0.610
#Elevation     0.004422   0.013319   0.332    0.747
#Min.Temp     -0.557673   3.124920  -0.178    0.862
#Max.Temp     -1.733770   1.041953  -1.664    0.127
#
#Residual standard error: 16.01 on 10 degrees of freedom
#Multiple R-squared:  0.368,	Adjusted R-squared:  0.1784 
#F-statistic: 1.941 on 3 and 10 DF,  p-value: 0.187

# compared to the full elevation plot, the adjusted R-sq is much smaller
