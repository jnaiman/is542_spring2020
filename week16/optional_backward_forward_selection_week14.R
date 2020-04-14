
# reference: http://r-statistics.co/Model-Selection-in-R.html

par(mfrow=c(1,1))

# ok, lets read in some ozone data
inputData = read.csv("~/Downloads/ozone2.csv", stringsAsFactors=F)

# define the response variable, in this case 
#y = inputData['ozone_reading']  # Y variable, pretty sure this is in units of mm
# formatting
#y = pull(y)
y = select(inputData, ozone_reading)
# formatting
y = pull(y)

# all other variables are explainitory/predictor variables
#x = inputData[, !names(inputData) %in% "ozone_reading" ]  # X variables
x = select(inputData, -ozone_reading)

head(inputData)
head(x)
# so here Temp_sandburg is the temperature at Sandburg Air Force Base
# temp_elmonte is a measurement in el monte (near LA), CA in C?
# the "inversion" things are just related to when there is a state change in a gas
#  (so like from gas to liquid)

# if we can remember all the way back to week 11
#  (we were all so young and naive then)
#  we remember that we can make correlation plots!
require(psych)
pairs.panels(inputData, ellipse=F, 
             main="Scatterplot matrix Ozone Data", lm=T)
# ** click on *zoom* to pop out this plot **
# look at the "ozone reading" column and go down to see what it may be realated too
# looks like pressure_height, the temperature measurements, and maybe inversion info
#  are strong predictors of ozone levels

# neat!  Ok, lets actually do a fit and see
myfit = lm(ozone_reading ~ .-ozone_reading, data=inputData)
summary(myfit)

# so, it looks like indeed our intuition is right, though
#  Month seems to also play a role, however intuitively
#  this is because temperature tends to change with month
#  (you can see a sinusoidal change in temperature vs. month)

# now, lets tell R to perform a stepwise, backward selection
#  the "goodness of fit" stat is AIC
selectFit = step(myfit)
# In stepwise regression, we pass the full model to step function. 
# It iteratively searches the full scope of variables in backwards directions 
# by default, if scope is not given. 
# It performs multiple iteractions by droping one X variable at a time. 
# In each iteration, multiple models are built by dropping each of 
# the X variables at a time. The AIC of the models is also computed and the 
# model that yields the lowest AIC is retained for the next iteration.
# So, on the first loop, a model with p-1 parameters, then the one with the best AIC is stored
#  then the next loop models with the same p-1 paramters before are refitted dropping another param
#  and so on until the AIC stat doesn't change "much"

summary(selectFit)
# this is the backward selected model - as assumed, the temperature
#  parameters are all there, so is month

# there are many other similar methods for 
# selection, one more that is kind of nice is best subsets:
#install.packages("leaps") # run once
library(leaps)
regsubsetsObj = regsubsets(x=x ,y=y, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")  # regsubsets plot based on R-sq

# the above is essentially just a fancy way of parsing through different 
#  kinds of models that do or do not include certain parameters
#  think of it as a "shortlist" for the important variables in your model
#  for example, you could decide you want a cut off in Adjusted R2 of 0.7 ish
#  and then you see you can leave out all but
# Intercept, Month, pressure_height, Humidity, Temperature_Sandburg and Temperature_Elmonte
#  ***draw line with cursor at highest 0.7 point ***

# NOTE there are other things like leap methods that also perform 
#  parameter searches that we aren't going to look at right now

