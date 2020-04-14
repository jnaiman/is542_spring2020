
###################
# Shrinkage Methods for parameter choice
###################

# Let's first check out some data we've worked with before: the ozone data:
inputData = read.csv("~/Downloads/ozone2.csv", stringsAsFactors=F)

head(inputData)
# Recall what we are looking at here.  We are looking at measurements of ozone levels (in mm) for 
#  different time of the year, and days of the week under different conditions.

# Here Temp_sandburg is the temperature at Sandburg Air Force Base
# temp_elmonte is a measurement in el monte (near LA), CA in C?
# The "inversion" things are just related to when there is a state change in a gas
#  (so like from gas to liquid)


# If we can remember all the way back to week 7?
#  (we were all so young and naive then)
#  we remember that we can make correlation plots!
require(psych)
pairs.panels(inputData, ellipse=F, 
             main="Scatterplot matrix Ozone Data", lm=T)
# ** click on *zoom* to pop out this plot **


# Look at the "ozone reading" column and go down to see what it may be related too.
# It looks like pressure_height, the temperature measurements, and maybe inversion info
#  are strong predictors of ozone levels.  Not related to things like day of week or month, 
#  some relation to month, but that could be colinear with temperature.

# This plot shows red-line linear fits.  Let's try using "shrinkage" methods
#   to select a subset of parameters.

#install.packages("glmnet") # install once
require(glmnet)

# To do this fit, we need to do a bit of formatting:
# Define the response variable, in this case we assume that is the ozone reading.
y = select(inputData, ozone_reading)
# formatting:
y = pull(y)

# all other variables are explainitory/predictor variables
x = select(inputData, -ozone_reading)
# formatting:
# recall: y = ozone reading, x = all other things (month, DOW, Humidity, temp, etc)
x = as.matrix(x)

# here we use glmnet to fit both ridge & lasso regressions
#  for ridge we set alpha = 0
#  for lasso, we set alpha = 1

# Fitting the model (Ridge: Alpha = 0)
# for glmnet
ridge.mod = glmnet(x, y, alpha = 0, standardize = TRUE)

# for plotting
#install.packages("plotmo")
require(plotmo)

plot_glmnet(ridge.mod)

# Let's take a moment to look at this plot.
# Recall from our equations, as Lambda -> 0, we get the cofficients from just a least squares fit.
# This is what we are seeing on the right of this plot: when lambda is small the absolute value of all 
#   the different slopes is the same as in a pure least squares fitting.
#  This is because lambda -> 0 there is no "penalty" for including another slope.

# As Lambda gets large -> we get the "null model" i.e. when all the
#   coefficients are zero and we get a model with ZERO predictors.

# We can also plot the lasso regression model.
lasso.mod = glmnet(x,y, alpha=1)

plot_glmnet(lasso.mod)
# Note in this case we have some parameters that get set to zero exactly.  

# To copy the ISL figures, we can also include the plot as
#   a function of the L1 norm pretty easily:
par(mfrow=c(1,2))
plot_glmnet(lasso.mod)
plot_glmnet(lasso.mod,xvar="norm")
# Note: the second one is just showing instead of lambda, the DOF in our model
#   from 0 up to all of the parameters, or 12 total input parameters.
# What is L1 here? This is discussed on page 219 and is the sum of all of the 
#  different slopes.  So when the sum of the slopes = 0 on the left of the right plot, we have 
#  a zero-parameter model - i.e. the DOF as shown on the top of the right plot is 0.

# From these plots we can see our intuition is pretty spot on.
# For the most "shrunk" models - i.e. when Lambda is large
#  we only have the two temperature predictors, then the humidty
#  and then the Month comes in
#  again, one might expect that the temperature measurments are
#  related, as is the month.

###### IN-PERSON CLASS #######
### IN GROUPS:
# 1. Repeat this excercise with the snow fall data from week 11:
snotel_s = read.csv("~/Downloads/snotel_s.csv")
snotel2 = snotel_s[,c(1:2,4:6,3)] #Reorders columns for nicer pairs.panel display
pairs.panels(snotel2[,-c(1:2)], ellipse=F,
             main="Scatterplot matrix of SNOTEL Data", lm=TRUE)

# what is "y" in this case?  what is "x"?  Then you can use the same
#  stuff we did before
#  What do the lasso and ridge regressions look like as a function of lambdas?
# hint: x = select(snotel2, -c(Snow.Depth,Station,ID))

# ANS 1:
ys = select(snotel2, Snow.Depth)
ys = pull(ys)

# all other variables are explainitory/predictor variables
xs = select(snotel2, -c(Snow.Depth,Station,ID))
xs = as.matrix(xs)

ridge.mod.s = glmnet(xs, ys, alpha = 0, standardize = TRUE)
par(mfrow=c(1,1))
plot_glmnet(ridge.mod.s)

lasso.mod.s = glmnet(xs,ys, alpha=1)

par(mfrow=c(1,2))
plot_glmnet(lasso.mod.s)
plot_glmnet(lasso.mod.s,xvar="norm")

# so, it looks like only eleivatrion alone makes for a good model!
#  otherwise, we can select either -> probably just max temp


####### ONTO CV OF finding BEST LAMDA ######


# So, now we have these plots that show how variables are 
#    selected vs this new parameter "Lambda"... so now what?
# How do we know which Lambda is the best?
# Back to our old friend Cross-Validation!

# There is actually a handy-dandy glmnet function for this!  Lucky us:

# Note: adapted from here: http://ricardoscr.github.io/how-to-use-ridge-and-lasso-in-r.html
#  can also check out: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
cv.lasso = cv.glmnet(x, y, alpha=1)
par(mfrow=c(1,1))
plot(cv.lasso)
# Ok, what just happened.  R just ran a CV for us, using the MSE as our "loss" measure.
#  lambda.min: value of lambda that gives minimum CV-measure, in this case the MSE - this is the smallest lambda
#  lambda.1se: largest value of lambda such that error is within 1 standard error of the minimum.
# On the top of this plot is the # of parameters for each model, the y-axis is the MSE.
# You can see the MSE increases if we have less than ~5 parameters - so our "best" model will have ~5 parameters 
#   needed to accuratly fit our data.

# One thing to note here, if we recall our pairs-panels plot:
pairs.panels(inputData, ellipse=F, 
             main="Scatterplot matrix Ozone Data", lm=T)
# We note that there are different "centers" and "normalizations" for each parameter.
# So months are "centered" at 6 and vary from 1-12, while days of the week are "centered" at 3.5 and vary 1-7.
# This means our slopes along each of these parameters have different normalizations.
# To avoid this we can basically re-center all parameters around a 0 center, and make all their ranges = 1.
cv.lasso = cv.glmnet(x, y, alpha=1, standardize=TRUE)
par(mfrow=c(1,1))
plot(cv.lasso)
# Now all the slopes are in a sense "weighted" in the same way.

# What is the best Lambda to choose?
# Let's plot a few options:
# (i) The *very* minimum could be 1 SE from where the Lambda that minimizes the MSE is:
minLambda = max(c(cv.lasso$lambda.min-cv.lasso$lambda.1se, 0.0)) # incase min < 0
# (ii) max is one SE in the other direction
maxLambda = cv.lasso$lambda.1se
outstr = paste("Lambda = ", toString(cv.lasso$lambda.min), ", interval =", 
               toString(minLambda), 
               " - ", toString(maxLambda))
print(outstr)
# We can use our MSE-minimizing lambda, OR, our 1SE lambda - our 1SE lambda will result in a 
#  model with the fewest parameters that still produces a low MSE and probably guards against
#  overfitting.

# Let's plot what parameters are in our "best" model
plot_glmnet(lasso.mod)
abline(v=log(cv.lasso$lambda.min))
abline(v=log(cv.lasso$lambda.1se), lty=2)
# We can see that we have things that don't intuatively make sense like day-of-week in our 
#  lambda.min model.  That coupled with the fact that is generally useful to use the lambda$1se
#  to avoid overfitting means we should probably take the lambda.1se model.

# Note: we can run the above a few times (the cv.lasso part) and 
#  see that we can get slightly different values for the min Lambda.

# Again, to grab the "best" model we can either take the 
#  minimum lambda, *or* we can assume that anything
#  within 1 STD is also an adaquite model, and 
#  then just take the Lambda + 1STD which will 
#  select the simpliest (fewest parameter) model

lasso.mod_best = glmnet(x,y, alpha=1, lambda=cv.lasso$lambda.1se)
# We could then use this model to make predictions (in a moment)

# We can also see these coefficients from the defaults of:
coef(cv.lasso)
#print(coef(cv.lasso))
# or
print(coef(lasso.mod_best))

# Where we see only a subset of the variables have coefficients

### IN-PERSON IN CLASS ######
### IN GROUPS:
#  1. use CV to pick the best lambda for the snotel data

############ PREDICTIONS ################


# We can predict something of course with
#predict(lasso.mod_best, NEWX)

# lets see how well we do!
#
#Month Day_of_month Day_of_week ozone_reading pressure_height Wind_speed Humidity
#1     1            1           4          3.01            5480          8 20.00000
#2     1            2           5          3.20            5660          6 48.41432
#3     1            3           6          2.70            5710          4 28.00000
#4     1            4           7          5.18            5700          3 37.00000
#5     1            5           1          5.34            5760          3 51.00000
#6     1            6           2          5.77            5720          4 69.00000
#Temperature_Sandburg Temperature_ElMonte Inversion_base_height Pressure_gradient
#1             37.78175            35.31509              5000.000               -15
#2             38.00000            45.79294              4060.589               -14
#3             40.00000            48.48006              2693.000               -25
#4             45.00000            49.19898               590.000               -24
#5             54.00000            45.32000              1450.000                25
#6             35.00000            49.64000              1568.000                15
#Inversion_temperature Visibility
#1              30.56000        200
#2              46.86914        300
#3              47.66000        250
#4              55.04000        100
#5              57.02000         60
#6              53.78000         60
#
# **** assuming Month, humidity, temp_S and temp_el, inversion_base_height

# check out which coeff with
#print(coef(lasso.mod_best))

# pick new Xvec
month = 12
humidity = 60 # units?
temp_S = 40
temp_E = 45
ibh = 1000
# # rest are zero, since they don't matter
dom = 0.0
dow = 0.0
ph = 0.0
ws = 0.0
pg = 0.0
it = 0.0
vis = 0.0

xvec = c(month, dom, dow, ph, ws, humidity, temp_S, temp_E, ibh, pg, it, vis)
#xvec = c(month, humidity, temp_S, temp_E, ibh)

myPrediction = predict(lasso.mod_best, rbind(xvec))


# lets put this new plot with just our new variables
par(mfrow = c(2,3))
plot(inputData$Month, inputData$ozone_reading)
points(month, myPrediction, col="red")

plot(inputData$Humidity, inputData$ozone_reading)
points(humidity, myPrediction, col="red")

plot(inputData$Temperature_Sandburg, inputData$ozone_reading)
points(temp_S, myPrediction, col="red")

plot(inputData$Temperature_ElMonte, inputData$ozone_reading)
points(temp_E, myPrediction, col="red")

plot(inputData$Inversion_base_height, inputData$ozone_reading)
points(ibh, myPrediction, col="red")

### IN-PERSON CLASS #######
### IN GROUPS: 
#  1. Make some predictions for snow fall!




##### ONLINE: ANOTHER EXAMPLE ####
# 1. Repeat this excercise with the snow fall data from week 11:
snotel_s = read.csv("~/Downloads/snotel_s.csv")
snotel2 = snotel_s[,c(1:2,4:6,3)] #Reorders columns for nicer pairs.panel display
pairs.panels(snotel2[,-c(1:2)], ellipse=F,
             main="Scatterplot matrix of SNOTEL Data", lm=TRUE)

# what is "y" in this case?  what is "x"?  
# HERE: y = snowdepth, x = other things besides station IDs and names

ys = select(snotel2, Snow.Depth)
ys = pull(ys)

# all other variables are explainitory/predictor variables
xs = select(snotel2, -c(Snow.Depth,Station,ID))
xs = as.matrix(xs)

ridge.mod.s = glmnet(xs, ys, alpha = 0, standardize = TRUE)
par(mfrow=c(1,1))
plot_glmnet(ridge.mod.s)

lasso.mod.s = glmnet(xs,ys, alpha=1)

par(mfrow=c(1,2))
plot_glmnet(lasso.mod.s)
plot_glmnet(lasso.mod.s,xvar="norm")

#  2. use CV to pick the best lambda for the snotel data

#  from before:

cv.lasso_s = cv.glmnet(xs, ys, alpha=1, standardize=TRUE)
par(mfrow=c(1,2))

plot(cv.lasso_s)

minLambda_s = cv.lasso_s$lambda.min+cv.lasso_s$lambda.1se

lasso.mod_best_snotel = glmnet(xs,ys, alpha=1, 
                               lambda=minLambda_s)
# plot
plot_glmnet(glmnet(xs,ys, alpha=1))
abline(v=log(minLambda_s))

print(coef(lasso.mod_best_snotel))
# so, as expected we can use just elevation!

