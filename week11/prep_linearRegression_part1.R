par(mfrow=c(1,1))

# first, lets get a feel for how this works interactively
#install.packages("manipulate") # run once
#install.packages("tigerstats") # run once

# make sure all packages are loaded
require(tigerstats)
require(manipulate)
FindRegLine()

# *** play with the above for a bit, let the students do it ***

# note: this is essentially what the computer is doing when
# you ask it for a linear regression - its trying 
# numbers to get the right slope and intercept

#-----------------------------
# now, lets work with some real data

par(mfrow=c(1,1))
# BAC, in grams of alcohol per deciliter of blood
# study of the effects of beer consumption on blood alcohol levels
# A group of n=16 student volunteers at The Ohio State University drank a randomly assigned number of beers
#Thirty minutes later, a police officer measured their BAC.

#Your instincts, especially as well-educated college students with some chemistry knowledge, 
#should inform you about the direction of this relationship – that there is a positive relationship
#between Beers and BAC
# i.e. - if you drink more beer, you're gonna get more drunk

# BAC is usually what is quoted for blood alcohol level

# read in data
#BB <- read.csv("http://www.math.montana.edu/courses/s217/documents/beersbac.csv")
# OR:
BB = read.csv('/Users/jillnaiman/Downloads/beersbac.csv')
#print(BB)

Beers = BB$Beers
BAC = BB$BAC
plot(Beers, BAC, pch=16, col=30)
# note, can also do the following if you prefer:
plot(BAC~Beers, data=BB, pch=16, col=30)

# Let's first calculate the correlation coefficient to see how "linear" this thing is
R = cor(Beers,BAC)
print(R)
# so R ~ 0.9, so quite linear & > 0, so positive correlation

# ok!  Lets get to actually fitting a line to this relation
myLine = lm(formula = BAC ~ Beers, data = BB)
# NOTE: it depends on which is "X" and which is "Y" variable
#  the above statment implies that BAC depends on Beers
#  (beer is explanitory)
#  and not the other way around 
print(summary(myLine))

# We see a few things here: we see what the intercept is (Intercept): -0.012701
#   and what the slope is, labeled in the "Beers" column as 0.017964.
# There's also a few other numbers we'll get to in a little bit.

# Here the slope is saying that for every extra beer we drink,
#  on average, our Blood alcohol level increases by 0.018.

# We can easily plot this line using abline
abline(myLine,col="blue")

# Maybe we want to extract the coefficients and plot our own line somewhere.
# We can do this with:
b0 = myLine$coefficients[1] # intercept
b1 = myLine$coefficients[2] # slope

# Now, let's plot a line using these coefficients
x = seq(0, 100) # # of beers 
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', col="green")

# Now, you can see that we have here done the dangerous EXTRAPOLATION.
# We can, for example overplot where a fatal does of alcohol is:
abline(h=0.4, col="blue") # note, this is an average, a heavy drinker can get up to about 0.8
# So clearly, someone can't drink 100 beers in a limited time span
#  since they would likely be dead.
# Let's replot to about 20 beers and take a look:
x = seq(0,20)
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', col="green")

# Another thing we can note is that for 0 beers the BAC measurement is 
#   negative, i.e. there is a negative blood alcohol level projected.
# Of course, this doesn't make sense - at zero beers, there should be 
# about 0% blood alcohol level.

# Let's overplot our points again and take a look
points(Beers, BAC, pch=16, col=30)

# Looking at this we can see that out side of a few & ~10 beers
#   we are essentially extrapolating - so, we really can't
#   make predictions there.

# What if we want to know what the predicted BAC is for 
#   4.5 beers?  We can use our line information since we 
#   don't have an observation there to estimate this value.

# There are a few ways to do this, we can 
#  try making our own function:
myLineFunction = function(xInput){
  return(b0 + b1*xInput)
}

print(myLineFunction(4.5))

# we can check this on our graph and see this seems to be approximately
#  where we would expect
# **DO BY HAND ***


# We talked a bit about residuals and that they are how "off" each point is
#   from our line fit, but lets actually plot them:
myResiduals = resid(myLine)
plot(myResiduals)
# Plot where they would lie if it was a perfect linear fit:
abline(h=0.0, col="red")
# From this residual plot, we can see that there *might* be some 
#   underlying structure here, but it is hard to tell for sure.

# We can also check if our residuals are normal (one of our fit conditions)
qqnorm(myResiduals)
qqline(myResiduals)
# So, we can see that our residuals are nearly normal.

# We can also combine plots to look at all our
#  things of interest at once.

# Maybe we want to plot our line, the fit, and the residuals
#par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # this makes a 2x2 or 4 plots and makes 1 on the top row
x = seq(1, 10)
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', col="green", xlab="Number of Beers", ylab="BAC")
points(Beers, BAC, pch=16, col=30) # over plot observation points

plot(myLine, which=1) # Residuals plot
plot(myLine, which=2) # qq-norm plot

# change to: layout(matrix(c(2,2,1,3), 2, 2, byrow = TRUE)) to demonstrate

# btw: what are those numbers on the bottom 2 plots?  they just refer to the "labels"
#  of the points
print(Beers)
# you can see that the 3rd observation corresponds to the value 9, and the point labeled
#  3 in the QQ plot

# Finally, lets test the "goodness" of our fit by calculating R2
#  aka the amount of variation of our points from our fit line:
R2 = R*R
print(R2)
# This roughly means that about 80% of the variation in 
#  our points is explained by the fact that they are on a
#  line.

# *** some folks got errors for which=1 ***

#################################
# BACK TO R FOR OUTLIERS
#################################

# Ok, lets go back to some of our plots we made before:
par(mfrow=c(2,2))
x = seq(1, 10)
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', 
     col="green", xlab="Number of Beers", ylab="BAC",
     ylim=c(0,0.2))
points(Beers, BAC, pch=16, col=30) # over plot observation points
plot(myLine, which=1) # Residuals plot
plot(myLine, which=2) # qq-norm plot

# Now, lets add another plot to our meagurie
plot(myLine, which=4) # Outliers plot, Cooks distance
#plot(myLine, which=5) # residuals vs. leverage

# The cooks distance is a measure the influence that each point 
# has on the regression line. It also is a positive measure 
# with higher values suggesting more influence. 
# The rule of thumb is that Cook’s D values over 1.0 correspond 
# to clearly influential points, values over 0.5 have some 
# influence and values lower than 0.5 indicate points that are not 
# influential on the regression model slope coefficients

# There is a significant amount of math that goes into 
#  the calculation of cook's D that we are glazing over 
#  at the moment.  Its related to "cross validation" 
#  methods that we'll be discussing in a few weeks, so 
#  stay tuned for that!

# If you really wanna know:
# Di = ei^2/(p X MSE)*(hii/(1-hii)^2)
# where ei is the ith residual
# MSE is the Mean Square Error (sum of residuals)
# p is # of parameters (2 in this case)
# hii is the leverage: hii = d(yhati)/d(yi) where yhat is fitted, yi is measured

# So here, we see that one of our "3" beers is definitely
# a influential point
# --> this is the point at 9 Beers, near 0.2 BAC

# We can also plot this another way
par(mfrow=c(2,2))
x = seq(1, 10)
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', col="green", 
     xlab="Number of Beers", ylab="BAC",
     ylim=c(0,0.2))
points(Beers, BAC, pch=16, col=30) # over plot observation points
plot(myLine, which=1) # Residuals plot
plot(myLine, which=2) # qq-norm plot

# Now, lets add another plot to our meagurie
plot(myLine, which=5) # residuals vs. leverage
# again, high leverage *can* lead to a point being influential, 
# esp. if the cook's distance is > 1

# so this is giving us a sense of if the point with high leverage
# (cook's distance > 1) is influential and in which direction
# so here we see the upper value of "3" beers is creating leverage

# Let's work through a few examples
par(mfrow=c(2,1))
plot(x, myNewLine, type='l', col="green", 
     xlab="Number of Beers", ylab="BAC",
     ylim=c(0,0.2))
points(Beers, BAC, pch=16, col=30) # over plot observation points
plot(myLine, which=5) # residuals vs. leverage

# This again is our original plot.
# Now, lets add something at 10 beers to fake some outliers and see
#   how our pltos change
new_Beers = c(Beers, 10)
new_BAC = c(BAC, 0.01)
new_BB = data.frame(new_Beers,new_BAC)


# plot original l ine
plot(x, myNewLine, type='l', col="green", xlab="Number of Beers", ylab="BAC")

# construct new linear regression for new points
new_myLine = lm(formula = new_BAC ~ new_Beers, data = new_BB)
new_b0 = new_myLine$coefficients[1] # intercept
new_b1 = new_myLine$coefficients[2] # slope

# lets plot our new points
points(new_Beers, new_BAC, pch=16, col=30) # over plot observation points


# now, lets plot a line using these coefficients
new_myNewLine = new_b0 + new_b1*x
lines(x, new_myNewLine, col="red")

# so we can see, compared to our old slope and intercept, things have really changed!

# we can see this more quantitatively using our residuals vs. leverage plot
plot(new_myLine, which=5) # residuals vs. leverage

# now we can see that our new "added" 17th point is pulling the slope down

# we can do the same thing and incrase the slope
# ***go back and add points 10 & 0.5****

# Finally, we can see how our "goodness of fit" parameters
#  change with these different points
new_R = cor(new_Beers,new_BAC)
print("New R, R^2")
print(c(new_R, new_R*new_R))


#################################
# BACK TO R FOR GOODNESS OF FIT
#   INFERENCE STUFFS
#################################






