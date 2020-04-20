# Now we'll go over some useful functions associated with 
#  drawing normal distributions

# first, a rehashing of sequences and polygons
x = seq(-3,3, length=10)
print(x)

# now, lets try to understand a polygon function
# We'll use this to help us draw areas
# **POINT TO DOWNLOAD FUNCTION**

# lets make a triangle
# lets say the triangle goes from -3 to +3 in x & 
#  0-1 in y
#plot.new()
#plot.window(c(-3,3),c(0,1))
par(mfrow=c(1,1))
plot(NULL,xlim=c(-3,3),ylim=c(0,1)) # sets up axes
xvertices = c(-3, 0, 3)
yvertices = c(0, 1, 0)
polygon(xvertices, yvertices,col="red") # plots on top of previous plot

# lets try overplotting a little rectangle at 
#  x = (-1,1), y = (0.4,0.6)
xvertices = c(-1, -1, 1, 1)
yvertices = c(0.4, 0.6, 0.6, 0.4)
polygon(xvertices, yvertices, col="blue")

# essentially, polygon just fills in between a 
# list of verticies we give it
#  we can use this to plot underneath our normal
#  distributions.
#  This will help us get a "feel" for how much of the 
#  graph is related to our measurement of interest


#----------------------------------------------

# now, lets build some tool's we will need 
#  to examine normal distributions
#  (1) lets plot them using "dnorm"

# moving onto normal distributions
# first, lets start by plotting a normal distribution
#  like last time
help(dnorm) # to see list of normal distribution functions
x=seq(-3,3,length=200)
y=dnorm(x, mean=0, sd=1)
plot(x,y)

# center this around a different value
y = dnorm(x, mean=2, sd=1)
plot(x,y)

# but, of course with a new mean
# comes a new sequence to be able to plot it
x=seq(-2,4,length=200)
plot(x, dnorm(x,mean=1,sd=1))

# we can overplot a bunch of different normal distributions
x = seq(-10,10,length=200)
y1=dnorm(x,mean=0,sd=1)
y2 = dnorm(x,mean=0, sd=5)
plot(x,y1, type="l", ylim=c(0,2))
par(new=TRUE)
plot(x,y2,type='l',col='red',ylim=c(0,2))
y3=dnorm(x,mean=0,sd=0.25)
par(new=TRUE)
plot(x,y3,type='l',col='blue', ylim=c(0,2))

# (2) 2nd in our toolset is calculating
#  Z-scores: remember this is a measure of 
# how "far off" a score is from the mean

# so first, as is always a good example,
# lets plot!
x = seq(-6,6,length=200)
mean_dist = 1.0
sd_dist = 0.5
plot(x,dnorm(x,mean=mean_dist,sd=sd_dist),ylim=c(0,1.0))
# lets say I want the Zscore for x=2.5?
# first of course, lets plot!
abline(v=2.5,col="red")
# recall: Zscore = (observation - mean)/SD
Zscore = (2.5 - mean_dist)/sd_dist
print(Zscore)

# now, lets say I've got a 2nd distribution
# with mean = 0.5 and sd=2, is the 
# Zscore at x=2.5 higher or lower than 
# the first one?
# as always, lets start by plotting
mean_dist2 = 0.5
sd_dist2 = 2.0
par(new=TRUE)
plot(x,dnorm(x,mean=mean_dist2,sd=sd_dist2),col="blue",ylim=c(0,1.0))
# so, by eye we can see that the red line falls
# at a higher y-value on the blue, 2nd distribution
# this tells us at x=2.5, we are closer to the mean on the 
# 2nd distribution so we expect a lower Zscore, but lets find out!
Zscore2 = (2.5-mean_dist2)/sd_dist2
print(Zscore2)
# indeed 1 < 3

# Zscores allow us to in a sense "normalize" each normal 
# distribution to allow for comparisions between them
# for example, if these distributions were measuring a test
#  then a student that scored a 2.5 on both would have done 
#  better than the overall class distribution on the first test

# So, lets take a moment to try an example yourself:
# I am the manufacteror of rulers.  My rulers 
# should be 10cm long, but I am having issues:
#  on Run #1 I get rulers with a mean of 11cm and 
#  an SD of 2.0cm
#  on Run #2 I get rulers with a mean of 10 cm and 
#  an SD of 4.0 cm
# Q1: Which is the better run of my manufacturing 
# equiptment?  Note: could be differing answers!
#   **DISCUSS AFTER PLOTTING**
# Q2: in each run, pull out a ruler to see how off it is
#  in both runs, I pull out a 9cm ruler - how unusual 
#  is it for me to pull out a ruler of this size?
# make a plot showing this & guess
# then, calculate with a Zscore

# ANS, Q1:
# depends on your perspective, but lets look at this distributions
x = seq(5,15,length=200)
plot(x,dnorm(x,mean=11,sd=2)) # further out for run #1
par(new=TRUE)
plot(x,dnorm(x,mean=10,sd=4),col="blue")


# ANS, Q2:
# its more unusual in run #1 since we are 
# further then the mean
abline(v=9.0,col="red")
# calculation
Z1 = (9.0-11)/2.0 # -1.0
Z2 = (9.0-10)/4.0 # -0.25
print(c(Z1,Z2))

#### BACK TO SLIDES FOR PERCENTILES ######

#------------------------------------------------
#### NOW: PERCENTILES #####

source('/Users/jillnaiman/Downloads/plot_polygons.R')

# if we recall the sum under the curve of our 
# normal probability distribution has to == 1
# given the laws of probability, we can 
# associate every Zscore with a percentile

# lets go back to our toy example of something centered a 1
x = seq(-3,3,length=200)
y = dnorm(x,mean=0,sd=1)
plot(x,y,type='l')

# what is the Zscore for x=0?
Zscore = (0-0)/1 # = 0
# what is the percentile?  
#  i.e. what percent under the curve 
# corresponds to a Zscore of 0 here?
# first, lets plot with a polygon
# lets make sure we are only giving vertices
#  up to Zscore = 0
x2 = seq(-3, Zscore,length=100)
y2 = dnorm(x2, mean=0, sd=1)
#polygon(c(-3,x2,Zscore),c(0,y2,0),col="red") # plots on top of previous plot
draw_polygon(x2,y2)
# so, by eye we can see that its probably 1/2 or
#  a percentile of 50%
# but we can also get an exact number using "pnorm"
print(pnorm(Zscore,mean=0,sd=1))

# in fact, we can do this percentage-from-Zscore
# for any Zscore

# make a plot of the percentile associated with negative "X"
plot(x,y,type='l')
Zscore_of_interest = 2
x2 = seq(-3, Zscore_of_interest,length=100)
y2 = dnorm(x2, mean=0, sd=1)
#polygon(c(-3,x2,Zscore_of_interest),c(0,y2,0),col="red") # plots on top of previous plot
draw_polygon(x2,y2)
# print
print(pnorm(Zscore_of_interest,mean=0,sd=1))

# this is now showing results for the area < some Zscore
#  what about if we want to know the area for > some Zscore?

# example with lower.tail=FALSE
plot(x,y,type='l')
x2 = seq(Zscore_of_interest,3,length=100)
y2 = dnorm(x2,mean=0,sd=1)
#polygon(c(Zscore_of_interest,x2,3),c(0,y2,0),col="red")
draw_polygon(x2,y2)
print(1-pnorm(Zscore_of_interest,mean=0,sd=1))
print(pnorm(Zscore_of_interest,mean=0,sd=1,lower.tail=FALSE))

# NOTE: I can also ask for the percentage of the 
#  curve between 2 numbers
par(mfrow=c(1,1))
x = seq(-3,3,length=200)
y = dnorm(x,mean=0,sd=1)
plot(x,y,type='l')
# what percentage of the curve
#  is between -1 and 1?
# first, lets plot
x2 = seq(-1,1,length=100)
y2 = dnorm(x2,mean=0,sd=1)
#polygon(c(-1,x2,1),c(0,y2,0),col="red")
draw_polygon(x2,y2)
# for the calculation of percentage, 
# we have to subtract one from the other like so:
print(pnorm(1.0,mean=0,sd=1)-pnorm(-1.0,mean=0,sd=1))


# lets try with an example
# The mean for SAT scores is 1500 & the SD is 300 points
# The mean for ACT scores is 24.0 and the SD is 3.5
# A student gets 1750 on the SATs and 31 on the ACT
# Q1: Use plots to justify which the studen did better on
# Q2: Calculate each Zscore -> was your reasoning justified?
# Q3: Make plots showing the percentiles with the polygon function,
#     which looks like the larger percentile?
# Q4: Calculate each percentile => justified Q3?

# A1:
par(mfrow=c(1,2))
x=seq(800,2200,length=200)
plot(x,dnorm(x,mean=1500,sd=300),type='l')
abline(v=1750,col="blue")
x=seq(12,36,length=200)
plot(x,dnorm(x,mean=24,sd=3.5),type='l')
abline(v=31,col="red")
# by eye ACT Zscore > SAT Zscore
# A2:
# NOTE: here I'm assuming the max scores for each are effectively infinite
#  an ok approximation here
Z_sat = (1750-1500)/300
Z_act = (31-24)/3.5
print('SAT ACT')
print(c(Z_sat,Z_act))
# 0.833 < 2.0
# A3
par(mfrow=c(1,2))
x=seq(800,2200,length=200)
plot(x,dnorm(x,mean=1500,sd=300),type='l')
x2 = seq(800,1750,length=200)
y2 = dnorm(x2,mean=1500,sd=300)
#polygon(c(800,x2,1750),c(0,y2,0),col="red") # plots on top of previous plot
draw_polygon(x2,y2)

x=seq(12,36,length=200)
plot(x,dnorm(x,mean=24,sd=3.5),type='l')
x2 = seq(12,31,length=200)
y2 = dnorm(x2,mean=24,sd=3.5)
#polygon(c(12,x2,31),c(0,y2,0),col="red") # plots on top of previous plot
draw_polygon(x2,y2)
# A4:
print('SAT ACT - percentiles')
print(c(pnorm(1750,mean=1500,sd=300),pnorm(31,mean=24,sd=3.5)))
# much higher percentile for ACT

### BACK TO SLIDES FOR ZSCORES FROM PERCENTILES

# so, we've gotten percentiles from Zscores
# but lets to the opposite
x=seq(-3,3,length=200)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l")
# now we can use the function "qnorm" like 
#  we've been using dnorm and pnorm, but 
#  now it will give us the Zscore 
#  corresponding to a percentage
Zscore_40 = qnorm(0.40,mean=0,sd=1)

# lets plot this over our figure
x=seq(-3,Zscore_40,length=100)
y=dnorm(x,mean=0,sd=1)
#polygon(c(-3,x,Zscore_40),c(0,y,0),col="red")
draw_polygon(x,y)
text(-1,0.1,"0.40")
# and we can put a nice little arrow for fun
arrows(0.5,0.1,-0.2,0,length=.15)
text(0.75,0.12,Zscore_40)


