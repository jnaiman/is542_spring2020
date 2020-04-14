
### PLOTTING NORMAL DISTRIBUTIONS ######

# Let's plot a few normal distributions using "dnorm"
x = seq(-3,3,length=200) # plotting normal dist. -3,3 SD
y1 = dnorm(x, mean=0, sd=1)
plot(x,y1, type='l', ylim=c(0,2), ylab='Normal Distributions')

# overplot a few other normal distributions
y2 = dnorm(x, mean=0, sd=0.5)
par(new=TRUE) # for overplotting
plot(x, y2, type='l', col='red', ylim=c(0,2), ylab="")

# Let's add to this by visualizing a Z-score and 
#  actually calculating it as well.  We'll go back to just one
#  normal distribution.

#  Z-scores: remember this is a measure of 
# how "far off" a score is from the mean

# so first, as is always a good example,
# lets plot!
x = seq(-6,6,length=200)
mean_dist = 1.0
sd_dist = 0.5
# note here: I'm calling the dnorm function directly in the "y" data position of this function
#  this is instead of doing "y = dnorm..."
# Its just us being fancy :)
plot(x,dnorm(x,mean=mean_dist,sd=sd_dist),ylim=c(0,1.0), type='l')

# lets say I want the Zscore for x=2.5 - i.e. given this normal distribution, 
#  if I measure pick out an observation that is at the value of 2.5, how off from 
#  the mean is it?
# First of course, lets plot!
abline(v=2.5,col="red")
# we can see already that its pretty far off from the mean here -> if we by eye
#  try to compare the area to the right of this line (the little tail) it is 
#  very small compared to the area to the left - so we expect our Z-score to be 
#  pretty big!

# Now let's actually calculate:
# recall: Zscore = (observation - mean)/SD
Zscore = (2.5 - mean_dist)/sd_dist
print(Zscore)
# this is saying our measurement of 2.5 is 3 times bigger than the standard deviation of 
#  our normal distribution.
#  So pretty gosh-darn big!

# Now, lets say I've got a 2nd distribution
# with mean = 0.5 and sd=2, 
#  is the Zscore at x=2.5 higher or lower than the first one?

# As always, lets start by plotting
mean_dist2 = 0.5
sd_dist2 = 2.0
par(new=TRUE) # overplot on our original axis
plot(x,dnorm(x,mean=mean_dist2,sd=sd_dist2),col="blue",ylim=c(0,1.0), type='l')

# By eye we can see that the red line falls
# at a higher y-value on the blue, 2nd distribution
# this tells us at x=2.5, we are closer to the mean on the 
# 2nd distribution so we expect a lower Zscore, but lets find out!
Zscore2 = (2.5-mean_dist2)/sd_dist2
print(Zscore2)
# indeed 1 < 3 - in our 2nd distribution, an observation of x=2.5 is only 1 SD from the mean

# Zscores allow us to in a sense "normalize" each normal 
# distribution to allow for comparisions between normal distributiosn with different means & SDs.
# For example, if these distributions were measuring a test
#  then a student that scored a 2.5 on both would have done 
#  better than the overall class distribution on the first test.

# So, lets take a moment to try another example:
# I am the manufacteror of rulers.  My rulers should be 10cm long, but I am having issues:
#  On Run #1 I get rulers with a mean of 11cm and an SD of 2.0cm.
#  On Run #2 I get rulers with a mean of 10 cm and an SD of 4.0 cm.

# Q1: Which is the better run of my manufacturing 
# equiptment?  Note: could be differing answers!
#   **THINK ON THIS FOR A BIT**

# Q2: in each run, pull out a ruler to see how off it is.
#  In both runs, I pull out a 9cm ruler - how unusual is it for me to pull 
#     out a ruler of this size?
# (i) Make a plot showing this & guess using the plot,
# (ii) Then, calculate with a Zscore and say for sure.
#  **TAKE A MOMENT - you don't have to do this, but think on how you might start**

# ANS:
# (i) Plot:
#  Run # 1: "mean of 11cm and an SD of 2.0cm"
x = seq(5,15,length=200)
plot(x,dnorm(x,mean=11,sd=2), type='l', ylim=c(0,0.2)) # further out for run #1
par(new=TRUE) # to overplot
#  Run # 2: "mean of 10 cm and an SD of 4.0 cm"
plot(x,dnorm(x,mean=10,sd=4),col="blue", type='l', ylim=c(0,0.2))
#  Our observation, a 9cm ruler:
abline(v=9.0,col="red")
# To remind us what is what:
legend("topright", c("Run 1", "Run 2"), col=c("black","blue"), lw=1)

# By eye, it looks like in run 1 (black) we are further from the mean (11cm), 
#  than for run 2 (blue).  So this means that it is more unusual to 
#   get this 9cm ruler in run 1 than run2

# But lets do the calculation to be sure
Z1 = (9.0-11)/2.0 # -1.0
Z2 = (9.0-10)/4.0 # -0.25
print(c("Run 1", "Run 2"))
print(c(Z1,Z2))
# Here -1.0 < -0.25 so run 1 is MORE SDs from the mean even though its negative!

#### BACK TO SLIDES FOR PERCENTILES #####



