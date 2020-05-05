### ====== Confidence intervals ===== ####

# Let's first start with a generic example.

# Let's say we have a list of observations:
# observations: x1, x2, ..., xn
# Here, let's assume 200 observations to start with.

# From this set of observations, let's say we calculate some mean:
# mean(x1, x2, ..., xn) = 5.0
# This is just an example!

# And let's assume we find from our xn observations we measure 
#  the standard devation of:
# sd(x1, x2, ..., xn) = 2.0

# Put down our measurements:
sample_mean = 5.0
sample_SD = 2.0
nsample = 200 # number of samples

WHERE IS SE??

# First, let's plot the distribution of our theoretical sample means:
x = seq(sample_mean-3*SE, sample_mean+3*SE, length=200)
y = dnorm(x, mean=sample_mean, sd=SE)
plot(x,y,type='l', xlab='Value of the mean of the sample', ylab='Frequency of Mean')

# We can now create a confidence interval for this sample_mean.
# Let's start with the typical 95% confidence interval:
confidence_percent = 0.95

# To calculate a confidence interval, we need to calculate the 
#  Standard Error:
SE = sample_SD/nsample**0.5

# Let's calculate the Z-score corresponding to the 95% percentile.
# This 95% is centered around the center of the normal distribution.
# To figure out what Z-score this corresponds to, we need to translate this
#   to a percentile *around* the mean - which we do by finding the 
#   remaining part of the distribution "left" in the tails:
percent_tails = (1.0 - confidence_percent)/2.0 # 0.025
print(percent_tails)
# i.e. the percent of the distribution in the tails will be 2.5% on each side
#   -> (100%-95%)/2

# Now, let's find the Z-score corresponding to this percentile:
Z_95_star = qnorm(1-percent_tails) # = 1.96
print(Z_95_star)
# i.e. this is how far our "confidence interval" extends from the mean 
#  on a *normalized* normal model.

# To figure out how that relates to our actual model, we multiply this by 
#  the SE of our model:
interval = Z_95_star*SE
# estimate mean: sample_mean +/- Z_95*SE
print(paste("Estimate of sample mean is", sample_mean, '+/-', interval))

# How does this look on the plot - we can use our polygon again:
source('~/Downloads/plot_polygons.R') # to help with plotting
x2 = seq(sample_mean-interval, sample_mean+interval, length=100)
y2 = dnorm(x2, mean=sample_mean, sd = SE)
draw_polygon(x2,y2)

# To see that this is indeed our typical 95% rule we can do the same 
#  thing but comparing it to a normalized model:
par(mfrow=c(2,1))

plot(x,y,type='l', xlab='Value of the mean of the sample', ylab='Frequency of Mean')
x2 = seq(sample_mean-interval, sample_mean+interval, length=100)
y2 = dnorm(x2, mean=sample_mean, sd = SE)
draw_polygon(x2, y2)

# our "normal" normal model
xn = seq(-3,3,length=100)
yn = dnorm(xn,mean=0,sd=1)
plot(xn,yn, type='l')
xn2 = seq(-Z_95_star,Z_95_star,length=100)
yn2 = dnorm(xn2, mean=0,sd=1)
draw_polygon(xn2, yn2)

### BACK TO SLIDES FOR ARBITRARY CONFIDENCE INTERVAL ####

### ARBITRARY CONFIDENCE INTERVAL ####

par(mfrow=c(2,1))

# Just grabbing what we had above:
confidence_percent = 0.99 # try other values

# Calculate what is in the tails, percentage wise:
percent_tails = (1.0 - confidence_percent)/2.0 # 0.025

# Arbitrary Z-star (Z-score) for the percent in the tails
Z_star = qnorm(1-percent_tails) # = 1.96 for 95%
print(Z_star)

# Grab our confidence interval
interval = Z_star*SE

# Plot!
plot(x,y,type='l', xlab='Value of the mean of the sample', ylab='Frequency of Mean')
x2 = seq(sample_mean-interval, sample_mean+interval, length=100)
y2 = dnorm(x2, mean=sample_mean, sd = SE)
draw_polygon(x2, y2)

# Compare to normalized normal model:
xn = seq(-3,3,length=100)
yn = dnorm(xn,mean=0,sd=1)
plot(xn,yn, type='l')
xn2 = seq(-Z_star,Z_star,length=100)
yn2 = dnorm(xn2, mean=0,sd=1)
draw_polygon(xn2, yn2)




### ANOTHER EXAMPLE: UNFISH DATA ####

# Recall what we had before (can go back to fish-data example)
par(mfrow=c(1,1))

# read in our old friend the fish data
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")
log_trade_usd = log10(fishdata$Trade..USD.)

# Let's say we have a sample of this data
nsamples = 20
mysample = log_trade_usd[sample(1:length(log_trade_usd),nsamples,replace=FALSE)]

# We can calcualate the mean of this sample:
mean_fish = mean(mysample)

# And the standard error
SE_fish = sd(mysample)/nsamples^0.5


# What is the 72% confidence interval for our sample mean?

# We can do our setup much in the same way as before:
confidence_percent = 0.72 # try other values
# Calculate what is in the tails, percentage wise:
percent_tails = (1.0 - confidence_percent)/2.0 # 0.025

# Arbitrary Z-star (Z-score) for the percent in the tails
Z_star = qnorm(1-percent_tails) # = 1.96 for 95%
print(Z_star)

# The only difference now is what Standard Error we are using:
interval = Z_star*SE_fish

# Finally print:
print(paste("Estimate of sample mean is", mean_fish, '+/-', interval))

# In this way - we can get an arbitrary confidence interval about this mean.


#### EXAMPLE WITH OZONE DATASET ####

# This assumes "ozone" dataset is stored in Downloads folder
#  THIS IS A FILE you can download from the moodle page
ozone = read.csv('~/Downloads/ozone2.csv')

# check out this dataset a bit:
print(head(ozone))

# So there are a lot of random looking things there - we'll actually
#    use this dataset again for linear regression (I think), but 
#    right now we're just going to look at some temperature readings:

# Make a temperature vector:
temp_sandburg = ozone[,names(ozone)=="Temperature_Sandburg"]

# Let's check out the distribution of this temperature:
hist(temp_sandburg, xlab='Temp in F', ylab = 'Frequency')

# It looks like the mean temperature at Sandburg (airbase) is ~ 60?  Let's calculate:
mean_sandburg = mean(temp_sandburg)
print(mean_sandburg)

# How confident are we about this mean?
confidence_percent = 0.99
percentile_tails = (1.0-confidence_percent)/2.0 
Z_star = qnorm(percentile_tails, lower.tail=FALSE) # again, either way is fine
Z_star = qnorm(1-percentile_tails)

# Creating a point estimate: sample_mean +/- z* X SE
SE_sandburg = sd(temp_sandburg)/length(temp_sandburg)**0.5

interval = Z_star*SE_sandburg

print(paste("Estimate of sample mean is ", mean_sandburg, "+/-", interval))

# This seems overly small given the width of the Sandburg temperature 
#  distribution - any guesses to why that is?
# This is because we are trying to compare 2 different things.

# The width of the histogram of the Sandburg temperatures gives a us a measure 
#   of how the temperature changes at Sandburg over the time period of this dataset
#   (a year).

# The confidence interval we calculated gives us how much we expect the *mean of this 
#   distribution to change* if we go out and re-collect a sample.

# We can sort of *fake* this by simulating doing a sample of this dataset and calculating 
#  its mean a few times - giving us the distribution of sample means:

mymeans = c() # where the sample means are stored

nsamples = 10 # *Try changing this parameter and see how the plot and SE change*

nmeans = 300 #start with 5
for (i in 0:nmeans){
  mysample = temp_sandburg[sample(1:length(temp_sandburg), nsamples, replace=FALSE)]
  mymeans = append(mymeans, mean(mysample))
}
hist(mymeans, xlab="Sample Mean in F", prob=TRUE)
# lets plot the actual population mean
abline(v=mean(temp_sandburg), col="red", lwd=4)
# so something like a few 10s of sample

# First, calculate its standard error:
SE = sd(mysample)/nsamples^0.5
print(SE)

# We can see if we change the number of samples (change nsample) the width of our distribution 
#  of means changes and so does SE.  This is what SE is measuring!

# If we have a naturally wider population and therefore sample distribution
#  (the sample SD is bigger) than our measure of SE is indeed bigger
#  (SE is proportional to SD of the sample), *but* we can get better and better 
#  estimates of the true population mean if we have larger sample sizes since 
#  SE is also proportional to 1/nsamples**5.
