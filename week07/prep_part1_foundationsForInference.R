# Foundations for Inference

par(mfrow=c(1,1)) # reset plots, if needed

# We'll start by reading in our good pal, the fish-data
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")
log_trade_usd = log10(fishdata$Trade..USD.)
# Let's remind ourself what this distribution looks like:
hist(log_trade_usd)

# Now, let's assume this distribution is a measurement of the population.
# I.e., this dataset represents all of the trade of fish all over the globe.
# In reality, it is itself a sample, of which we don't really 
#  know the data collection methods, but lets suspend our disbelief for the moment.

# Let's pretend we are going out and collecting a random sample of the trade 
#  (in units of log10) of fish in the world.  We are asking over random years, 
#  countries, and import or export - what is the dollar value of the transaction?
# In short - we are *sampling* from this background population.

# Let's create a "mock" sample from our "mock" population data.
nsamples = 10 # Let's start with 10 random samples.

# Next, we'll pull randomly using our "sample" function we used last week:
mysample = log_trade_usd[sample(1:length(log_trade_usd), nsamples, replace=FALSE)]
# Here, we are using sample to pull random *indices* that run the length
#  of the log_trade_usd vector - a total of nsamples.
#print(sample(1:length(log_trade_usd), nsamples, replace=FALSE))
#print(mysample)
# In opposition to last week we will be setting replace=FALSE.
# This last part just means "sampling without replacement" so we don't double 
#   count indicies.

# Let's print out the mean of our sample and compare this to the mean of
#  our "mock" population:
print('Sample Mean,   Population Mean')
print(c(mean(mysample), mean(log_trade_usd)))
# Note: if I run this a few times with different random samples, I get 
#  different means for my sample.
# **RUN A FEW TIMES**

# Some questions we might have at this point:
# So, the means are close, but are they close enough? 
#  How to quantify how "close" or "far away" they are?

# Let's try to see how good our means are by taking a bunch of samples and 
#   calculating their means.  We'll store these in a vector of "sample means":
mymeans = c() # where the sample means are stored
nmeans = 5 #start with 5 samples -> 5 sample means

# Use sample function to generate samples of length nsamples (10), take the 
#  mean of these, and store it in our sample mean vector.  Do this nmeans times:
for (i in 0:nmeans){
  # grab the sample like before
  mysample = log_trade_usd[sample(1:length(log_trade_usd), nsamples, replace=FALSE)]
  # take the mean and store it:
  mymeans = append(mymeans, mean(mysample))
}

# Let's make a histogram of these means - this is refered to as our
#  "sampling distribution" in the book:
hist(mymeans, xlab="Sample Mean", prob=TRUE, xlim=c(2,7))
# We'll overplot the actual population mean
abline(v=mean(log_trade_usd), col="red", lwd=4)
# If we start with nmeans = 5, i.e. a sample of 
#  means with only 5 measurements, we see a lot of 
#  variablity if we run this a few times.
# **RUN A FEW TIMES WITH**

# What if we change nmeans to 50? 500? 5000 is pretty sweet
# Q: What do we expect to see?  How will our histogram change?
#  **GIVE THEM A MOMENT TO ANSWER**
# then: **DO THIS***

# We see our collection of means is clustered around our population mean and 
#  that this clustering gets "tighter" if we increase the number of samplings 
#  that we do.

# But how can we quantify how "good" our distribution of means is based on the 
#   number of sample means we take?

# Enter the *standard error*!


# Standard Error - first a calculation
SE = sd(log_trade_usd)/nsamples**0.5
# Don't worry,we'll talk about this equation in a few minutes.

# Here we are calculating the Standard Error based on the population.
# Let's overplot the theoretical spread on our sample mean distribution 
#   based on the mean of our population and the standard error of the 
#   population:
x = seq(2,7, length=200)
lines(x, dnorm(x, mean=mean(log_trade_usd), sd=SE), col="green")
# Here, we've overplotted how much we *think* the means should vary from the 
#  true, population, mean. We notice that our simulated distribution of sample 
#  means lines up nicely with our theoretical.  We can see that this is true 
#  when we change our the size of our samples (nsamples) **GO BACK AND DO**

# Often, don't know anything about the population, only have the sample.
# Also, we often just have the *one* sample, not a whole bunch where we can 
#   calculate the distribution of means.  What do we do then? 
# In this case, we can get a handle on how much we *estimate* the mean should 
#   vary by using the standard deviation of the values measured in our 
#   *sample* instead of the population:
SE_estimate = sd(mysample)/nsamples**0.5
lines(x, dnorm(x,mean=mean(mysample), sd=SE_estimate), col="blue")

# Again, the Standard Error using the sample SD is 
#  close to the population measurement, but not exact. 

# Let's summarize these things a bit more and get a handle on how the 
#   Standard Error is used.

#### BACK TO SLIDES FOR WHEN WE CAN USE SAMPLE MEANS ####






