# NOTE: for this class (spring 2020, online) we'll skip down to the "fish data" section
#  The rest of this is left as extra examples to be worked through on your own.

#----------------------------------
# Simulations
# what is the normal distribution?  What does
#  it represent?  
# we saw some examples earlier on that website, but
#  we can also simulate our own!

# first, lets do some toy examples
samples = rnorm(1000, mean=0, sd=1)
print(samples)
print(length(samples))

# so, we see that we have 1000 samples
# we can first plot them with a scatter plot
par(mfrow=c(1,3))
dotchart(samples) 
# ok, so by eye we can see a cluster around zero (our mean)
# and that things start falling off at like +/- 1, our SD
#  We can check out this distribution by building a histogram
hist(samples)

# it sure looks normal!
# 2nd plot
qqnorm(samples)
qqline(samples)

# ACTIVITY 2
# we can also simulate things like 2 dice rolls to 
#  see how the sum is distributed
number_of_sides = 6
number_of_throws = 100
# throw the dice
die1 = sample(number_of_sides,number_of_throws, replace=TRUE)
die2 = sample(number_of_sides,number_of_throws, replace=TRUE)
# add the sum
distrib = die1+die2
# plot
par(mfrow=c(1,3))
dotchart(distrib)
# histogram of distribution
hist(distrib)
# is it normal? qqline plot
qqnorm(distrib)
qqline(distrib)

# questions:
#  how does this change with number of throws?
#  what about sides of die?

# ------------------------------------------

# a slight tangent - fitting with a gaussian
# another way to guestimate if something is normal
#  we'll do more regrious fitting later in the 
# course, but just to get a taste
library(MASS)
fit = fitdistr(distrib, "normal")
para = fit$estimate
print(para) # prints mean & standard deviation

par(mfrow=c(1,1)) 
hist(distrib, xlab = "Sum of die rolls", prob=TRUE) # the prob=TRUE normalizes the distribution
x = seq(0,10,length=200)
# plot this fitted normal distribution
mean_fit = para[1]
sd_fit = para[2]
curve(dnorm(x, mean=mean_fit,sd=sd_fit), col = "red", add = TRUE)

# now that we have our normal distribution, we can do
# the same thing to it we've been doing before
# for example, what is the percentile of sum = 10?
# we start by plotting...
x=seq(0,12,length=200)
plot(x,dnorm(x,mean=mean_fit,sd=sd_fit))


# ---------------------------------------------------
# FISH DATA SECTION

# is it normal?
# with FISH DATA!!

# lets look back at our fish data to see if we can find anything
# interesting
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")

# lets plot some stuff
par(mfrow=c(1,2)) # do a 1x2 plots (2 plots per page)
# what about a log scale?
# plot #2
hist(log10(fishdata$Trade..USD.), xlab = "All Trade in log10($USD)")

# subset example: grab only croatian values that are imported oysters for weight 
#mask = (country=="Croatia") & (transaction=="Import") & (type == "Oysters")
#weight_croatia_import_oysters = subset(weight,mask)

# 2nd plot
qqnorm(log10(fishdata$Trade..USD.))
qqline(log10(fishdata$Trade..USD.))
# Note: takes a while to run

# Ask class: is it normal?

## Will probably skip below, but for reference?

# looks like a long-tailed (wider than normal) distribution
# lets play around with fitting a normal distribution anyway
# NOTE: we'll get more into the nitty-gritty of fitting things later in class
library(MASS)
fit <- fitdistr(log10(fishdata$Trade..USD.), "normal")
para <- fit$estimate
print(para) # prints mean & standard deviation

par(mfrow=c(1,1)) 
hist(log10(fishdata$Trade..USD.), xlab = "All Trade in log10($USD)", prob=TRUE) # the prob=TRUE normalizes the distribution
x = seq(0,10,length=200)

# plot this fitted normal distribution
curve(dnorm(x, para[1], para[2]), col = "red", add = TRUE)

# some questions you can ask (given the mean and sd of this normal distribution just calculated): 
#  what is the probability that any trade transaction will have a value > 10^8 dollars (USD)?
#  what is the trade value at the 10th percentile? 40th?

# bonus:
# what other variables in this dataset are normal(ish)? What can we learn about 
# the probability of one of their values?  Percentiles?

# how good are your estimates given how "normal" the distributions look?