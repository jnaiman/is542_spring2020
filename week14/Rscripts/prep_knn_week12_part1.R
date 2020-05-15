# KNN, week12

#  K-nearest neighbors
# https://idc9.github.io/stor390/notes/cross_validation/cross_validation.html

# to probe KNN a bit more what we are gonna do is
# (1) generate some "fake" sample data from a known distribution
# (2) use this fake data to "train" our KNN
# (3) see how well we do at reproducing the background distribution!

# these are the packages I had to install
#install.packages("mvtnorm")
#install.packages("flexclust")

# grab a bunch of libraries
library(mvtnorm)
library(flexclust)
library(class)
library(tidyverse)
library(stringr)

# we're also going to use some helpful KNN functions from github
# these are also uploaded onto the moodle page if you need them
source('https://raw.githubusercontent.com/idc9/stor390/master/notes/cross_validation/synthetic_distributions.R')
source('https://raw.githubusercontent.com/idc9/stor390/master/notes/cross_validation/knn_functions.R')
# if you have downloaded the files, you can instead do:
#source('~/Downloads/synthetic_distributions.R')
#source('~/Downloads/knn_functions.R')

## pause and make sure this works for everybody!

# ok, lets plot some 2D distributions
# What we are going to do is draw points from gaussians (i.e. normal distributions) 
# in 2D space

# ok, now lets pull from a 2D gaussian distribution and take a look
# in our gaussians, we will label points drawn from the first as "-1"
#  and the 2nd as "1"

# 2 random choices are being made
#  (1) a random choice of the centers of each 2D normal
#  (2) the x1 and x2 coordinates of the 2d normal

# Lets pull 10 points from each distribution
n_neg = 10
n_pos = 10

mydata = gmm_distribution2d(n_neg=n_neg, n_pos=n_pos)
print(mydata)
# We can look at the x1 and x2 values and which are 
#  tagged as positive and negative.

# lets also plot our distributions:
# lets plot!
plot(mydata$x1[mydata$y==-1], mydata$x2[mydata$y==-1], col="blue", 
     xlim=c(min(mydata$x1), max(mydata$x1)), ylim = c(min(mydata$x2), max(mydata$x2)),
     xlab='x1', ylab='x2',pch=16)
points(mydata$x1[mydata$y==1], mydata$x2[mydata$y == 1], col="magenta",pch=16)

# we can run this and see the points move around, lets try with more data points
#****go back and set n_neg/n_pos = 100 and 1000 ****

# What is x1 and x2?  In this case - we have not specified a physical meaning.  We are just using 
#  a gaussian mixture model for practice.  If it helps, you can think of x1 and x2 as amounts  
#  of 2 different drugs in a drug combination and the blue as patients that are cured and 
#  pink as patients that are still sick.  If we know that certain patients can only tollerate 
#  low levels of drug "x1" then we might want to know how high of a dosage to give patients of 
#  drug "x2" inorder to cure them - so we might want to find a "cure" boundary in this 
#  x1 & x2 space.

# Now, we can see that there are 2 distinct populations.
#  Let's say we want to keep the means of each of the distributions fixed & pull randomly from these fixed-mean distributions.

# We can do this by fixing the "seed" of the random means
#  this is the (1) random number

# Let's first practice doing this before going whole hog.
# Since we are doing random distributions, we need to set a "seed" 
# for the random number generator
# for example:
print(sample(1:10, 5))
# if I run this a few times, I get different sets of 5 numbers in 
# the range 1-10

# but lets say I'm testing something and I want to be 
# able to regenerate the same random numbers
set.seed(554)
print(sample(1:10, 5))

# if we run this another few times, we see we get 
# now the same random numbers

# to make sure things are random from now on
#  we can reset the seed
set.seed(NULL)
print(sample(1:10, 5))
# and now we get the same random numbers again

# ok, so lets go back and fix the mean of our distributions
mean_seed = 238
# and lets do a lot of points
n_neg = 1000
n_pos = 1000

mydata = gmm_distribution2d(n_neg=n_neg, n_pos=n_pos, mean_seed=mean_seed)

# lets plot!
plot(mydata$x1[mydata$y==-1], mydata$x2[mydata$y==-1], col="blue", 
     xlim=c(min(mydata$x1), max(mydata$x1)), ylim = c(min(mydata$x2), max(mydata$x2)),
     xlab='x1', ylab='x2')
points(mydata$x1[mydata$y==1], mydata$x2[mydata$y == 1], col="magenta")

# Now you can see that if I run this section a few times, the "middles" of the distributions are fixed, but
#    the drawn points shift around.

# We can also fix the actual draws so those are fixed as well and 
# the same data points will be chosen every time
#  this is useful for debugging our code
data_seed = 52345
mydata = gmm_distribution2d(n_neg=n_neg, n_pos=n_pos, mean_seed=mean_seed, data_seed=data_seed)

# lets plot!
plot(mydata$x1[mydata$y==-1], mydata$x2[mydata$y==-1], col="blue", 
     xlim=c(min(mydata$x1), max(mydata$x1)), ylim = c(min(mydata$x2), max(mydata$x2)),
     xlab='x1', ylab='x2')
points(mydata$x1[mydata$y==1], mydata$x2[mydata$y == 1], col="magenta")

# Now we can see that the draws from the underlying distribution are fixed as well
#  this was random number (2) in our list above.

# Let's try with a smaller simulated dataset for now:
n_neg_sample = 200
n_pos_sample = 200
mean_seed = 238
data_seed = 1232

sample_data = gmm_distribution2d(n_neg=n_neg_sample, n_pos=n_pos_sample, 
                                 mean_seed=mean_seed, data_seed=data_seed)
# note that I'm using a different data seed as we want a different pull


# What are some ways we could think of classifying these?

############## FIRST: GLM MODEL ################

# What about a GLM model?  Lets try that to see its limitations

# For this, we'd have to map our -1 to zero to keep with our usual nomenclature.
# This is not totally necessary, but lets be consistent:
sample_data_glm = sample_data
levels(sample_data_glm$y) = c("0","1")
print(sample_data_glm$y)

glm2 = glm(y~x1+x2,data=sample_data_glm, family=binomial)
print(summary(glm2))

# So we see that this is a pretty good fit if we just looked at our p-values.

# Let's make a function that returns the probaility we are in 1 or 2 using the 
#   methodology for logistic functions as before:
myMultipleProbFunction = function(x1, x2){
  # first, input our cofficients
  ly2 = glm2$coefficients[1] + glm2$coefficients[2]*x1 + glm2$coefficients[3]*x2
  ply2 = exp(ly2)/(1.0 + exp(ly2))
  return(ply2)
}

# Let's pick a point x1=-1 & x2=4 and see what probability of being in the blue
# here: "success" is +1, our magenta points
#  "failure" was -1, is now 0, is our blue points
# so here, our probability is the probabiity of being "magenta", or +1
print(myMultipleProbFunction(-1,4)) # small prob, very unlikely magenta
print(myMultipleProbFunction(2,-2)) # large prob, very likely magenta
print(myMultipleProbFunction(0,0)) # 30% chance magenta
print(myMultipleProbFunction(1,-2)) # high probability magenta

#How might we plot a "decision" boundary?  Well, we can construct a set of x1 steps, and 
# loop through some x2 steps and record where the probability that we are 
# magenta is higher than some set probability and then mark this value
#  and then plot a line when we are done.  Lets try this!

##### SKIP THIS PART ONLINE ######
grid_size = 100 # size of our grid (assume equal in x & y)
prob_cut_off = 0.5 # cut at 1/2 - 50% probability of success i.e. equally likely to be blue/pink
# how far will our grid span in x1 and x2 - lets cover all space visible:
myx1_seq = seq(-2,4,length=grid_size)
myx2_seq = seq(-2, 4, length=grid_size) # for looping and testing

# for saving results for plotting
myx2=c() # for saving
myx1=c()

# loop over x1 and x2
for (i in 1:grid_size){
  for (j in 1:grid_size){
    # for each grid point, calculate the probability of success
    myProb = myMultipleProbFunction(myx1_seq[i],myx2_seq[j])
    # tag this as the cut off if prob > our cut off
    # BUT! only if we haven't already tagged it
    if ((length(myx2) < j) && (myProb >= prob_cut_off)){
      myx2 = c(myx2,myx2_seq[j])
      myx1 = c(myx1,myx1_seq[i])
    }
  }
}
# plot and take a look!
lines(myx1,myx2)

###### CONTINUE ########

# Note: we can plot this with a contour map, with just some futzing.

# a few variables we need:
grid_size = 100 # size of our grid (assume equal in x & y)
prob_cut_off = 0.5 # cut at 1/2 - 50% probability of success i.e. equally likely to be blue/pink
# how far will our grid span in x1 and x2 - lets cover all space visible:
myx1_seq = seq(-2,4,length=grid_size)
myx2_seq = seq(-2, 4, length=grid_size) # for looping and testing

# Let's first make a probability grid - will calculate the probability of 
#  success, i.e. probability that a specific grid point should be tagged as 
#  being in the "magenta" distribution:
probGrid = matrix(0,grid_size,grid_size) # 100x100 grid
for (i in 1:grid_size){
  for (j in 1:grid_size){
    myProb = myMultipleProbFunction(myx1_seq[i],myx2_seq[j])
    # now, lets save!
    probGrid[i,j] = myProb
  }
}

contour(myx1_seq,myx2_seq, probGrid, levels=0.5, labels="")
points(sample_data$x1[sample_data$y==1], sample_data$x2[sample_data$y == 1], col="magenta")
points(sample_data$x1[sample_data$y==-1], sample_data$x2[sample_data$y == -1], col="blue")




#### IN GROUPS ####
#### SKIP IN ONLINE CLASS #####
# (1) redo this with different random seeds and see what changes, use "contour" or construct lines to show the cut-off
#     Do you think our GLM model classifies the boundaries well?  Why or why not?
#     You might have to use: set.seed(NULL) to get random seeds if that is what you want
#
# BONUS: check out some of the other distributions in synthetic_distributions.R
#  How does classifying for boston_cream_doughnut distribtuion look?  Why do you expect this to work/better or worse?

# ANS
#(1) With different seeds
set.seed(NULL)

n_neg_sample = 1000
n_pos_sample = 1000
sample_data = gmm_distribution2d(n_neg=n_neg_sample, n_pos=n_pos_sample)
# lets plot!
# even make a plotting function
plotMyData = function(the_data){
  plot(the_data$x1[the_data$y==-1], the_data$x2[the_data$y==-1], col="blue", 
       xlim=c(min(the_data$x1), max(the_data$x1)), ylim = c(min(the_data$x2), max(the_data$x2)),
       xlab='x1', ylab='x2')
  points(the_data$x1[the_data$y==1], the_data$x2[the_data$y == 1], col="magenta")
}
plotMyData(sample_data)

# What about a GLM model?  For this, we'd have to map our -1 to zero to keep with our usual nomenclature
sample_data_glm = sample_data
levels(sample_data_glm$y) = c("0","1")
print(sample_data_glm$y)

glm2 = glm(y~x1+x2,data=sample_data_glm, family=binomial)
print(summary(glm2))

# lets make a function that returns the probaility we are in 1 or 2
myMultipleProbFunction = function(x1, x2){
  # first, input our cofficients
  ly2 = glm2$coefficients[1] + glm2$coefficients[2]*x1 + glm2$coefficients[3]*x2
  ply2 = exp(ly2)/(1.0 + exp(ly2))
  return(ply2)
}

grid_size = 100 # size of our grid (assume equal in x & y)
prob_cut_off = 0.5 # cut at 1/2
myx1_seq = seq(-4,4,length=grid_size)
myx2_seq = seq(-4, 4, length=grid_size) # for looping and testing
# for saving results
myx2=c() # for saving
myx1=c()
for (i in 1:grid_size){
  for (j in 1:grid_size){
    myProb = myMultipleProbFunction(myx1_seq[i],myx2_seq[j])
    # tag this as the cut off if prob > our cut off
    # BUT! only if we haven't already tagged it
    if ((length(myx2) < j) && (myProb >= prob_cut_off)){
      myx2 = c(myx2,myx2_seq[j])
      myx1 = c(myx1,myx1_seq[i])
    }
  }
}
# note: we can also plot this with a contour map, with just some futzing:
# lets first make a probability grid
probGrid = matrix(0,grid_size,grid_size) # 100x100 grid
for (i in 1:grid_size){
  for (j in 1:grid_size){
    myProb = myMultipleProbFunction(myx1_seq[i],myx2_seq[j])
    # now, lets save!
    probGrid[i,j] = myProb
  }
}

contour(myx1_seq,myx2_seq, probGrid, levels=0.5, labels="")
points(sample_data$x1[sample_data$y==1], sample_data$x2[sample_data$y == 1], col="magenta")
points(sample_data$x1[sample_data$y==-1], sample_data$x2[sample_data$y == -1], col="blue")
# note if we compare to our more intuative plotting before, they line up nicely
lines(myx1,myx2,col='red')

# (2) THE BONUS
n_neg_sample = 1000
n_pos_sample = 1000
sample_data = boston_cream_doughnut(n_neg=n_neg_sample, n_pos=n_pos_sample)

sample_data_glm = sample_data
levels(sample_data_glm$y) = c("0","1")
print(sample_data_glm$y)

glm2 = glm(y~x1+x2,data=sample_data_glm, family=binomial)
print(summary(glm2))

# lets make a function that returns the probaility we are in 1 or 2
myMultipleProbFunction = function(x1, x2){
  # first, input our cofficients
  ly2 = glm2$coefficients[1] + glm2$coefficients[2]*x1 + glm2$coefficients[3]*x2
  ply2 = exp(ly2)/(1.0 + exp(ly2))
  return(ply2)
}

grid_size = 100 # size of our grid (assume equal in x & y)
prob_cut_off = 0.5 # cut at 1/2
myx1_seq = seq(-4,4,length=grid_size)
myx2_seq = seq(-4, 4, length=grid_size) # for looping and testing

probGrid = matrix(0,grid_size,grid_size) # 100x100 grid
for (i in 1:grid_size){
  for (j in 1:grid_size){
    myProb = myMultipleProbFunction(myx1_seq[i],myx2_seq[j])
    # now, lets save!
    probGrid[i,j] = myProb
  }
}

contour(myx1_seq,myx2_seq, probGrid, levels=0.5, labels="")
points(sample_data$x1[sample_data$y==1], sample_data$x2[sample_data$y == 1], col="magenta")
points(sample_data$x1[sample_data$y==-1], sample_data$x2[sample_data$y == -1], col="blue")





###### CONTINUE ######

####### BACK TO SLIDES #######
### talk about issues with logistic regression #####


###### REDO WITH KNN #####

## Lets get into some KNN!!

# following: https://stats.stackexchange.com/questions/21572/how-to-plot-decision-boundary-of-a-k-nearest-neighbor-classifier-from-elements-o

# Let's re
#n_neg_sample = 200
#n_pos_sample = 200
# lets set our seeds so we can reproduce things
#mean_seed = 238
#data_seed = 1232
#sample_data = gmm_distribution2d(n_neg=n_neg_sample, n_pos=n_pos_sample, 
#                                 mean_seed=mean_seed, data_seed=data_seed)

# We'll label our dataset as our "training" sample.
# Then we'll "test" how well we do with a set of grid points.
# Technically, the "train" and "test" datasets usually are splits of our collected 
#  dataset, but we'll get more into that in a moment.
# For formatting, make columns
train.X = cbind(sample_data$x1, sample_data$x2)

# grid of new x1 & x2 values we'd like to have a boundary for
grid_size = 10 # new grid size
x1_new = seq(-2,4,length=grid_size)
x2_new = seq(-2,4,length=grid_size)
# new x1 and x2 grid => like collapsing a matrix
test.X = expand.grid(x1_new,x2_new)

# now, lets pick the "k" of our fit - how many neighbors do we want to average over?
k = 5

# ok, lets make a prediction!
# Here train.X and test.X are our matrix formatting from above
#  sample_data$y is a -1 or a +1
# and setting prob=T just means we want to output our proability too
knn_train_prediction = knn(train.X, test.X, sample_data$y, k=k, prob=T) # set k

# Let's take a look:
print(knn_train_prediction)

# Let's plot this using our "contour" function again

# Now, we are grabbing the probability of belonging to a group
# 0.5 is the boundary
# 1 means positive group, 0 means negative group
prob = attr(knn_train_prediction, "prob")
print(prob)

# This is a bit of a hack - it just flips the probability so that we
#  pick out *only* the 0.5 line for prob = -1
# This is just fancy stuff for plotting
prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
# Basically, we are now calculating the probability of "success"
#  instead of probability of belonging in a specific group.

# Formatting for contour plots (like before)
prob2 = matrix(prob2, grid_size, grid_size)

contour(x1_new,x2_new, prob2, levels=0.5, labels="")
points(sample_data$x1[sample_data$y==1], sample_data$x2[sample_data$y == 1], col="magenta")
points(sample_data$x1[sample_data$y==-1], sample_data$x2[sample_data$y == -1], col="blue")

# now we have something that looks like a boundary!  Neato!!

# now we can go back and play around with our model
#  (1) make a finer grid with changing grid_size
#  (2) changing the # of neearest neighbors with k


### IF ONLINE DO:
# Let's look at a few different k's all at once
k_values = c(1, 3, 5, 9, 15, 20, 50, 100, 300)
par(mfrow = c(3,3))
for( i in 1:length(k_values)){
  knn_train_prediction = knn(train.X, test.X, train_data$y, k=k_values[i], prob=T)
  # grab stuff for plotting boundary
  prob = attr(knn_train_prediction, "prob")
  prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
  prob2 = matrix(prob2, grid_size, grid_size)
  # make title labels
  titleLab = paste("k = ", toString(k_values[i]))
  # now plot
  contour(x1_new, x2_new, prob2, levels=0.5, labels="", lwd=4, 
          xlab="x1", ylab="x2", main=titleLab)
  points(train_data$x1[train_data$y==1], train_data$x2[train_data$y==1], col="magenta")
  points(train_data$x1[train_data$y==-1], train_data$x2[train_data$y==-1], col="blue")
}

# Things to note: if we use a very small # of neightbors we get a very 
#  squiggly line, but the more neighbors we include the more linear the line looks.

# But what is the best k here?  Will it be the same k for all distributions?
#  Keep this question in mind!!


######## SKIP ONLINE or go through if time #########
###### IN GROUPS ######
# (1) do the same but for boston_cream_doughnut
n_neg_sample = 200
n_pos_sample = 200
sample_data = boston_cream_doughnut(n_neg=n_neg_sample, n_pos=n_pos_sample)
# BONUS: what if you change k? What happens if it is smaller?  Or larger?  What looks like the k for a "best" fit?
#  Plot a matrix of classifications for different k and see what the differences are.
#  You can use either distribution (2 gaussians or boston cream)
#  How does this change with different random seeds?  (note: for boston cream, there is no random seed input)
#  How do things change with more or less sample points?  Or grid points?

# ANS
# (1) 
n_neg_sample = 200
n_pos_sample = 200
sample_data = boston_cream_doughnut(n_neg=n_neg_sample, n_pos=n_pos_sample)

# for formatting, make columns
train.X = cbind(sample_data$x1, sample_data$x2)

# grid of new x1 & x2 values we'd like to have a boundary for
grid_size = 10 # new grid size
x1_new = seq(-2,4,length=grid_size)
x2_new = seq(-2,4,length=grid_size)
# new x1 and x2 grid => like collapsing a matrix
test.X = expand.grid(x1_new,x2_new)

# now, lets pick the "k" of our fit
k = 5

# ok, lets make a prediction!
knn_train_prediction = knn(train.X, test.X, sample_data$y, k=k, prob=T) # set k

# lets plot this using our "contour" function again

# now, we are grabbing the probability of belonging to a group
# 0.5 is the boundary
# 1 means positive group, 0 means negative group
prob = attr(knn_train_prediction, "prob")
print(prob)

# this just flips the probability so that we
#  pick out *only* the 0.5 line for prob = -1
# this is just some fancy stuff for plotting
prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)

prob2 = matrix(prob2, grid_size, grid_size)

par(mfrow=c(1,1))
contour(x1_new,x2_new, prob2, levels=0.5, labels="")
points(sample_data$x1[sample_data$y==1], sample_data$x2[sample_data$y == 1], col="magenta")
points(sample_data$x1[sample_data$y==-1], sample_data$x2[sample_data$y == -1], col="blue")


# BONUS:
mean_seed = 238
data_seed = 1232
train_data = gmm_distribution2d(n_neg=n_neg_sample, n_pos=n_pos_sample, 
                                 mean_seed=mean_seed, data_seed=data_seed)

train.X = cbind(train_data$x1, train_data$x2)

# grid of new x1 & x2 values we'd like to have a boundary for
grid_size = 10 # new grid size
x1_new = seq(-2,4,length=grid_size)
x2_new = seq(-2,4,length=grid_size)
# new x1 and x2 grid => like collapsing a matrix
test.X = expand.grid(x1_new,x2_new)

# ok, lets look at a few different k's all at once
k_values = c(1, 3, 5, 9, 15, 20, 50, 100, 300)
par(mfrow = c(3,3))
for( i in 1:length(k_values)){
  knn_train_prediction = knn(train.X, test.X, train_data$y, k=k_values[i], prob=T)
  # grab stuff for plotting boundary
  prob = attr(knn_train_prediction, "prob")
  prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
  prob2 = matrix(prob2, grid_size, grid_size)
  # make title labels
  titleLab = paste("k = ", toString(k_values[i]))
  # now plot
  contour(x1_new, x2_new, prob2, levels=0.5, labels="", lwd=4, xlab="x1", ylab="x2", main=titleLab)
  points(train_data$x1[train_data$y==1], train_data$x2[train_data$y==1], col="magenta")
  points(train_data$x1[train_data$y==-1], train_data$x2[train_data$y==-1], col="blue")
}

