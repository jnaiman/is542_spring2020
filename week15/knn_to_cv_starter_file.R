# knn in class

# RECALL:
# to probe KNN we will do this in steps
# (1) generate some "fake" sample data from a known distribution
# (2) use this fake data to "train" our KNN
# (3) we can see how well we do/try to calculate the boundary between 2 distributions
# (4) Now this week: we'll start quantifying the test/training errors on this simulated dataset
# (5) we'll explore validatian methods (assuming we don't know anything about the underlying population/model)

# lets upload all the package we had to install last time
#install.packages("mvtnorm") # run once
#install.packages("flexclust") # run once
library(dplyr)
library(mvtnorm)
library(flexclust)
library(class)
library(tidyverse)
library(stringr)

# just to set everything right
par(mfrow=c(1,1))

# use some helpful R scripts from github
source('https://raw.githubusercontent.com/idc9/stor390/master/notes/cross_validation/synthetic_distributions.R')
source('https://raw.githubusercontent.com/idc9/stor390/master/notes/cross_validation/knn_functions.R')

# we will now draw points from 2D gaussians, i.e. normal distributions
# one gaussian will be tagged as "1", the other as "-1"

# 2 random choices being made
#  (1) where the center of each gaussian/normal distribution is
#  (2) x1 & x2 coordinates

# For this example, we're going to make a "fake" population to choose a sample from:
#  a *population* of 2 classified datasets
#  based on a 2 randomly placed gaussians

# first, we fix the "mean seed" so this chooses where each 2D gaussian is placed
mean_seed = 238

# If we recall to last week, to get the same distribution every time, we need to 
#  have the same means & the same random drawing, we fix this with:
data_seed = 53272

# then for our underlying simulated population, lets take 2000 points of pos & 2000 points of neg
n_neg = 2000
n_pos = 2000

test_data = gmm_distribution2d(n_neg=n_neg, n_pos=n_pos, mean_seed=mean_seed, data_seed=data_seed)
plot(test_data$x1[test_data$y == -1], test_data$x2[test_data$y== -1], col="blue", 
     xlim=c(min(test_data$x1),max(test_data$x1)), ylim = c(min(test_data$x2),max(test_data$x2)), 
     xlab="x1", ylab="x2")
points(test_data$x1[test_data$y==1], test_data$x2[test_data$y==1], col="magenta")


# ok, so the above is our simulated population, or the "test" dataset
#  AGAIN: we will probably not have access to the actual distribution like we have
#  here -> which is just 2 gaussians

# Now, lets also simulate a smaller sample, i.e. the training dataset.
# Let's assume this sample dataset comes from this underlying population and 
#   is 400 points in total 
# We'll use the sample function to randomly grab these:
train_ind = sample.int(n = nrow(test_data), size = 400, replace = F)
train_data = test_data[train_ind, ]
# Of course, in reality, we don't want to double-count our test data - test data should 
#  be all new points, so let's take these out
test_data = test_data[-train_ind, ]

plot(train_data$x1[train_data$y == -1], train_data$x2[train_data$y== -1], col="blue", 
     xlim=c(min(train_data$x1),max(train_data$x1)), 
     ylim = c(min(train_data$x2),max(train_data$x2)), 
     xlab="x1", ylab="x2")
points(train_data$x1[train_data$y==1], train_data$x2[train_data$y==1], col="magenta")
