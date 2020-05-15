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

mask = test_data$y == -1
plot(test_data$x1[mask], test_data$x2[mask], col="blue", 
     xlim=c(min(test_data$x1),max(test_data$x1)), ylim = c(min(test_data$x2),max(test_data$x2)), 
     xlab="x1", ylab="x2")
points(test_data$x1[!mask], test_data$x2[!mask], col="magenta")


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

mask = train_data$y == -1
plot(train_data$x1[mask], train_data$x2[mask], col="blue", 
     xlim=c(min(train_data$x1),max(train_data$x1)), 
     ylim = c(min(train_data$x2),max(train_data$x2)), 
     xlab="x1", ylab="x2")
points(train_data$x1[!mask], train_data$x2[!mask], col="magenta")


# OK, if we recall from last lecture, we fit a k=5 KNN model. 
#  This involved doing some data reformatting.

# First, we turned our training data in to columns:
train.X = cbind(train_data$x1, train_data$x2)

# We then made a grid of new x1 & x2's
grid_size = 50 # number of new points we want to classify in x/y
n_x1_new = grid_size
n_x2_new = grid_size
x1_new = seq(-2,4, length=n_x1_new)
x2_new = seq(-2,4, length=n_x2_new)

new_grid.X = expand.grid(x1_new,x2_new)

# pick the "k" part of our KNN
k = 5

# We then trained to make a prediction:
knn_train_prediction = knn(train.X, new_grid.X, train_data$y, k=k, prob=T)

# And we manipulated the output probability attributes to plot:
# now we want to plot a boarder, so we need to figure out where P(green) = P(blue) = 0.5
prob = attr(knn_train_prediction, "prob")
# picking out only 0.5 line -> fancy plotting stuff
prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
# reformat into a grid
prob2 = matrix(prob2, n_x1_new, n_x2_new)

contour(x1_new, x2_new, prob2, levels=0.5, labels="", lwd=4)
mask = train_data$y == -1
points(train_data$x1[mask], train_data$x2[mask], col="blue")
points(train_data$x1[!mask], train_data$x2[!mask], col="magenta")

# So, this is essentially where we got to last lecture.

# Here, because we have the underlying distribution - test_data, we can use a useful 
# function to calculate *both* the test and the training error
# recall: Training error is 1/n * SUM ( Indicator(yi != yhati) ) for our sample
#  and:   Test error is Average( Indicator(y0 != yhat0) ) for a new observation

errs = get_knn_error_rates(train_data, test_data, k)
print(errs)
# so here "tr" is the training error (sample) and "tst" is the test error (new observation)

# We'll also try with different k's **DO THIS BY HAND **

# ok, lets look at a few different k's all at once
# we played a bit with this last week, but lets add the 
# errors in as well to our plot:
k_values = c(1, 3, 5, 9, 15, 20, 50, 100, 200)
par(mfrow = c(3,3))
for( i in 1:length(k_values)){
  knn_train_prediction = knn(train.X, new_grid.X, train_data$y, k=k_values[i], prob=T)
  errs = get_knn_error_rates(train_data, test_data, k_values[i])
  # grab stuff for plotting boundary
  prob = attr(knn_train_prediction, "prob")
  prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
  prob2 = matrix(prob2, n_x1_new, n_x2_new)
  # make title labels - note I'm adding in some rounding
  titleLab = paste("k = ", toString(k_values[i]), ", testE = ", toString(round(errs$tst,4)), 
                   ", trainE = ", toString(round(errs$tr,4)))
  # now plot
  contour(x1_new, x2_new, prob2, levels=0.5, labels="", lwd=4, xlab="x1", ylab="x2", main=titleLab)
  mask = train_data$y==-1
  points(train_data$x1[mask], train_data$x2[mask], col="blue")
  points(train_data$x1[!mask], train_data$x2[!mask], col="magenta")
}

# We can see that the test & training errors change with different k values.
# Training error tends to increase with increasing complexity (here, lower k) - 
#    model getting further from fitting our sample dataset.
#  Testing error tends to decrease before increasing again - 
#   model gets closer to test data, and diverges again.

# Let's plot both of these errors as a function of k
# NOTE: this can take a while...
k_values = seq(from=1, to=400, by=4) # only go up to 200
num_k = length(k_values)
tr = c()
tst = c()
# now lets loop
for( i in 1:num_k ){
  errs = get_knn_error_rates(train_data, test_data, k_values[i])
  tr = append(tr, errs$tr)
  tst = append(tst, errs$tst)
}

# lets plot how these errors change with our KNN-k
par(mfrow = c(1,1))
plot(k_values, tr, xlab='k value', ylab='Error levels', type='l', col='blue', ylim=c(0.0,0.12))
lines(k_values, tst, col="magenta")
legend("topleft",c("Training error","Test error"),col=c("blue","magenta"), lwd=2)

# NOTE: that the training error (sample) tends to increase with increasing complexity (increasing k)
#  i.e. as our line fits less the data by being wiggly this increases

#  Test error has a characteristic U shape
# by looking at these 2 lines it seems that k ~ 5-10 minimizes both test and training errors

# so, in this example, we had the background model and used it to generate the TEST data set
#  but usually, we won't be using simulated data, so then what?  Who do we get the 
#  Test error rate and not just the training error rate so that we can get the full
#   error rate?

# we can look at a few different methods, but first we'll start with the CROSS-VALIDATION 
#  set of methods


###################################
# COME BACK AFTER TALKING ABOUT CV
###################################

# Now, lets pretend we don't know the underlying distribution
# i.e. all we have is "train_data" from above, and not "test_data"

# What we are going to do is split the train_data into 
# 2 groups a "fake" training set and a "fake" testing set i.e. the "validation set".

# Let's select how many times we want to break our data into these groups
k_folds_k = 5 #  note, both KNN and k-folds use a "k", hence the silly naming

# Process for each k_folds_k is as follows:
# 1. split data into training set & validation set randomly
#     (k_folds_k-1)/(k_folds_k) % of the data is in training, 
#     remaining 1/k_folks_k is in validation (fake "test") dataset
# 2. Fit model on train data
# 3. Compute train & test error using train and validation data
# 4. Use the test error as a proxy for our validation error: 
#    i.e. CV = Test error is Average( Indicator(y0 != yhat0) )

# NOTE: we're only going to store the test error here because
#  we are using CV to test how well our model fits the *underlying population*.
#  We are in essense *model picking*.
# In the end, we'll have our "best set of parameters"
#  and we can go back and calculate the full MSE for our 
#  fit which will tell us both how well we fit the data
#  but also (about) how well we are fitting the 
#  underlying population.


# Do 1-3 for each "k" of the KNN process we are interested in
#  this gives use a k by k_folds_k matrix of errors
#  we'll use this to calculate the error on our model.

# ok, enough chit chat, lets do it!
k_values = seq(from=1, to=100, by=2) # lets stay small to start with
num_k = length(k_values)

# to store our errors
error_matrix = matrix(0, nrow=num_k, ncol=k_folds_k)
print(error_matrix)

# ok, so that is going to be a little hard to keep track of, lets do something fancy
#   using "tibble"
error_matrix = matrix(0, nrow=num_k, ncol=k_folds_k) %>% as_tibble() %>% 
  add_column(k=k_values)

# lets name our matrix columns to keep track
colnames(error_matrix) = str_replace(colnames(error_matrix), 'V', 'fold')

# now if we look at the matrix we have stored useful info
head(error_matrix)

# Now we have labels of rows (each is a k value up to 100)
# under the "k" column, and the "fold" we are using is along the column
#  the entries.

# We'll store our total MSE from both tr & tst in each entry of our matrix.

# While we want to do select samples for test & train
#  randomly in theory, in practice here
#  we are going to set a seed so we all get the same thing
set.seed(31244) # use different one

# ok, lets loop and fill our matrix!
n = nrow(train_data) # total number of original training points
for(m in 1:k_folds_k){
  # calculate the number of data points that go into the new training set
  n_cv_tr = floor(n * (k_folds_k-1)/k_folds_k ) 
  
  # now, lets grab the indices of our new training dataset
  # note that we want unique indices so we sample w/o replacement
  # again, our "sample" function is just pulling random indicies
  #  so we can grab random points from our sample
  cv_tr_indices = sample(x=1:n, size=n_cv_tr, replace=FALSE)
  
  # pull from our original training data set our k-folds training and test sets
  cv_tr = train_data[cv_tr_indices, ] # training indicies
  cv_tst = train_data[-cv_tr_indices, ] # - sign means NOT training indicies
  
  # now that we have our fake training & test sets, 
  #  again loop over each KNN k and calculate 
  #  test & training errors
  for(i in 1:num_k){ # loop for each KNN-k!
    # compute the test error on the validation set
    errs <- get_knn_error_rates(cv_tr, cv_tst, k_values[i])
    # store values in the data frame
    error_matrix[i, paste0('fold',m)] = errs[['tst']]####+errs[['tr']]
    # remember, only using tst error as a proxy for our CV error (see ISL 5.1.5) for now
  }
  
}

# now, lets plot our CV results for each fold
color = c("blue", "red", "orange", "magenta", "yellow")
plot(k_values, pull(error_matrix[,1]), type='l', col=color[1], ylim=c(0.0, 0.12))
lnames = c("k fold = 1")
for(i in 2:k_folds_k){
  lines(k_values, pull(error_matrix[,i]), col=color[i])
  lnames = append(lnames, paste("k fold =",toString(i)))
}
legend("bottomright", lnames, col=color, lwd=2)

# so, we can see in general that all of these CV errors tend to increase
#  at both the low and high k ends, though they have different
#  overall noramlizations

# ok, now, lets compute the mean CV error for each KNN k
# we'll do this across each row
#print(error_matrix) to take a look
cv_mean = rowMeans(error_matrix[-(k_folds_k+1)])
# note, the -(k_folds_k+1) is to not include the k labeling column
# now, lets plot this as well
lines(k_values, cv_mean, col='black')

# we can also see how this changes with different seeds
# **** GO BACK AND DO ***
### NOTE: go back and set to original seed & re-run

# ok, now, lets plot this with the tst and tr errors we calculated
# before to compare
plot(k_values, cv_mean, col='black', type='l', ylim=c(0.02, 0.08))
# since we originally calcualted these with a 
# differnt set of KNN k's lets re calculate
tr = c()
tst = c()
# now lets loop
for( i in 1:num_k ){
  # here: our training data is our sample, 
  #  and our test_data is our actual distribution
  errs = get_knn_error_rates(train_data, test_data, k_values[i])
  tr = append(tr, errs$tr)
  tst = append(tst, errs$tst)
}

lines(k_values, tst, col="magenta")
lines(k_values, tr, col="blue")

legend("topright", c("Test", "CV", "Training"), col=c("magenta","black","blue"), lwd=2)

# ideally, all the minimums would line up, but of course this is not the case
#  what we can say is that k between ~10 to 35 seems to be the best choice to 
#  pick.  In theory, we should also run this for different random seeds to see 
#  what changes, but in the interest of time, we'll move on (or do it if we have time!)

# In summary: the black line we calculated using CV is similar to the actual
#   test error rate from our simulated population, in magenta

# so, now that we know how the mechanism works in detail, we can also just use
#  a built in function to do this for us:

# might need the following, but I'm not sure, was messing around with
# things:
# install.packages("e1071")
library("e1071")

y = pull(train_data[,3]) # grab 3rd column, format into vector
x1 = pull(train_data[,1]) # pull 
x2 = pull(train_data[,2])
# bind
x = cbind(x1,x2)

knn.kfold = tune.knn(x=x, y=y, k=1:50)
print(summary(knn.kfold))

# so, we can see that the above does a 10-fold CV measurement
# it also gives you the "best" k
#  you can run this a few times to see that this "best" k changes
#   with different random fold subsets

# we can also specify the number of folds to compare to our previous
# calculations like so:
knn.kfold_5 = tune.knn(x=x, y=y, k=1:50, 
                       tunecontrol=tune.control(sampling = "cross", cross=k_folds_k))

# lets plot!
k_k5 = knn.kfold_5$performances$k
err_k5 = knn.kfold_5$performances$error
### WRONG!!!!
#lines(k_k5, err_k5/k_folds_k, col="orange")
lines(k_k5, err_k5, col="orange")

# we can also do:
plot(knn.kfold_5)

# the final method we'll look into is bootstrapping

#######################
#  GO TO BOOTSTRAPPING SLIDES!
#######################

# now, we could go through the whole example again of doing this
# by hand, or now that we know about the tuning function in 
# R, lets just do that!

#Full Data set can be used for cross validation
knn.boot = tune.knn(x = x, y = y, k = 1:50,tunecontrol=tune.control(sampling = "boot") )

#Summarize the resampling results set
summary(knn.boot)

# if we run this a few times we see that we get slightly different k's 
# lets compare this to our k-folds above
k_boot = knn.boot$performances$k
err_boot = knn.boot$performances$error
lines(k_boot, err_boot, col="orange")

# again, because it is a different method, the normalization is slightly
# different, but overall the shape is the same


# with either method, we find k ~20-30 minimizes the error
#  so, lets go all the way back to plotting our KNN classification 
#  and take a look
my_k_choice = 25

# so lets make a prediction!
knn_train_prediction = knn(train.X, new_grid.X, train_data$y, k=my_k_choice, prob=T)
# now we want to plot a boarder, so we need to figure out where P(green) = P(blue) = 0.5
prob = attr(knn_train_prediction, "prob")
# picking out only 0.5 line -> fancy plotting stuff
prob2 = ifelse(knn_train_prediction=="1", prob, 1-prob)
# reformat into a grid
prob2 = matrix(prob2, grid_size, grid_size)

contour(x1_new, x2_new, prob2, levels=0.5, labels="", lwd=4)
points(train_data$x1[train_data$y==1], train_data$x2[train_data$y==1], col="green")
points(train_data$x1[train_data$y==-1], train_data$x2[train_data$y==-1], col="blue")

# TAHDAH!! How lovely.

###############
### INPERSON CLASS: SLIDES ABOUT A "REAL LIFE" example
################

