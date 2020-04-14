# Notes for Class #1

# data from mozzerlla consumption vs civil engineering PhDs awarded
# Note how vectors are defined here with a c()
year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
mozz <- c(9.3, 9.7, 9.7, 9.7, 9.9, 10.2, 10.5, 11, 10.6, 10.6)
edocs <- c(480, 501, 540, 552, 547, 622, 655, 701, 712, 708)
# Note here that I can also use "=" for variable assignment

# show that the things show up in the data environment
plot(year,mozz)
# ok, but maybe we want nice red dots like on the website
plot(year,mozz,col="red")
# but hey, what about connecting them with lines though?
plot(year,mozz,col="red",type="b")
# well, the "year" label is fine, but what about "mozz" on the y axis?
plot(year,mozz,col="red",type="b", ylab="Per Capita Mozzerella Consumption in lb")

# now lets plot the # of engineering docs as a function of time as well
par(new=TRUE) # we use this to over plot
plot(year,edocs,col="black",type="b")

# but wait! the axis look horrible!  Lets try this again:
par(mar = c(5,4,4,4) + 0.1) # this just adds a buffer onto the right axis
plot(year,mozz,col="red",type="b", ylab="Per Capita Mozzerella Consumption in lb")

# lets change the color of the axis for Per Mozz consumption
axis(2,col="red",col.ticks="red",col.axis="red",col.lab="red")
mtext("Per Capita Mozzerella Consumption in lb", side=2, line=3, col="red")

# now, lets plot the engineering data
par(new=TRUE) # we use this to over plot
plot(year,edocs, type = "b", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
axis(side = 4)
mtext("Engineering Doctorates", side = 4, line = 2)

#hurray!  We have a cool looking plot!

# ok, lets just look at the cheese consumption data => maybe we want to know what is the overall
# per capita cheese consumption
# we can do this with the histogram 
hist(mozz,xlab="Per Capita Mozzeralla Consumption in lbs")
# here the y-axis is showing the frequency of a certain value - so how often a value shows up in 
# our data
# it looks like ~9.75 and 10.75 pounds are the most frequent values
# its worth noting this distribution is somewhat "bimodal" in that there are sort of 2
# distint peaks
# however, the number of measurements is very small here, so its hard to tell for sure
# this could be a "unimodal" distribution peaked around 9.75 lbs

# we can also print out some summary statistics:
summary(mozz)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  9.30    9.70   10.05   10.12   10.57   11.00 
# we can also calculate these things seperately
mean(mozz)
median(mozz)
var(mozz) # this is the "varience" which we'll discuss in a minute
sd(mozz) # this is the standard deviation

# lets try a larger dataset.
# check out data.gov or just google "statistical datasets"
# or there are some embedded in "R" as well, but lets try reading from a file first.

# This is a file of random "tests" of before and after.
# this is just a toy dataset but lets just say its a measure of 
#  resting heart rate before & after a drug trial
# Lets see where my example data is stored: **NAVIGATE**
mydata <- read.csv("~/Downloads/l1_trialData.csv",header=TRUE,stringsAsFactors=FALSE)
#print(mydata)
# We can compare the print statement to **what is in the file** as well.  
#  notice there is an NA - this just means there is no data there.

# grab rows and columns
before = mydata[,1]
after = mydata[,2]

# if we print our summaries
summary(before)
summary(after)

## GO BACK TO SLIDES ##

# ###------------------ Relivantnt to the HW ---------------
# # we see that the before one has an NA -> this just means there is no value there
# #  We can see this if we look at the file **go to file** there is a missing value
hist(before)
# # but wait, lets label it
hist(before,main="Histograms")
# # lets label x & y
hist(before,main="Histograms",xlab="count")
# # now, lets add in the histogram of the after
hist(after,add=T)
# # right, but its hard to see what is going on
hist(after,col=rgb(1,0,0),add=T)
# # hmmm... but I feel like I want colors for both and maybe some transparency so lets try
hist(before,main="Histograms",xlab="Count",col=rgb(1,0,0,0.5))
# # so look its a bit see through - now lets add the other one on in green
hist(after,col=rgb(0,1,0,0.5),add=T)
# # now lets add a nice legend so we can tell which is which
legend("topright",c("Before","After"),fill=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5)))

# ------------ Packages and Data ----------------
# Let's look at how we can install data from R via packages
## packages
install.packages("survival")

# if we have already installed this package, we can load it like so:
# for later use, maybe a different code
library(survival)

# this contains data about survival rates of cancer
# lets look at lung cancer.  Easiest thing to do is to just
#   print out what we have for this dataset:
print(survival::lung)

# we can also check out the help pages for this package
help(package="survival")
# **NAVIGATE: to the "lung" section **

# from this we see that there are things stored in this dataset like the person's age
#  in years, their sex, and how long they've survived.

# We can use this dataset much like the one we loaded from the CSV file:
time_in_days = survival::lung[,2]
hist(time_in_days)

# There are also datasets "embedded" in R that are easy to use as well:
# to load a list of all R packages 
data() # should pop up a list of internal data packages

# let's try one:
data("ChickWeight") # loads a particular dataset

# get more info:
help("ChickWeight") # displays more info about a package in help window

# or, if that is too confusing, we just look at the data:
print(ChickWeight)

# So, this shows the weight of a baby chicken on different diets

# note that there are some examples on the "help" page.  We can run these with:
example(ChickWeight)

# NOTE: we won't get into what any of these plots mean just yet, but this is more of an FYI for you
#  about some of the features of R!
