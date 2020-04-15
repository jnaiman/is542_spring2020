# Class Notes #2, Part 1 - Play with Class Survey results

# now we are going to continue to learn about how to read and look
# at data in R, again we are starting slow!

classData = read.csv("~/Downloads/formatted_class_answers.csv")
# lets make a vector of the different languages folks use
languages = classData[,5]
# can also ask for the "levels" of these catagories
print(levels(languages))

hist(languages) # should produce an error since "languages" is NOT a count - its a bunch of strings

# instead, we can play some tricks
table(languages) # shows how many "hits" for a specific language

hist(table(languages)) # plots something now, but is actually counting # of bottom level, not counts

# compare this plot to
barplot(table(languages)) # which sort of does what we want
# Note: there are some long strings that aren't showing 
# we can try changing one - for example, "python" should be "Python":
print(levels(languages)[2]) # let's replace this
levels(languages)[2] = "Python"
# now what does it look like?
levels(languages)

# let's try replotting
barplot(table(languages)) # which sort of does what we want


# but what is the y axis?
barplot(table(languages),ylab='Counts')
# ok, hard to see => maybe different colors for each?
# count 3 different answers
barplot(table(languages),ylab='Counts',col=c("red","blue","green"))
# psychadelic man :D


# ok cool, we see that a lot of folks use Python, and nobody uses R which is great because that is 
# what we are learning!

# we can do the same sort of thing for the length of time folks have been coding
time_in = classData[,6]
barplot(table(time_in)) # note: you will probably have to expand the x-axis window
# ok, so this tells us *something* but its hard to get a sense
# of the actual timescales since they are not ordered &
# are non-uniform increments of time

# lets reformat this data a bit!
time_min = c() # storage, save the min edge of our bins in years
time_max = c() # storage, save the max edge of our bins in years
# lets look at the options again
myLevels = levels(time_in)
print(myLevels)
# so ,we can see our options

# Now we get to practice doing a for-loop in R.  Note, it is similar to how it's done in 
#   Python, but there are a few differences!

# lets do a for loop and determine bins by hand:
for (i in 1:length(time_in)){
  # note we can also do: time_in[i] == '< 6 months'
  if (time_in[i] == myLevels[1]){ # < 6 months
    time_min = c(time_min,0)
    time_max = c(time_max, 0.5) # in years => 0.5 years = 6 months
  } else if (time_in[i] == myLevels[2]) { # 1-2 years
    time_min = c(time_min, 1.0)
    time_max = c(time_max, 2.0)
  } else if (time_in[i] == myLevels[3]) {
    time_min = c(time_min, 2.0)
    time_max = c(time_max, 4.0)
  } else {
    time_min = c(time_min,0.5)
    time_max = c(time_max,1.0)
  }
}

# lets say the mid-point of this data is the time
time = 0.5*(time_min+time_max)
hist(time)
# note of course, in reality, the bins are not the same size
# so we can manually change the break-points to more accurately represent the data
hist(time,breaks=c(0.0,0.5,1,2,4))
#hmmm, but that looks like density, not counts, how do we get counts?
help(hist)
# see there is a "freq" variable, set to TRUE to get counts
hist(time,breaks=c(0.0,0.5,1,2,4),freq=TRUE)
# We get an error telling us that the areas are wrong, but we have to think about 
#   what that means for us => do we actually care?
# So, density is telling us the frequency over the unit time, but thats not what we
# really want => we want counts with bars representing actual times.
# Again => picking which one depends on your data & how you want to present it 
#    and you have to make sure to think about it

# ----------------- Summary Statistics for our Dataset ---------------

# lets look at some summary stats for our data for how long folks have been programming
summary(time)
# lets over plot where these are
abline(v = mean(time), col = "blue", lwd = 2)
abline(v = median(time), col = "red", lwd = 2)
# lets add a legend 
legend("topright",c("Mean","Median"),col=c("blue","red"),lwd=2)
# something to think about - does the mean or median describe the distribution better here?
# what do they mean if the box sizes are different?

# now lets say we add the data of an old person in like myself that has 
#  been programming for a bit
new_time = c(time,15.0)
# lets overplot the mean and median of our new time on our old histogram
abline(v=mean(new_time),col="blue",lwd=4,lty=2)
abline(v=median(new_time),col="red",lwd=4,lty=2)

# so, we see that the mean changes a lot, BUT the median does not => this is very interesting to think
# about when we want to characterize our data

# lets try the same exercise with the standdard deviation
# first, lets replot our histogram
hist(time,breaks=c(0.0,0.5,1,2,4),freq=TRUE)
# lets add a horizontal line that shows the standard dev at the height of the plot
# NOTE: I'm just doing 1/2 STDDEV around the mean for plotting purposes.
#  Usually (and later on in class) we'll talk about full STDDEV's around the mean.
lines(c(mean(time)-0.5*sd(time),mean(time)+0.5*sd(time)),c(2,2), col="blue",lwd=2)
# lets add the 25th & 75th quantiles
lines(c(median(time)-0.5*IQR(time),median(time)+0.5*IQR(time)), c(1.5,1.5), col="red",lwd=2)
# again, lets add a little legend
legend("topright",c("STDDEV","Quartiles"),col=c("blue","red"),lwd=2)

# so, lets see how both of these change with adding my data point
lines(c(mean(new_time)-0.5*sd(new_time),mean(new_time)+0.5*sd(new_time)),
      c(2,2), col="blue",lwd=4, lty=2)
lines(c(median(new_time)-0.5*IQR(new_time),median(new_time)+0.5*IQR(new_time)), 
      c(1.5,1.5), col="red",lwd=2,lty=2)


# so, we also see that while the standdard dev changes a lot with new and very different data, the quantiles do not
# also something interesting to consider in characterizing data!

# -------- A few more things we can do with data ----------


# we can either do this or double click on "classData" in "Enviornment"
View(classData)


# We can make a dotchart of the programming time range
dotchart(time,xlab="Programming Age")
# we can sort of see that there are a lot of dots ~<1year, and a few at 3years
# note here: y-axis means nothing

# can also see that in a boxplot 
boxplot(time)
#maybe lets remind ourselves of what we are plotting
boxplot(time,ylab="Programming Age")
# for this last one, lets also overlay the boxplot for newtime too
boxplot(time,new_time, col=c("green","purple"))
# and lets add a little legend too
legend("topleft",c("Programming Age","New Programming Age"),fill=c("green","purple"))

# now we can see that again - the boxplots don't change a huge amount, only the new age
# one now includes an outlier point for my old-person programming age
#  there are also a bunch of parameters you can change with boxplots we aren't going to get into here

######## BACK TO SLIDES TO TALK ABOUT VARIABLE TYPES ########







