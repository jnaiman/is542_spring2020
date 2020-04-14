## Z-SCORES FROM PERCENTILES ##

# So, we've gotten percentiles from Zscores but lets to the opposite.

# As always, let's start with plotting a distribution:
par(mfrow=c(1,1)) # just to re-set things
x=seq(-3,3,length=200)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l")

# Let's try to reproduce that slide I showed.
#  First, let's say we want to know the Z-score associated with 40%:
percentile = 0.40 # 40%

# We can use the function "qnorm" like we've been using dnorm and pnorm, but 
#     now it will give us the Zscore corresponding to our percentage
Zscore_40 = qnorm(percentile,mean=0,sd=1)
# Note the very similar calling sequence to pnorm and dnorm.  This will also be a 
#  continuing cause of confusion.  If you can remember that "p" = percentile (from Z-score) 
#  and "d" is density (probability density function) then "q" is basically what is left and 
#  is the Z-score-from-percentile.  That's the best way I've got to remember it.

# in case we've opened a new file we need to re-source our function:
source('~/Downloads/plot_polygons.R')

# Now that we have a Z-score let's plot this over our figure using our polygon function
x=seq(-3,Zscore_40,length=100)
y=dnorm(x,mean=0,sd=1)
draw_polygon(x,y)

# We can also make our plot fancy like the slide
text(-1,0.1,"0.40") # first, let's plot text on our plot
# And we can put a nice little arrow for fun
arrows(0.5,0.1,-0.2,0,length=.15)
text(0.75,0.12,Zscore_40)

# Note: another way to think of this Z-score and percentile is that 
#  the probability of getting a Z-score of -0.25 is about 40% in this distribution.

#### BACK TO SLIDES FOR A PRACTICE PROBLEM #####