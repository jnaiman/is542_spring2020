# CLASS NOTES WEEK 2, Part 2 - Looking at the UN-fish database dataset

# this assumes you have the fishdata set downloaded to your Downloads folder
#  Windows users: put in the chat window where your downloads are
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")

# let's make some vectors out of this data - you can use the data as 
#  a dataframe (which we'll get to later) but since many folks have a Python 
#  background, we might be more used to doing things with vectors.

# make some vectors, first country:
country = fishdata[,1]
# how about year of data?
year = fishdata[,2]
# how about type of fish
type = fishdata[,3]
# how about transaction type? (import, export, re-export/import)
transaction = fishdata[,4]
# how about the cash amount of the transaction?
trade_usd = fishdata[,5]
# how about the weight of the fish in kg?
weight = fishdata[,6]
# how about the quantity name?
quant_name = fishdata[,7]
# looks like some of of the "quantity" measures are weight, or # of items, or nothing


# ------------- GIVE up to here ------------------

# (I) Data exploration

# The first step here is to explore our dataset - let's look at one vector at a time:
# Country of each case:
barplot(table(country)) # note: if you stretch the plot window you see more/less data
# what are the different countries?
print(levels(country))

# how about year of data?
barplot(table(year))

# how about type of fish
barplot(table(type))
# since its hard to see the labels, maybe we want to print out levels by hand:
levels(type)
# So, all-in-all we see we have about 87 different types of fish import/export in this dataset

# how about transaction type? (import, export, re-export/import)
barplot(table(transaction))

# how about the cash amount of the transaction?
hist(trade_usd)
# We can see here that this histogram tells us very little
#  Why? Well let's print out the values of trade_usd and take a look:
print(trade_usd)
# These numbers are very large overall.  One option is that we can 
#  divide by something like $1000 and see what that looks like:
hist(trade_usd/1000., xlab='Trade in $1000 USD')
# Well that didn't do too much good!  Why is that?  Let's look at some summary stats for this varible:
print(summary(trade_usd))
#  So, the min seems to be $0-$1 and the max is $5.2 billion dollars!
#  You can see that the Median & mean are very different, and the 
#  IQR extends from 1000 to almost 10 million 
# When we have data over such a huge range that we want to take a look at, 
#  one good idea is to take the log and plot that.
# Recall log10 means "log base 10" 
# what about a log scale plot?
hist(log10(trade_usd))
# Now we can see a lot more detail - what this plot means is
#  that the peak of the distribution is log10 ~ 5 or
#  at 0.1 million dollars (10^5 dollars)

# how about the weight of the fish in kg?
hist(weight)
# hard to see - let's look at a summary again:
print(summary(weight))
# so again, min & max have a wide range, and a large spread in quartiles
#  Let's try a log plot
hist(log10(weight))
# That this looks similar to the trade_usd histogram makes sense inuitivley - 
#  more weight of fish probably corresponds to more money flowing.

# how about the quantity name?
levels(quant_name)
# looks like some of of the "quantity" measures are weight, or # of items, or nothing
# Since this is non-numeric, and only 3 value, let's just look at the table
table(quant_name)
# It looks like most entries are in kg, and only a few are in #'s
#  A few specify No Quantity - we might want to be careful that we are
#  comparing "apples to apples" - i.e. "like things to like things" and make sure 
# we are not comparing measurements in kg to measurements in "# items"

# ------------------- Further data exploration and some beginning Stats Functions -----------

# I'm going to show a few stats functions that we'll use later in class
#  We'll go over them in a lot of detail later, but for right now, I'm 
#  just showing an example of how one might use R to explore a dataset and 
#  try to learn stuff about it.  I'll say a lot of "this will be quantified later" and 
#  it will!  So don't get frustrated if its weird or vague at this point!

# Ex. 1: Croatian Imports
# Let's start by grabbing a subset of our data.  We can do this by "masking" out our 
#  dataset and only looking at these specific values using "boolean" operators
# Let's say I want *only* Croatian data:
mask = country=="Croatia" # Feel feel to pick your own country! recall: print(levels(country)) to take a look
# I can then make a subset of this dataset to work with, for example, to plot the total
#  amount of trade in USD from Croatia:
trade_usd_croatia = subset(trade_usd,mask) 
# here ^ I use the "subset" function to grab only data with a country code of "croatia"

# what does this histogram look like?
hist(trade_usd_croatia)
# again, we probably want to do a log10-based histogram
hist(log10(trade_usd_croatia))

# we can make more complex masks, for example, remember how we only wanted to 
#  compare "like to like" in terms of how things are weighted/counted?
# Let's also "mask out" only Croatian data that is measured in kg:
mask = (country=="Croatia") & (quant_name == "Weight in kilograms")
trade_usd_croatia_kg = subset(trade_usd,mask)
# let's overplot this new dataset on our old histogram
hist(log10(trade_usd_croatia_kg),col=rgb(1,0,0),add=T)
# it turns out this was an important addition to our mask - it changes how things
#   look at the lowest trade in USD values

# finally, we can further subset and look at only how much import trade they are doing:
mask = (country=="Croatia") & (quant_name == "Weight in kilograms") & (transaction == "Import")
trade_usd_croatia_kg_import = subset(trade_usd,mask)
hist(log10(trade_usd_croatia_kg_import),col=rgb(0,0,1),add=T)

# and of course, no plot is complete without a legend!
legend("topleft",c("Croatian Trade","Croatian Trade (kg)", "Croatian Imports (kg)"),
       fill=c(rgb(1,1,1),rgb(1,0,0),rgb(0,0,1)))

# This subsetting is also useful for looking at summary statistics of this dataset:
print(summary(trade_usd_croatia))
print(summary(trade_usd_croatia_kg))
# with the difference between these two, we can already see that if we select
#  for things weighted in kg we find a slightly higher mean/median, etc
# This sort of lines up with what we expect from looking at the histograms

# let's also finally compare the imports to the exports from Croatia
mask = (country=="Croatia") & (quant_name == "Weight in kilograms") & (transaction == "Export")
trade_usd_croatia_kg_export = subset(trade_usd,mask)
hist(log10(trade_usd_croatia_kg_export),col=rgb(0,1,0),add=T)

# and, obviously, update our legend:
legend("topleft",c("Croatian Trade","Croatian Trade (kg)", 
                   "Croatian Imports (kg)", "Croatian Exports (kg)"),
       fill=c(rgb(1,1,1),rgb(1,0,0),rgb(0,0,1),rgb(0,1,0)))

# By eye we can see that they seem like the mean/medians might be different
#  but let's use summary to see:
print('IMPORTS')
print(summary(trade_usd_croatia_kg_import))
print('EXPORTS')
print(summary(trade_usd_croatia_kg_export))
# again, our histogram seems to be accurate - the export median < import
#  though note this is not true of the mean
#  This makes sense because if we look at the STDDEV of each:
print(sd(trade_usd_croatia_kg_import))
print(sd(trade_usd_croatia_kg_export))
# the sd of the export > import meaning there is a larger spread
#  of trade in USD in the export dataset so it makes sense the 
#  mean might be different from the median
# Q: skewness of this histogram?

# Q: Can we accurately say for sure that the medians between these
#  are different?  Can we quantify how sure we are these means or medians
#  are different? 
#  ==> more on these concepts later in class.

# Ex 2. Plotting by time
# We can also check out relationships between the data in other ways
# Like how things change over time.
# To make sure we are comparing like-to-like, we should also apply
#  whatever mask we are using to our time variable.
#  Let's say we want to see how Croatian imports change with time:
mask = (country=="Croatia") & (quant_name == "Weight in kilograms") & (transaction == "Import")
year_croatia_import_kg = subset(year, mask)

# now we can plot the imports into Croatia as a function of time:
plot(year_croatia_import_kg,trade_usd_croatia_kg_import,
     xlab="Year",ylab="Import Trade in USD in Croatia")

# so this has multiple values - what are they?  They are for each type of fish
# If we want to sum along each year there are plenty of fancy ways to do this.
# One thing that is nice about R is its use of dataframes.  We'll work more with this 
#  later, but as an intro, we could either use our original dataframe, or create a new
#  dataframe out of our subset data.  Let's try the last option.

# first, lets take a look at our original dataframe
#print(fishdata)
head(fishdata)
# also try: fishdata$ and see what autocompletes

mask = (fishdata$Country.or.Area == "Croatia") & (fishdata$Quantity.Name == "Weight in kilograms") & (fishdata$Flow == "Import")
croatianImports = subset(fishdata,mask)

#print(croatianImports)
head(croatianImports)
# so you can see from ^ that we get the same type of dataframe, or data list, except
#  now if we do croatianImports$Country its only croatia

# we'll talk more about functions later, so don't worry if this doesn't make sense now
#  But we can use something called the aggregate function to aggregate the "Trade USD"
#  variable in our dataframe by year:
tradeUSD_by_year = aggregate(Trade..USD. ~ Year, data=croatianImports, sum)
# What does that ~ mean??  In this case it means "aggregate Trade USD by Year"
#  But in other functions it means different things! We'll look at this later 
#  in class as well

plot(tradeUSD_by_year$Year, tradeUSD_by_year$Trade..USD.)

# We could do fancier aggregates with our base data, and we'll get to those later
#  in the class, but for now, this was just a taste.

#EXTRA:
myfit = lm(tradeUSD_by_year$Trade..USD. ~ tradeUSD_by_year$Year)
abline(myfit, col='blue')

# LM - is a linear model this model a good fit?  We'll quantify this later!
