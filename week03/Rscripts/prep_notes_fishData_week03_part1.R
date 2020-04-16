# CLASS NOTES WEEK 3 - going over fish data again

# this assumes you have the fishdata set downloaded to your Downloads folder
#  Windows users: put in the chat window where your downloads are
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")

# Just a quick reminder of what we covered at the end of class:

# first, lets take a look at our original dataframe
#print(fishdata)
head(fishdata)
# also try: fishdata$ and see what autocompletes

# fishdata is something called a "dataframe"

# We can use this dataframe to take subsets like before, here is how we can re-do our histogram
mask = (fishdata$Country.or.Area == "Croatia") & 
  (fishdata$Quantity.Name=="Weight in kilograms")
fishdata_croatia = subset(fishdata, mask)

# make a histogram like before
hist(log10(fishdata_croatia$Trade..USD.))

# and then we wanted to see the croatian imports as a function of time:
mask = (fishdata$Country.or.Area == "Croatia") & 
  (fishdata$Quantity.Name == "Weight in kilograms") & (fishdata$Flow == "Import")
croatianImports = subset(fishdata,mask)

# plot imports as a function of time
plot(croatianImports$Year, croatianImports$Trade..USD., 
     xlab="year", ylab="Import trade in USD in Croatia")

# ... and we saw that we had multiple values and we wanted to do something about it
#  to do that we recalled that "croatianImports" is itself a dataframe:

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

# we can also aggregate by multiple things:
commodity_aggregate = aggregate(Trade..USD. ~ Commodity, data=croatianImports, sum)
par(las=2) # use this to rotate axis
plot(commodity_aggregate$Commodity, commodity_aggregate$Trade..USD.)

# we probably want a barplot though
barplot(commodity_aggregate$Trade..USD., names.arg = commodity_aggregate$Commodity)

# to fix the axis: default margin sizes are mar=c(5.1, 4.1, 4.1, 2.1) for the bottom, left, top, and right margins respectively
par(mar=c(20, 6, 4.1, 2.1)) # bigger bottom
barplot(commodity_aggregate$Trade..USD., names.arg = commodity_aggregate$Commodity)
# looks like cuttle fish are popular! (pop out to see)

# Let's go back to as a function of time
dev.off() # reset par

# We could do fancier aggregates with our base data, and we'll get to those later
#  in the class, but for now, this was just a taste.

plot(tradeUSD_by_year$Year, tradeUSD_by_year$Trade..USD.)


#EXTRA:
myfit = lm(tradeUSD_by_year$Trade..USD. ~ tradeUSD_by_year$Year)
abline(myfit, col='blue')

# LM - is a linear model this model a good fit?  We'll quantify this later!
