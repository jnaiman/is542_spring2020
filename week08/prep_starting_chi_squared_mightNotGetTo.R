# Script for class #9

# --------------------------
#   Basic Chi-squared things
# --------------------------

# lets start with our usual - plotting these distributions
x = seq(0, 50, length=200)
dist = dchisq(x, df=3)
plot(x, dist, type='l', ylab='Probability Distribution of Chi-Sq', xlab='Chi-Squared Value')

# ok, now lets loop through and plot a few different degrees of freedom
dfs = c(5, 10, 30)
# lets make them all different colors too
colors = c("red", "blue", "magenta")
for (i in 1:length(dfs)){
  lines(x, dchisq(x,df=dfs[i]), col=colors[i])
}

# lets add another little legend for making things look nice
# making sure we include the first one
dfs = c(3, dfs)
colors = c("black",colors)
legend("topright", as.character(dfs), col=colors, lwd=2)

# so we can see that the distribution slides to higher chi-squared values
#  for higher degrees of freedom

# it even looks like as DOF increases it approaches a normal curve...
# lets plot something to guide our eye...
lines(x, dnorm(x,mean=29,sd=7),col="green")
# so this green normal curve doesn't overly the highest DOF curve, but it looks close

# this is actually a feature of chi-squards - as the DOF increases, it approaches a normal curve
#  this once again comes from the Central Limit Theorem
# again, this is why normals are so famous!
# **** WHY?? ***

# lets go back to just one plot:
# now, lets say we have some made up distribution that has 8 DOF, 
#and when we calculate our chi^2, we find a value of 10
#  what is the pvalue of this?
df = 8
test_stat = 10

x = seq(0, 50, length=200)
plot(x, dchisq(x, df=df), type='l', ylab='Probability Distribution of Chi-Sq', xlab='Chi-Squared Value')
pvalue = 1-pchisq(test_stat, df=df)
print(pvalue)
# also can do:
pvalue = pchisq(test_stat, df=df, lower.tail=FALSE)
print(pvalue)

# now lets plot what this corresponds to on our graph
x2=seq(test_stat,50,length=200)
y2=dchisq(x2,df=df)
# using our old friend and foe - the polygon function
#  *now* might be the last time we use it... maybe
polygon(c(test_stat,x2,0),c(0,y2,0),col="red")

# ok, so these are some simple things, similar to things we've done with 
# the normal & binomial distributions

# now lets see about reproducing the example we just went through

#--------------------
# Labby's Dice
#--------------------

# first lets make a vector holding each die roll
die_roll = c(1, 2, 3, 4, 5, 6)
# now lets plug in what Labby found for the observed rolls
obs = c( 53222, 52118, 52465, 52338, 52244, 53285 )
# lets also fill in what we expect from theory
# recall
number_of_dice_rolled_at_once = 12
number_of_rolls = 26306
expected_value_of_each_side = number_of_dice_rolled_at_once * number_of_rolls/6
# now, since this is true for all sides of the die we can
# for-loop over this
expected = c()
for (i in 1:6){
  expected = c(expected,expected_value_of_each_side)
}

# before we get to calculations, lets plot these results
barplot(obs, names.arg=die_roll)
# lets overplot what we expect from "theory"
abline(h=expected_value_of_each_side, col="blue")
# so we can see at the very top there, indeed it looks like 
#  1  & 6 are more frequent then the others

# now, lets calculate each element of the total chi squared test statistic:
# recall X = sum( (observation-expected)^2/expected )
chi_vals = (obs - expected)**2/expected
#print(chi_vals)
test_stat_chi = sum(chi_vals)
#print(test_stat_chi)

# now, we need to compare this to the appropriate chi-squared distribution
df = length(obs)-1
# print(df) = 5
pvalue = pchisq(test_stat_chi,df=df, lower.tail=F)
# print(pvalue)

# we see that this pvalue is much less than our typical level of confidence of 
# alpha = 0.05 so we can reject the null hypothesis
#  i.e. the observed die rolls are indeed different than the expected values
#   and so, we say this tests provides convincing evidence that the dies are biased

# ok, lets move on and work on some more real-life examples of 
#  chi-sq tests in action

#------------------
# Polling Data
# -----------------

# lets look at voting data from the 2000 presidential election

# just run this once
# also could be: library("poLCA", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("poLCA")

# load election data
data(election)

# **STOP HERE & SEE THAT EVERYBODY HAS THIS INSTALLED OK**


# lets look at a few variables in particular
#print(election$VOTE3)
#print(election$PARTY)
#print(election$EDUC)
#print(election$GENDER)

# now lets process the data a bit => turn numerical values into catagorical
election$VOTEF <- factor(election$VOTE3)
election$PARTY <- factor(election$PARTY)
election$EDUC <- factor(election$EDUC)
election$GENDER <- factor(election$GENDER)

# now, lets translate the different "levels" from 1, 2, NA to actual values
# look at help(election) to copy paste
#  also,posted on moodle
levels(election$VOTEF) <- c("Gore","Bush","Other")
levels(election$EDUC) <- c("<8th", "9-11 grades","HS Diploma", ">12 years schooling", "Junior College Degree", "BA/BS", "Adv. deg.")
levels(election$GENDER) <- c("Male", "Female")
levels(election$PARTY) <- c("Strong Democrat", "Weak Democrat", "Independent-Democrat", "Independent-Independent","Independent-Republican", "Weak Republican", "Strong Republican")

# install a nice plotting package
#install.packages("ggplot2") # just run once
#install.packages("tabplot", dependencies = TRUE) # just run once, make sure you get dependancies

#####install.packages("dplyr", dependencies = TRUE)
######install.packages("tidyverse")


###### NOTE HERE: MAKE SURE YOU EXPLAIN WHAT EACH ROW IS!!!! ####
### PERCENTAGES!!! MAKE PLOT!!!


#require(ggplot2)
require(tabplot)

# ignore all the errors => just masking out variables from other packages

# make pretty plot!
tableplot(election, select=c(VOTEF,PARTY,EDUC,GENDER),pals=list("BrBG"))

# ***PAUSE HERE TO SEE IF INSTALLED OK ***


# see all the red => these are missing info
# esp. in who they voted for => lots of people didn't respond here, don't know why

# we could certainly explore the data more and see who didn't answer 
# this question, hence the "missing" value and why 
#  for example, by eye it looks like more women then men
# didn't answer, and also lower levels of education 
# didn't seem to answer this (EDUC)

# anyway, lets though look at "full" datasets
#  i.e. data in which all answers are filled in
election2 <- na.omit(election[,c("VOTEF","PARTY","EDUC","GENDER")])

# note, we can sort things based on different columns
# ** play with this by changing sort=1 to other numbers!!!**
tableplot(election2, select=c(VOTEF,PARTY,EDUC,GENDER),sort=1,pals=list("BrBG"))

# lets ask something obvious - is there a relationship between voting party
#  and who they voted for?
# lets state the null & alternative hypthesis
#H0: There is no relationship between the party affiliation (7 levels) 
#    and voting results (Bush, Gore, Other) in the population.

#HA: There is a relationship between the party affiliation (7 levels) 
#    and voting results (Bush, Gore, Other) in the population.

# Veryify condtions for using chi-squared
#  Independence: probably, but we aren't sure, there are also probably biases in 
#   how this population was sampled

# now, lets make a table of just these variables:
####electable <- tally_(~PARTY+VOTEF, data=election2)
electable = table(election2$PARTY, election2$VOTEF)
# print(electable)

# note: this is going to involve a chi-squared test for a 2D table
# table df: (# rows - 1) * (#columns - 1) = (7 - 1) * (3 - 1) = 12

# lets see if the chi^2 is going to work
print(chisq.test(electable)$expected)
# we get an error - this is because in "Other" & "I-I" we have < 5 samples as our expected value
#  in reality, chi^2 won't be as accurate here & we should do another test, but lets proceed

print(chisq.test(electable))

# note our pvalue is SUPER small so we can reject the null hypothesis
#  and assume that party affilation & who they voted for are related
#  which makes sense

#### FOR LIVE CLASS: HAVE THEM DO THEMSELVES!!! ####

# lets do another quick one: Is gender & who they voted for related
#H0: There is no relationship between gender (2 levels) 
#    and voting results (Bush, Gore, Other) in the population.

#HA: There is a relationship between gender (2 levels) 
#    and voting results (Bush, Gore, Other) in the population.
gentable = table(election2$VOTEF, election2$GENDER)
print(gentable)
print(chisq.test(gentable))

# here we see that our pvalue is 0.0015 < 0.05 so we can 
# reject the null hypothesis that these two variables are not related,
#  and gender and party have a relationship

# any others?  Play with?

