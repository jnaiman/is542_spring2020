# Let's say you are playing a game in which you want to roll a majority of 1's out of 6 sized die.
# So, if you have 10 rolls, you want to roll 5 or more ones let's see how likely this is.

# reset plot window
par(mfrow=c(1,1))

# Here, success is rolling a 1 - on a fair die this is simply 1 out of 6:
p = 1/6
#p = 0.9

# Lets start by having a game of 10 throws
nToss = 10
# What do we have to plot this?
help(dbinom)
# Hey!  that looks familiar!  
# Turns out R has a bunch of helper functions for the binomial distribution too!

# Ok, so our sequence now, instead of "x" which was our Z-scores, it is k-successes:
# Let's say we want to see how many successes we get up to nToss throws:
k_suc = seq(0,nToss) # note: we can only have whole numbers
barplot(dbinom(k_suc,size=nToss, prob=p), width=1, names.arg=k_suc, col='magenta', 
        xlab = 'k successes - number of 1s', 
        ylab = 'Prob',
        main='p=1/6; N=10', 
        space=0) # Note this space=0 is key to making our axis line up to our measurement!

# What does this graph represent again?
#  This is the probability of getting k number of 1s if we roll a total of N=10 times
# For example: the probability of not rolling a 1 at all in the game, out of 10 is ~0.15 or 15%.
#  Probability of rolling only one 1?  That is k=1, or about ~0.35, 35%.

# Lets plot our critera for winning - i.e. rolling 50% or more 1s.
#  In our game of 10 tosses, this is 5 or greater rolls of a 1.
abline(v=nToss*0.5+1, col='red', lw=2)
# Why +1 because we are starting our bars at 0 so we need to move 6 spaces!


# By eye we can see, the probability of >5 sucesses out of 10 throws is VERY small.
# We can even quantify this with:
print('Prob of majority of tosses being 1s = ')
print(1-pbinom(nToss*0.5, size=nToss, prob=p))
# note: 1- since we are looking for >, can also use "lower.tail" flip.


# What about if we toss 5 times? 100 times? **DO THIS**
# we see as n gets bigger, the situation gets worse!
# This is because we have an unlikely event = p < 0.5.
# The situation is reversed for p > 0.5 **GO BACK AND DO**

# This is because the binomial distribution loses its skew for large n => becomes more symmetric!
#  What distribution does it start to look like...?  We'll come back to this later!

# We now want to talk about the mean & SD of this distribution
### **BACK TO SLIDES***

##############################
# For the reader's interest, we won't cover:
# lets say I have 10 coins and I flip them all & ask if I 
# get heads (1) or tails (2)
# this is the same thing as asking if I flip 1 coin 10 times
# lets also say they have == probability
# rbinom(number of coins, number of times I flip all coins, probability of success)
outcomes = rbinom(n=10, size=1, prob=0.5)
print(outcomes)
# running the above a few times shows us that we get random heads or tails ~5 each
##############################

# BINOMIAL EXAMPLES

# Example 1:
#Suppose a university announced that it admitted 2,500 students for 
#   the following year’s freshman class. However, the university 
#   has dorm room spots for only 1,786 freshman students. If there 
#   is a 70% chance that an admitted student will decide to accept 
#   the offer and attend this university, what is the approximate 
#   probability that the university will not have enough dormitory 
#   room spots for the freshman class?

# Step 1: can we use the normal model?
n = 2500
p = 0.7
print(n*p)
print(n*(1-p))
# Both are > 10 so we are good to go!

# Step 2: Calculate mean and SD
normal_mean = n*p
normal_sd = (n*p*(1-p))**0.5

# Step 3: Use normal distribution to find this probability as a percentile
prob_norm = pnorm(1786,mean=normal_mean, sd=normal_sd, lower.tail=FALSE) # false because P(k>___)
print(prob_norm)

# Note: we could have also just as easily used R to do the binomial calculation:
prob_binom = pbinom(1786, size=2500, prob=0.7, lower.tail=FALSE)
print(prob_binom)

# So we can see they are very close to the same (even if slightly off)

# Example 2: 
# When telephones were just invented, there was a probability of 
# 80% of success in any attempt to make a telephone call. 
# What is the probability of 7 successful calls in 10 attempts?

# We can't use the normal model here since it small number stats.

# Because the binomial model is *discrete* we can actually use the "dbinom" function directly:
n = 10
p = 0.8
prob = dbinom(7, size=n, prob=p)
print(prob)
# Note that this is a feature of the binomial distribution and is not true of the normal
#  distribution!  The binomial distribution lives in a weird "inbetween" space of discrete
#  and continuous.

# Example 3:
# A manufacturer of silver pistons finds that on the average, 12% of her pistons are 
#   rejected because they are either oversize or undersize. 
# What is the probability that a batch of 10 pistons will contain
# (i) no more than 2 rejects?
# (ii) at least 2 rejects?

n = 10
p = 0.12 # note, here success = rejection!

# (i) 2 or less: so 0, 1, 2 rejects
#  We could do:
prob_way1 = dbinom(0,size=n,prob=p) + dbinom(1,size=n,prob=p) + dbinom(2, size=n,prob=p)
print(prob_way1)
# OR:
prob_way2 = pbinom(2,size=n, prob=p)
print(prob_way2)
# NOTE: asked for P(<= 2).  What if asked "probability of less than 2 rejects?) **LET THEM THINK**

# (ii) at least 2 rejects: so 2, 3, 4, 5, 6, 7, 8, 9, 10
# We could do what we did before but for many more additions of dbinom or...
#  we can note: P(>=2 rejects) = 1 - P(<= 1 reject)
# way 1:
prob_way1 = 1-(dbinom(0,size=n,prob=p) + dbinom(1,size=n,prob=p))
print(prob_way1)
# OR:
prob_way2 = 1-pbinom(1,size=n, prob=p)
print(prob_way2)  
# OR:
prob_way3 = pbinom(1,size=n, prob=p,lower.tail=FALSE)
print(prob_way3)  





