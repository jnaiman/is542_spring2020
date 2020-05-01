# ---- INTRO TO BINOMIAL DISTRIBUTION ------

# ok, first lets look at an example
#  we flip a coin and call a head a "SUCCESS"
#  and tail a "FAILURE"
#  note: this is just to use the nomenclature
#  of the binomial distribution - we could 
#  as easily call tails a success

# We can use the function "sample" to simulate
#  a fair coin toss
#help(sample)
nTosses = 10
samples = sample(2,size=nTosses, replace=TRUE) # replace=TRUE just means we start again with equal prob of heads/tails
#print(samples)
# now, lets re-map 1=>0 & 2=>1 just for consistency in using the binomial formula
samples[samples==1] = 0 # tails, failures
samples[samples==2] = 1 # heads, successes
#print(samples)

# What do we expect the # of successes & failures to be?
# Well, for a fair coin it should be p=0.5
# What about for our sample above?
prop_sample = sum(samples)/nTosses
print(prop_sample)
# If we run this a few times, we see that we get 
#   widely varying answers for our *sample* proportion

# What if we re-run with more coin flips?
#  ** go back and change nTosses to ~100 or 1000**
# Now we get things that are much closer to 0.5 if we increase nTosses.
# This illustrates the difference between the population & sample means => this is important
# For the next topic (Ch. 4) we'll cover which is trying to estimate things like 
#   the population mean from the sample mean & quantifying how well we con do this.

# Lets explore this further: what is expectation & SD?

# *****GO TO R SLIDE to take about mean and SD *****