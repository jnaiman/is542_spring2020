#################
# Tree Diagrams #
#################

# following: https://rdrr.io/cran/openintro/man/treeDiag.html
# FYI, also by hand here: https://daranzolin.github.io/2018-01-07-probability-trees/

#only have to run once:
#install.packages("openintro")
library(openintro)

# so, lets get R to make all these tree diagrams for us!

# lets check out the help
help(treeDiag)



# lets do a simple example - how likely is it that our
#   flight arrived on time, given that our 
#   luggage arrived on time?
#   P(f on time | l on time)?
# first, lets name the primary & secondary branchs
name_primary = 'Flight on time?'
name_secondary = 'Luggage on time?'

# Lets say:
# 90% probability our flight is on time
# 98% chance that given our flight is on time, our luggage is on time
# 10% that luggage is on time if flight is not


# the default order of options for our branches are "Yes" and "No"
#  so if there is a probability of 90% that our flight *will* be 
#  on time, or primary probility vector looks like:
prob_primary = c(0.9, 1-0.9) # YES, NO; marginal probabilities

# now lets say that there is a 98% chance that given
# our flight is on time, our luggage is on time
# P(l on time | f on time) = 98% - conditional
prob_l_given_f = c(0.98, 0.02) # YES, NO

# probability that luggage arrives on time
#  given that flight does *not* arrive on time
# P(l on time | f not on time)
# is 10% - so like, our luggage is on a different
#  flight which actually arrives on time
prob_l_given_not_f = c(0.1, 0.9) # YES, NO

# now, lets make a tree diagram
treeDiag(c(name_primary,name_secondary),
         prob_primary, 
         list(prob_l_given_f, prob_l_given_not_f))

# now, we have a few options to make this look nice 
#  or help us out with our calculations (hint, on HW)
treeDiag(c(name_primary,name_secondary),
         prob_primary, 
         list(prob_l_given_f, prob_l_given_not_f), 
         showWork = TRUE)


# back to the question:
# Q: P(f on time | l on time)?
# P(f on time | l on time) = P(f on time & l on time)/P(l on time)
prob_f_onTime_given_l_onTime = (0.882)/(0.882 + 0.01)
print(prob_f_onTime_given_l_onTime)
# so, its very unlikely that we'll be late if our luggage isn't

# **************************************************

# ok, lets go back to our original problem - SIR disease model
# "Imagine a population in the midst of an epidemic where 60% of 
#   the population is considered susceptible, 10% is infected, 
#   and 30% is recovered. "
# so, in this problem, we have 3 branches on the primary:
#  with P(sus) = 60%
#  P(inf) = 10%
#  P(rec) = 30%
prob_primary = c(0.6, 0.1, 0.3)
names_primary = c("Susceptible", "Infected", "Recovered")

# for the secondary branch, we have the info:
# "The only test for the disease is accurate 95% of the time 
# for susceptible individuals, 99% for infected individuals, 
#  but 65% for recovered individuals. (Note: In this case 
# accurate means returning a negative result for susceptible 
# and recovered individuals and a positive result for 
# infected individuals)."

# P(+ | sus) = 1-0.95 # accurate 95% of time, sus are *not* pos
# P(- | sus) = 0.95 # since should be negative if only sus
prob_acc_given_sus = c(0.05, 0.95)

# P(+ | inf) = 0.99 # accurate means positive
# P(- | inf) = 1-0.99
prob_acc_given_inf = c(0.99, 0.01)

# P(+ | rec) = 1-0.65 # accurate means neg
# P(- | rec) = 0.65
prob_acc_given_rec = c(0.35, 0.65)

# either positive or negative for each condition
names_acc_given_COND = c("positive","negative")

treeDiag(c('Group','Test Result'), 
         prob_primary, #primary prob
         list(prob_acc_given_sus, prob_acc_given_inf, prob_acc_given_rec), # secondary prob
         names_primary, # primary names
         names_acc_given_COND, # seconary branch names
         showWork = TRUE)

# now, we can just follow along lines:
# Q: P(inf | +) = P(inf & +)/P(+), where P(+) is sum of ALL ways to get a positive test
prob_inf_given_pos = (0.099)/(0.03 + 0.099 + 0.105)
print(prob_inf_given_pos)

# NOTE!! THIS ONLY DOES 2 BRANCHES!! 
# more than that you have to do by hand

###### PROBLEM ABOUT DNA TESTING KITS (SEE OPTIONAL RESOURCE) ######

# ANSWER:

# primary branch
names_approval = c("Approved", "Not Approved")
prob_approval = c(0.6, 1.0-0.6)

# secondary branch
# P(double | approval) = 80%
prob_doubled_approval = c(0.8, 1-0.8) # YES doubled, NO not doubled

# P(doubled | not approved) = 25%
prob_doubled_notApproval = c(0.25, 1-0.25) # YES doubled, NO not doubled

# make tree diagram
treeDiag(c('FDA Approval','Stock Doubled'), 
         prob_approval, #primary prob
         list(prob_doubled_approval, prob_doubled_notApproval), # secondary prob
         names_approval, # primary names, could also use default Y & N
         showWork = TRUE)

# Q1: P(approve & doubles) = 0.6 X 0.8 = 0.48
# Q2: P(approved | doubled) = P(approve & doubles)/P(doubles)
#       = 0.48/(0.48 + 0.1)
print(0.48/(0.48+0.1))

### BACK TO SLIDES FOR EXPECTATION & VARIENCE ###

