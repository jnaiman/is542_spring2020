#######################
#  LOGISTIC REGRESSION
#######################

#install.packages("alr3")
par(mfrow=c(1,1)) # reset
donner = alr3::donner

# outcome = 1 means alive, 0=dead
status = donner[,2]
age = donner[,1]

# Let's see the distributions of who lived/died with age.
boxplot(age[status==0],age[status==1],names = c('Died', 'Survived'), ylab='Age')
# This makes sense - older folks tended to die, while the young survived.
# It also makes sense that the spread of who died is large - probably young childern did not make it either.

# Let's also see if sex (again, treated here as binary) has any affect on death rate. 
#  This is easiest to see with a table:
sex = donner[,3]

print(table(sex,status))

# So, it seems like sex also has an effect on
# survival as 10 women died and 25 lived while 32 men died 
#  and only 24 lived.
# We can see this more clearly by calculating the table proportions:
print(10./(10+25)) #=0.29, or 29% of women died
print(32./(32+24)) #=0.57 or 57% of men died

# We can think of how we might want to build a model to predict who is going to live/die (categorical)
#  based on their age (numerical) and their gender (categorical).
# Here, "died or not" is an "either or" i.e. whether or not a person died

# How can we do this? Logistical regression to the rescue!

###### BACK TO SLIDES FOR LOGIT EXAMPLE #####
#### AFTER EXPLANING LOGIT #####

# Let's actually use R to fit such a linear model:
glm1 = glm(Outcome ~ Age, data=donner, family=binomial)
print(summary(glm1))

# Note that our summary is a bit different then the slide
# -> we are using a bit of a larger dataset.
# But in general, the results are much the same - the intercept is ~1 and the slope 
#  along age is negative - so older people are less likely to survive.

# What is the probability of survivial of a people of different ages? 
# Let's use our model to find out.
person_age = seq(0,80)

# To translate to the probability that someone survived we just need to do one extra step.
# We start with the "usual" thing for any linear model - beta_0 and beta_1 X (explanatory)
logit_prob_of_survival_age = glm1$coefficients[1] + glm1$coefficients[2]*person_age

# The Extra Step:
# Now we need to transform it back into probabilities using our link function
# recall: log(p/(1-p)) = logit_prob_of_survival_age
prob_of_survival_age = exp(logit_prob_of_survival_age)/(1.0 + exp(logit_prob_of_survival_age))

# lets plot!
plot(person_age, prob_of_survival_age, type='l', col="blue",ylim=c(-0.1,1.1))
points(donner$Age, donner$Outcome)

# Ok, but what on Earth does this mean??
# The simple interpretation is only possible in terms of log probabilities and
#  log probability ratios, but we can get an overall sense.

# Intercept: The log odds of survival for a party member with an age of 0 - about 75%.
# Slope: For a unit increase in age (being 1 year older) how much will the 
#  log odds ratio change, again, not particularly intuitive.
#  More often then not we care only about sign and relative magnitude.

# Admittedly, this is all pretty weird - do not worry!  We will have lots of 
# time to play around!  For example, we can make a function with our fit
#  just like we did for simple linear regression:
myProbFunction = function(p_age){
  # first, input our cofficients, per usual
  l_survival_age = glm1$coefficients[1] + glm1$coefficients[2]*p_age
  # Then just the one extra step with the "link" function:
  p_survival_age = exp(l_survival_age)/(1.0 + exp(l_survival_age))
  return(p_survival_age)
}
# now, we can ask questions like "based on a person's age of X"
#  what is the probability that they survived?
# 1. what if person is 20?
print(myProbFunction(20))
# 2. What about 80?
print(myProbFunction(80))
# 3. What about 5?
print(myProbFunction(5))

# These sort of make sense right - younger people
#  are more likely to have survived

# What about our other parameters?  Well, we can also 
# like with SLR, move to MGLM's and fit multiple 
# different sorts of parameters!!

######################
# MULTIPLE GLM!!
######################

# Now, we want to add in the parameter of the sex of the person (male or female) 
# and see how that effects their probability of survival

# call is very similar to what we have seen before
glm2 = glm(Outcome ~ Age+Sex, data=donner, family=binomial)
print(summary(glm2))

# note the "SexMale" call - and its slope => this means that 
#  in general if Sex == Male, there is a less chance of 
#  survival by a factor of ~1.1, i.e. we say "the odds are lower by ~1 if the person is Male"


# Let's replot our points with some identifiers for 
# sex and compare to our general model
plot(person_age, prob_of_survival_age, type='l', col="blue",ylim=c(-0.1,1.1))
points(donner$Age[donner$Sex == 'Male'], donner$Outcome[donner$Sex == 'Male'], col="magenta")
points(donner$Age[donner$Sex == 'Female'], donner$Outcome[donner$Sex == 'Female'], col="cyan")

# lets add a legend to keep things straight
legend("topright",c("Male","Female"),col=c("magenta","cyan"), pch="o")

# By eye it looks like there are many more male "dead" points clustered
# around p=0 (i.e. they died) and many more female "alive" points 
# clustered around p=1 (i.e. they survived)
# this makes sense with our noting of the negative slope for SexMale 
# in the summary of our fit

# Ok, now lets compare this model to the female & male lines alone
myMultipleProbFunction = function(p_age, MaleFemale){
  mfout = 0 # default female, 0
  if (MaleFemale == 'Male') {mfout = 1} # This is just a way to translate between an input string and M/F binary
  # first, input our cofficients
  l_survival_age = glm2$coefficients[1] + glm2$coefficients[2]*p_age + glm2$coefficients[3]*mfout
  # Extra Step: the link function
  p_survival_age = exp(l_survival_age)/(1.0 + exp(l_survival_age))
  return(p_survival_age)
}

# lets look at a few values for male & female
print(myMultipleProbFunction(20,'Male'))
print(myMultipleProbFunction(20,'Female'))

print(myMultipleProbFunction(10,'Male'))
print(myMultipleProbFunction(10,'Female'))

print(myMultipleProbFunction(80,'Male'))
print(myMultipleProbFunction(80,'Female'))

# lets also plot these things on our model
lines(person_age, myMultipleProbFunction(person_age,'Female'), type='l', col="cyan")
lines(person_age, myMultipleProbFunction(person_age,'Male'), type='l', col="magenta")

# and, again, just to keep our lines straight, lets plot a legend too
legend("bottomright",c("Male","Female"),col=c("magenta","cyan"), lwd=2)

# By eye, it really really looks like sex has an effect on 
# survival probobality, but how sure are we of this?

# lets re-look at the summary of our fit for some
#  hypothesis testing
print(summary(glm2))

# Here, simlar to for the linear model testing
# H0: beta_sex (slope of sex) = 0
# HA: beta_sex != 0
#   P(Z) where Z = (-1.06798 - 0)/0.48229 
#   P(Z) = 0.0268 < 0.05 so
#   we can say that we reject the null hypthesis that 
#   there is no logit relationship between age & survival probability



#### BACK TO SLIDES ######


