# more details: http://www.r-tutor.com/elementary-statistics/inference-about-two-populations/population-mean-between-two-independent-samples

# check out datasets
data()

# for this, we are going to use the mtcars dataset, so lets 
#  load this
#  **check if folks have trouble**
data(mtcars)

# check out header & part of dataset
head(mtcars, 6)
# so, what do all of these mean?
help(mtcars)
# so we see that the first colum is miles/gallon
# then we have number of cylinders, displacement (no idea), 
# horsepower, real axel ratio (again, not sure), how fast its acceleration is (I assume)
# engine shape, wheter its manual or automatic transmission, number of gears and 
# number of carbureators

# for this exercise, what we want to know:
# for am => automatic or manual
# 0=automatic, 1=manual

# grab mpg for manual & auto
mpg_manual = subset(mtcars$mpg, mtcars$am == 1)
mpg_auto = subset(mtcars$mpg, mtcars$am == 0)

# **print out both of these, note diffenet sizes

# now, lets see how different they are
print(t.test(mpg_auto, mpg_manual))

# OK, what does this say?
# go through:
# say with a 95% confidence the difference between the means is from 3.2 - 11.3 mpg

# note it says: "true difference in means is not equal to 0" so auto != manual

# this was a 2 sided test
# lets say we had reason to beleive that mpg_auto < mpg_manual
# alternative: that "x" has a greater mean than "y"
print(t.test(mpg_auto, mpg_manual,alternative="greater"))
# can check out p-value specifically
print('p-value')
print(t.test(mpg_auto, mpg_manual,alternative="greater")$p.value)
# we can see this is >> 0.05
#  This makes sense since manual cars generally get better mpg than automatics

# Extension:
#   Part 1: Select for engine shape - are the mean mpg's the same? 
#   Part 2: While the number of samples is too small, 
#           we might be tempted to do this calculation with a normal distribution.
#           Re-do this test using the normal distribution.
#           Recall: SE = ( SD1^2/n1 + SD2^2/n2 )^0.5

# BONUS:
# plot all data with "pairs" function
#  any relationships you want to explore?
# for example: means of mpg for different #'s of cylinders
#  or, horse power as a function of different #'s of cylinders, or auto vs. manual
#  or, what is the probability that hp of autos are higher than manuals? t-test & normal model
#  or, make a cut for "low and high" horse power, and see how mpg depends on this





### ANS:
# Part 1:
mpg_v = subset(mtcars$mpg, mtcars$vs == 0)
mpg_s = subset(mtcars$mpg, mtcars$vs == 1)

print(t.test(mpg_v, mpg_s))
# p-value = 0.0001098 << 0.05 => so, they are different

# Part 2:
SE_combined = (sd(mpg_v)^2/length(mpg_v) + sd(mpg_s)^2/length(mpg_s))^0.5
Zscore = (mean(mpg_v)-mean(mpg_s))/SE_combined # -4.66
pvalue = 2.0 * (pnorm(Zscore)) # 2* for 2 sided test
print(pvalue)
# note: if did mean(mpg_s)-mean(mpg_v)
#  then: * pnorm(Zscore,lower.tail=F)

