# from: http://www.stat.columbia.edu/~martin/W2024/R3.pdf

# Suppose a drug company has 3 formulations of a drug for pain relief for migranes.
# They randomly assign 1 of the 3 forumulations to 27 volunteers to take during
#  their next migrane episode & rate their pain levels from 1-10.

# results look like:
drugA = c(4, 5, 4, 3, 2, 4, 3, 4, 4)
drugB = c(6, 8, 4, 5, 4, 6, 5, 8, 6)
drugC = c(6, 7, 6, 6, 7, 5, 6, 5, 5)

# lets first make boxplots to check out what these look like
boxplot(drugA, drugB, drugC, names=c("A", "B", "C"), xlab="Drug", ylab="Pain Scale")

# By eye it certainly looks like A is better at controling pain than B or C, but how sure are we?

# Let's check our conditions to use an ANOVA model:
# 1: Independance - this is something we have to assume, probably ok given that the 
#    subjects are randomly assigned to a group.
# 2: Normality? We'll let's try making some plots and taking a look:
par(mfrow=c(2,3)) # for each drug - histograms and QQ-plots
# (i) hists
hist(drugA)
hist(drugB)
hist(drugC)
# (ii) qq plots
# drugA
qqnorm(drugA)
qqline(drugA)
# drugB
qqnorm(drugB)
qqline(drugB)
# drugC
qqnorm(drugC)
qqline(drugC)

# With such small numbers, its hard to really tell how normal these samples are,
#   but they all look "OK", (maybe B less so), so we'll assume they are normally distributed going forward.
# Again, this is a judgement call on our parts at this point.

# 3: similar variences?
print(var(drugA))
print(var(drugB))
print(var(drugC))

# Again - these are a bit different, but, they all are within a factor of 4 of eachother.
#  So, again, another judgement call, but lets assume the variences are about == for this problem & carry on.

# To continue on further, we need to move around our data a bit:
pain = c(drugA,drugB,drugC) # concatinate all together
drug = c(rep("A",9), rep("B",9), rep("C",9)) # just some fancy labeling
print(pain)
print(drug)
# We'll smoosh these together as a dataframe (like a pandas dataframe if you like Python!)
migraine = data.frame(pain,drug)
print(migraine)

# Now, if we remember back to some of our R exercises at the beginning of class:
#  aov(response ~ factor, data=data_name)
#  where here "response" responds to a "factor"
#  i.e. the response is pain and the factor is the drug in our example
aov_results = aov(pain ~ drug, data=migraine)
print(aov_results)
# or for more info:
print('------')
print(summary(aov_results))
## NOTE: there are many ways to do this test, we are doing the defaults

# What we really want to pay attention to here is the F-value = (variability between groups)/(var in groups)
#  and the p-value which, again, is just the area under the F-distribution which tells us
#  something about how a-typical the difference in the means are.

# Because p-value ~ 0.0003 << 0.05, our usual level of significance, we say we can
#   reject the null hypothesis & there is a difference in the means.

##### BACK TO SLIDES ######

### after bonferri comparisions, come back ###

# To do this with our current drug trial example, lets do it 
# with a pairwise t-test, so like the t-tests we've done 
# before, but it will calculate this on pairs
# pairwise.t.test(response, factor, 
#                 p.adjust = method, 
#                 alternative = c("two.sided", "less", "greater"))

# here, we don't know which drug is better at pain management, so lets 
# assume two sided => the default
print(pairwise.t.test(pain, drug, p.adjust="bonferroni"))

# What does this table tell us?
# We see that the means are not significantly different between drugs B & C (pvalue=1).
# But both are significantly different from drug A => so the mean pain level is 
#    significantly different for Drug A.

# If we had reason to expect drug A to have lower measurements we could test with:
print(pairwise.t.test(pain, drug, p.adjust="bonferroni", alternative="greater"))
# But this gets into "data digging" territory!

####### IN A GROUP ########
# adapted from: https://bioinformatics-core-shared-training.github.io/linear-models-r/ANOVA.html#
par(mfrow=c(1,1))

# start by reading in data:
diet = read.csv("/Users/jillnaiman/Downloads/diet.csv",row.names=1)
# define a weight loss parameter
diet$weight.loss = diet$initial.weight - diet$final.weight 

# Q1: plot a boxplot and give first impressions
# Q2: is there a difference in the means between groups?
#     Note: you might get an "unbalanced error" - we'll ignore this for now
# Q3: which diet is better at weight change?

# ANS1:
boxplot(weight.loss~diet.type,data=diet,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="blue") # weight loss = 0 line
# ANS2:
aov_results = aov(weight.loss ~ diet.type, data=diet)
summary(aov_results) # From our F & p-value looks like there is a difference!
# ANS3:
print(pairwise.t.test(diet$weight.loss, diet$diet.type, p.adjust="bonferroni"))
# Here B & A are the same, but C is different then both with pvalue < 0.05 level of significance



