# ANSWER for DNA test/Stock doubling question:

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
