# so read in original Hubble dataset
hubble = read.csv('/Users/jillnaiman1/Downloads/hubble.csv', 
                  stringsAsFactors = TRUE)

# explanatory variable is distance (x)
distance = hubble[,1]/1e6 # Mpc, 10^6 pc, ~30 trillion km, 3.26 LY
# pc != a unit of time!!!!!
vel = hubble[,2] # km/s

# for new datasets: vel = hubble[,1] & distance = hubble[,2]

# (0) Plot the dataset
# (1) How linear is the relationship?
# (2) Fit a line & plot with data
# lm(formula = y~x, data=data.frame(x,y))
myLine = lm(formula=vel~distance, data=data.frame(distance,vel))
par(mfrow=c(1,1))
x = seq(0,2, length=50)
plot(x,myLine$coefficients[1] + myLine$coefficients[2]*x, 
     type='l', col="green", xlab='Distance away in Mpc', 
     ylab='Recessional Velocity')
points(distance,vel)
# (3) Is our fit justified (how linear is the dataset, residuals)
#plot(myLine, which=1)
# (4) Are there any outliers we should worry about?
# (5) Does the intercept make sense?
print(myLine$coefficients[1]) # intercept
# (6) What is the slope of our fit?
print('slope in units of km/s/Mpc')
print(myLine$coefficients[2])

# lets calculate the beginning of time!
hubbles_const = myLine$coefficients[2]
beginning_of_time = 1.0/(hubbles_const *(1.0/3.08e19)) # Mpc -> km
beginning_of_time_in_Gyrs = beginning_of_time/3.15e7/1e9 # in Gyrs
print(beginning_of_time_in_Gyrs) # a little different than 13.7, but OM!

# hubble = read.csv('/Users/jillnaiman1/Downloads/sn_data_out_lg.csv', 
#                   stringsAsFactors=TRUE)
# distance = hubble[,2] # Mpc
# vel = hubble[,1] # km/s
# distErr = hubble[,3] # Mpc
# 
# plot(distance, vel, pch=16, col=30, xlab='Distance in Mpc',
#      ylab='Recessional Velocity in km/s')
# arrows(distance-distErr*0.5,vel,
#        distance+distErr*0.5,vel, length=0.05, angle=90, code=3)
# 
# myFit = lm(formula=vel~distance+d2, data= ..., weights=...) # what might d2 be?
