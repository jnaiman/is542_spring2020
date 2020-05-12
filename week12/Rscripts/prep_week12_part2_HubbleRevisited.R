# This is revisiting the Hubble dataset to check things out
# following here: http://adamdempsey90.github.io/python/dark_energy/dark_energy.html
# error bars from
# building from: http://www.phys.boun.edu.tr/~ozcan/lectures/phys443autumn13/classwork/fitting/fittingData_part2.html

hubble = read.csv('~/Downloads/sn_data_out_lg.csv',stringsAsFactors=TRUE)
# then do sn_data_out_lg.csv

distance = hubble[,2] # Mpc
vel = hubble[,1] # km/s
# errors
distErr = hubble[,3] # Mpc

# first things first, lets plot!
par(mfrow=c(1,1))
plot(distance, vel, pch=16, 
     col=30, xlab='Distance in Mpc', 
     ylab = 'Recessional Velocity in km/s')
# plot errors, for fun
arrows(distance-distErr*0.5,vel, 
       distance+distErr*0.5,vel, length=0.05, angle=90, code=3)


# Q1: If we assume we are fitting ressessional velocity as a function of distance, 
#    what is b1 (Hubble's constant)?  What about if we add another function of distance (some extra power of distance...)
# Bonus: What about if we instead fit distance as a function of velocity 
#    Aside: now formula for hubble's is distance = velocity/Hubble's constant (so Hubble's constant = 1/b1)
#           What new power of *velocity* should we look at?
# Bonus bonus: assuming you're working with the data in the "Bonus", 
#      if you bin in velocity space, what is the chi^2 of your fit?


# Give them up to here
#######################


# how linear is this thing?
R = cor(distance,vel)
#print(R)
# so, pretty linear
d2 = distance^2 # for lg
myLine = lm(formula = vel ~ distance+d2, 
            data = data.frame(d2,distance,vel),weights=1./distErr^2) #lg
#print(summary(myLine))
# so, we can see that we have a very small p-value for our slope - so probably a line is a good fit!

# lets analize our residuals
par(mfrow=c(2,1))
myResiduals = resid(myLine)
plot(myResiduals)

# we can also check if our residuals are normal (one of our fit conditions)
qqnorm(myResiduals)
qqline(myResiduals)

# there is definitely some non-QQPlot stuff going on

# ok, lets plot our data & fit
par(mfrow=c(2,2))

b0 = myLine$coefficients[1] # intercept
b1 = myLine$coefficients[2] # slope
b2 = myLine$coefficients[3] # slope for x2, lg

x = seq(-0.25, 14000) # little negative just so we can see the points
myNewLine = b0 + b1*x + b2*x**2 # lg
plot(x, myNewLine, type='l', col="green", xlab="Distance in Mpc", ylab="Resessional Velocity in km/s")
points(distance, vel, pch=16, col=30) # over plot observation points
# plot errors
arrows(distance-distErr*0.5, vel,
       distance+distErr*0.5,vel,length=0.05, angle=90, code=3)
plot(myLine, which=1) # Residuals plot
plot(myLine, which=2) # qq-norm plot

# now, lets add another plot to our meagurie
plot(myLine, which=5) # residuals vs. leverage

##################################
# Bonus
#################################

distance = hubble[,2] # Mpc
vel = hubble[,1] # km/s

# first things first, lets plot!
par(mfrow=c(1,1))
plot(vel, distance, pch=16, 
     col=30, ylab='Distance in Mpc', 
     xlab = 'Recessional Velocity in km/s')

myLineFit = lm(formula=distance~vel, data=data.frame(vel,distance))

v2 = vel*vel
myFit = lm(formula = distance ~ vel+v2, 
            data = data.frame(vel, v2,distance))

x = seq(0,5e5, length=200)
newLine = myFit$coefficients[1] + myFit$coefficients[2]*x +myFit$coefficients[3]*x*x
plot(x, newLine, type='l',ylab='Distance in Mpc', 
     xlab = 'Recessional Velocity in km/s')
points(vel, distance, pch=16, col=30) # over plot observation points

lines(x, myLineFit$coefficients[1]+myLineFit$coefficients[2]*x, col='red')
