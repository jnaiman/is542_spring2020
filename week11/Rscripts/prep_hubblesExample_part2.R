# got through this quickly - so do this as a class example activity


# looking at the *redshift* of the lights of galaxies => redder means receding further
hubble = read.csv('/Users/jillnaiman/Downloads/hubble.csv',stringsAsFactors=TRUE)

distance = hubble[,1]/1e6 # pc -> Mpc, PS: 1 pc = 30,856,776,000,000.00 km 
# (~30 trillion km), or 3.26 LY 
# and IS NOT A UNIT OF TIME (sorry star wars)
vel = hubble[,2] # km/s

### give above and ask:
# (1) How linear is the relationship? (R)
# (2) Fit a line
# (3) Is our fit justified? (residuals, qqnorm)
# (4) Are there any outliers we should worry about?


# first things first, lets plot!
par(mfrow=c(1,1))
plot(distance, vel, pch=16, col=30, xlab='Distance in Mpc', ylab = 'Recessional Velocity in km/s')
# point out things < 0 are moving towards us

# how linear is this thing?
R = cor(distance,vel)
print(R)
# so, pretty linear
myLine = lm(formula = vel ~ distance, data = data.frame(distance,vel))
print(summary(myLine))
# so, we can see that we have a very small p-value for our slope - so probably a line is a good fit!

# lets analize our residuals
par(mfrow=c(2,1))
myResiduals = resid(myLine)
plot(myResiduals)

# we can also check if our residuals are normal (one of our fit conditions)
qqnorm(myResiduals)
qqline(myResiduals)

# While our residuals look pretty normal, we see that there are some variations
#  these variations are actually "cosmic scatter" and its becuase there
#  are groups of galaxies that are gravitationally interacting and changing the 
#  relation from a straight line.

# ok, lets plot our data & fit
par(mfrow=c(2,2))

b0 = myLine$coefficients[1] # intercept
b1 = myLine$coefficients[2] # slope

x = seq(-0.25, 2.5) # little negative just so we can see the points
myNewLine = b0 + b1*x
plot(x, myNewLine, type='l', col="green", xlab="Distance in Mpc", ylab="Recessional Velocity in km/s")
points(distance, vel, pch=16, col=30) # over plot observation points
plot(myLine, which=1) # Residuals plot
plot(myLine, which=2) # qq-norm plot

# now, lets add another plot to our meagurie
plot(myLine, which=5) # residuals vs. leverage


##### GIVE UP TO HERE #######

## Then go through this

# This brings us to hubbles law: V = H0 X D where H0 is a constant in units of velocity/Mpc = km/s/Mpc (weird)
#  where H0 is hubble's constant = b1 
hubbles_const = b1
print(hubbles_const) # in km/s/Mpc
# note: actual value is like ~70 km/s/Mpc
# But even though it's pretty off, it was a huge discovery!!

# If we assume we are not in a special place in the universe, this means that
# the Universe is expanding away from itself
#  you can think of for, example 2D universe represnted as a balloon and 
# blowing up the balloon as the expanding universe
#  you can draw some dots on the balloon and see that they expand away from 
#  eachother and the ones furthest from your mouth expand away from you
# fastest

# So, lets real quick, calculate the begining of time ok?
# we recall that velocity = distance/time, so then time = distance/velocity
# we have a realationshp for velocity and distance in the hubble constant which is the inverse of this!
beginning_of_time = 1.0/( hubbles_const * (1.0/3.08e19) ) # the last is just a unit conversion from Mpc to km
# that is in seconds, so lets make it in years
beginning_of_time_in_yrs = beginning_of_time/3.15e7/1e9 # into Gyrs aka billions of years
print(beginning_of_time_in_yrs)

# so, this says the Universe is ~2Gyrs old
#  in realaity, here we have over estimated the hubble's constant so we are 
# underestimating the age of the universe, which is actually about 13.5 Gyrs (or 13.5 billion years) old

# so good job everybody!  With linear regression you just figured out (approximately) when the
#  beginning of time was!  Hurray!!




