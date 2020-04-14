# DO NOT UPLOAD TO MOODLE
#  This is a utility file to be run on the downloaded class data after they take the survey.
#  This file will format the class's data

# read in file with names
classData = read.csv("/Users/jillnaiman/Dropbox/teaching/stats_spring_2020/week02/stats_spring_2020.csv")

# take out columns
newData = classData[,-c(1,2,9,10,11)]

# write out file
write.csv(newData, file = "/Users/jillnaiman/Dropbox/teaching/stats_spring_2020/week02/formatted_class_answers_orig_orig.csv")

time_in = newData[,5]
# lets reformat this data a bit!
time_min = c() # storage
time_max = c() # storage
# lets look at the options again
myLevels = levels(time_in)
print(myLevels)
# so ,we can see our options
# lets do a for loop!
for (i in 1:length(time_in)){
  # note we can also do: time_in[i] == '< 6 months'
  if (time_in[i] == myLevels[1]){ # < 6 months
    time_min = c(time_min,0)
    time_max = c(time_max, 0.5) # in years
  } else if (time_in[i] == myLevels[2]) { # 1-2 years
    time_min = c(time_min, 1.0)
    time_max = c(time_max, 2.0)
  } else if (time_in[i] == myLevels[3]) {
    time_min = c(time_min, 2.0)
    time_max = c(time_max, 4.0)
  } else {
    time_min = c(time_min,0.5)
    time_max = c(time_max,1.0)
  }
}


# to print like fall 2018 for examples of datatypes
dataTypes = newData[c(1,4)]
# rename
names(dataTypes) = c("Fam.Stats","language")

# add time min & max
dataTypes$time_yr.min = time_min
dataTypes$time_yr.max = time_max

# for viewing
View(dataTypes)

