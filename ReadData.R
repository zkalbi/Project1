library(foreign)

test <- read.arff(unz(description="Data.zip",filename="AstronomyTestData.txt"))
train <- read.arff(unz(description="Data.zip",filename="AstronomyTrainingData.txt"))
lownoise <- read.csv(unz(description="Data.zip",filename="LowNoiseData.txt"),header=F)
poslab <- read.csv(unz(description="Data.zip",filename="PossibleLabels.txt"),header=F)

names(lownoise)
summary(lownoise$V1) # oh, these are ID numbers

names(train)
table(poslab)
class(poslab)

names(test)
names(train)

table(test$class)
table(train$class)
table(subset(test,test))