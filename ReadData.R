library(foreign)

test <- read.arff(unz(description="Data.zip",filename="AstronomyTestData.txt"))
train <- read.arff(unz(description="Data.zip",filename="AstronomyTrainingData.txt"))
lownoise <- read.arff(unz(description="Data.zip",filename="LowNoiseData.txt"))
poslab <- read.arff(unz(description="Data.zip",filename="PossibleLabels.txt"))