library(foreign)
library(cluster)
library(ggplot2)
library(MASS)
library(fastcluster)
library(protoclust)
library(ggdendro)
library(RSKC) # for CER : classification error rate

#test <- read.arff(unz(description="Data.zip",filename="AstronomyTestData.txt"))
#train <- read.arff(unz(description="Data.zip",filename="AstronomyTrainingData.txt"))
#lownoise <- read.csv(unz(description="Data.zip",filename="LowNoiseData.txt"),header=F)
#poslab <- read.csv(unz(description="Data.zip",filename="PossibleLabels.txt"),header=F)

#names(lownoise)
#summary(lownoise$V1) # oh, these are ID numbers

#names(train)
#table(poslab)
#class(poslab)

# gap
clusGap(test1, kmeans, K.max = 20, B = 100)


# remove ID and class information
# select variables which are not constant
 
test1<-test[lownoise[,1],]
vars <- apply(na.omit(test1[,c(-1,-87)]),2,var)
which(vars==0)
testL <- na.omit(test1[,c(-1,-87)])[,-which(vars==0)] #6631 removed



##hierachical on lonoise data only
#d1<-dist(scale(testL))
d<-dist(scale(testL))
d<-as.matrix(d)
#####
func<-ifelse(n>=65535,65535,n)
funcf<-function(n) {
  ifelse(n>=65535,65535,n)
}
d1<-apply(d,2,funcf)
#####
fit1 <- hclust(d, method = "centroid")
fit2<- hclust(d, method = "single")
fit3<- hclust(d, method = "complete")
fit4 <- hclust(d, method = "average")


# plot hclust result centroid

hccentroid<-as.dendrogram(fit1)
a<-ggdendrogram(cut(hccentroid,h=80)$upper,labels=FALSE,leaf_labels=FALSE)
plot(cut(hccentroid, h=.25)$lower[[2]], 
     main="Second branch of lower tree with cut at h=75")
ggsave('a.png', height = 10, width = 10)

# plot hclust result average
hcavg<-as.dendrogram(fit4)
b=ggdendrogram(cut(hcavg,h=50)$upper,labels=FALSE,leaf_labels=FALSE,main="Upper tree of cut at h=75")
plot(cut(hcavg, h=.03)$lower[[2]], 
     main="Second branch of lower tree with cut at h=75")

ggsave('b.png', height = 5, width = 5)


