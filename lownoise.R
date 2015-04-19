#### PCA on low noise data ####

####Getting low noise data set
lntest=test[lownoise[,1],]


# ============================
# CLEANING DATA
# ============================

# remove ID and class information
# select variables which are not constant
vars <- apply(na.omit(lntest[,c(-1,-87)]),2,var)
which(vars==0)

lndat <- na.omit(lntest[,c(-1,-87)])[,-which(vars==0)]


# ============================
# PCA
# ============================
lnpcs <- princomp(x=lndat,scores=TRUE,cor=TRUE)

lneigvec <- lnpcs$loading
lneigval <- lnpcs$sdev
lnscores <- lnpcs$scores

dim(lneigvec)
colnames(lneigvec) <- names(lndat)
lneigvec[1:10,1:5]

# for some reason, cannot coerse "loadings" type object to a data.frame
write.csv(lneigvec, "lnPCAloadings.csv", row.names=TRUE)
lneigvec <- read.csv("lnPCAloadings.csv")
names(lneigvec)[1] <- "Variable"

library(dplyr)
library(reshape)

# Using Variable as id variables
lneigvec.m <- melt(lneigvec, id.vars = "Variable", variable_name = "PC")
names(lneigvec.m)[3] <- "Loading"
lneigvec.m$Eigenvalue <- lneigval[lneigvec.m$PC]


# =========================
# Eigenvector Heat Plot - Width of tiles "proportional" to Eigenvalue
# =========================
library(ggplot2)
ggplot(lneigvec.m, aes(PC, Variable)) +
  geom_tile(aes(fill = Loading, width = Eigenvalue)) +
  scale_fill_gradient2(low="darkblue", high="red", guide="colorbar") +
  xlab("Principal Component Eigenvector (Width Proportional to Eigenvalue)") +
  theme(axis.text.x = element_blank(), panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("Low Noise Data Set")

# =========================
# Scree Plot
# =========================
qplot(y = lneigval/sum(lneigval), x = seq(1,length(lneigval), by = 1), geom = "line") +
  geom_point() + xlab("Number of Eigenvalues") +
  ylab("Proportion of Variance Explained")


# ============================
# Clustering
# ============================

# ========================================================================
# Decisions to make: Use principal components as variables to cluster on?
# If so, how many? Or, should we select variables based on their loadings?
# ========================================================================

# kmeans
clusts <- kmeans(na.omit(lntest[,c(-1,-87)]),centers=18)

plot(scores[,1] ~ scores[,2],col=clusts$cluster)

persp()

# heirarchical clustering
hclust(dist(na.omit(lntest[,c(-1,-87)]))) # need to reduce dimension
