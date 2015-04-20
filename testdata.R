# ============================
# CLEANING DATA
# ============================

# remove ID and class information
# select variables which are not constant
vars <- apply(na.omit(test[,c(-1,-87)]),2,var)
which(vars==0)

dat <- na.omit(test[,c(-1,-87)])[,-which(vars==0)] #6631 removed

# ============================
# PCA
# ============================
pcs <- princomp(x=dat,scores=TRUE,cor=TRUE)

eigvec <- pcs$loading
eigval <- (pcs$sdev)^2
scores <- pcs$scores

dim(eigvec)
colnames(eigvec) <- names(dat)
eigvec[1:10,1:5]

# for some reason, cannot coerse "loadings" type object to a data.frame
write.csv(eigvec, "PCAloadings.csv", row.names=TRUE)
eigvec <- read.csv("PCAloadings.csv")
names(eigvec)[1] <- "Variable"

library(dplyr)
library(reshape)

# Using Variable as id variables
eigvec.m <- melt(eigvec, id.vars = "Variable", variable_name = "PC")
names(eigvec.m)[3] <- "Loading"
eigvec.m$Eigenvalue <- eigval[eigvec.m$PC]

# =========================
# Eigenvector Heat Plot - Width of tiles "proportional" to Eigenvalue
# =========================
library(ggplot2)
ggplot(eigvec.m, aes(PC, Variable)) +
  geom_tile(aes(fill = Loading, width = Eigenvalue)) +
  scale_fill_gradient2(low="darkblue", high="red", guide="colorbar") +
  xlab("Principal Component Eigenvector (Width Proportional to Eigenvalue)") +
  theme(axis.text.x = element_blank(), panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank())+
  ggtitle("Test Data Set")

# =========================
# Scree Plot
# =========================
qplot(y = eigval/sum(eigval), x = seq(1,length(eigval), by = 1), geom = "line") +
  geom_point() + xlab("Number of Eigenvalues") +
  ylab("Proportion of Variance Explained")

# ============================
# Clustering
# ============================

# ========================================================================
# Decisions to make: Use principal components as variables to cluster on?
# If so, how many? Or, should we select variables based on their loadings?
# ========================================================================

#########Partitioning dataset into five subsets###########
### Creating Five Sets#####
N=1:dim(dat)[1]
n1=sample(N,8699)
n2=sample(N[-n1],8699)
n3=sample(N[-c(n1,n2)],8699)
n4=sample(N[-c(n1,n2,n3)],8698)
n5=N[-c(n1,n2,n3,n4)]

# Selecting all variables from "freq3_harmonic_rel_phase_1" to the end
# and variables "freq(1,2)_harmonic_rel_phase_(1,2,3)"

# reorder variable names into same variable
newdat <- dat[,sort(colnames(dat))]
newdat <- newdat[,c(grepl(pattern = "harmonics_rel_phase",x = colnames(newdat)),
                    c(64:82))]

# creating data chunks
new1=newdat[n1,]
new2=newdat[n2,]
new3=newdat[n3,]
new4=newdat[n4,]
new5=newdat[n5,]

####Running K Means on first data set  chunk
library(mclust)
library(fpc)

kc1 <- kmeansruns(new1, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)
kc2 <- kmeansruns(new2, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)
kc3 <- kmeansruns(new3, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)
kc4 <- kmeansruns(new4, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)
kc5 <- kmeansruns(new5, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)

qplot(y = kc1$crit[-1], x = seq(2, length(kc1$crit), by = 1), geom = "line") +
  geom_point() + xlab("Number of Clusters") + ylab("Average Silhouette Value") +
  ggtitle("Average Silhouette by Number of Clusters on First Subset of Test Data")


