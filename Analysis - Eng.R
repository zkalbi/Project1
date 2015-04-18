# ============================
# CLEANING DATA
# ============================

# remove ID and class information
# select variables which are not constant
vars <- apply(na.omit(test[,c(-1,-87)]),2,var)
which(vars==0)

dat <- na.omit(test[,c(-1,-87)])[,-which(vars==0)]

# ============================
# PCA
# ============================
pcs <- princomp(x=dat,scores=TRUE,cor=TRUE)

eigvec <- pcs$loading
eigval <- pcs$sdev
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
        axis.ticks.x = element_blank())

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

# kmeans
clusts <- kmeans(na.omit(test[,c(-1,-87)]),centers=18)

plot(scores[,1] ~ scores[,2],col=clusts$cluster)

persp()

# heirarchical clustering
hclust(dist(na.omit(test[,c(-1,-87)]))) # need to reduce dimension
