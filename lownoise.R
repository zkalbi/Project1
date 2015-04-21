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
lneigval <- (lnpcs$sdev)^2
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
  ggtitle("Low Noise Data Set Heat Plot")

# =========================
# Scree Plot
# =========================
qplot(y = lneigval/sum(lneigval), x = seq(1,length(lneigval), by = 1), geom = "line") +
  geom_point() + xlab("Number of Eigenvalues") +
  ylab("Proportion of Variance Explained") +
  ggtitle("Low Noise Data Set Scree Plot")


# =========================
# Cumulative Explained Plot
# =========================
qplot(y = cumsum(lneigval)/sum(lneigval), x = seq(1,length(lneigval), by = 1), geom = "line") +
  geom_point() + xlab("Number of Eigenvalues") +
  ylab("Proportion of Variance Explained") +
  ggtitle("Low Noise Data Set Cumulative Variance Explained")


# ============================
# Clustering
# ============================

# Selecting all variables from "freq3_harmonic_rel_phase_1" to the end
# and variables "freq(1,2)_harmonic_rel_phase_(1,2,3)"

# reorder variable names into same variable
lnnewdat <- lndat[,sort(colnames(lndat))]
lnnewdat <- lnnewdat[,c(grepl(pattern = "harmonics_rel_phase",x = colnames(lnnewdat)),
                    c(64:82))]

# randomly select a subset of observations to cluster on... for lack of computing power
set.seed(623)
lnnewdatsub <- sample_n(tbl = lnnewdat, size = 6000) #in dplyr package

# ============================
# kmeans
# ============================
library(fastICA)
library(kernlab)
library(fpc)
library(protoclust)
library(cluster)
library(kknn)
library(mclust)
library(EMCluster)

clusts <- kmeansruns(lnnewdatsub, krange = 1:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)

clusts2 <- kmeansruns(lnnewdat, krange = 2:25, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)

# silhouette plot
qplot(y = clusts2$crit[-1], x = seq(2, length(clusts2$crit), by = 1), geom = "line") +
  geom_point() + xlab("Number of Clusters") + ylab("Average Silhouette Value") +
  ggtitle("Average Silhouette by Number of Clusters on Random Subset of Low Noise Data")

# clustering is dumb - similar results were obtained using 4000 observations


# ============================
# hierarchical clustering
# ============================
library(protoclust)

# randomly select a subset of observations to cluster on... for lack of computing power

lnhclust = hclust(dist(lnnewdat),method="centroid") # need to reduce dimension

# Heirarchical Clustering with Centroid Linkage
lnhclust25=cutree(lnhclust, k = 25)

plot(lnhclust25)

# MINIMAX Linkage
plnhclust <- protoclust(dist(lnnewdat))
plnhclust25 <- protocut(plnhclust, k = 25)




############################################
hc=as.dendrogram(lnhclust)
plot(cut(hc,h=40.5)$upper)

library(ggdendro)
ggdendrogram(cut(hc,h=40)$upper,labels=FALSE,leaf_labels=FALSE)


library(cluster)
# silhouette values for centroid link
for(k in 2:25)
  plot(silhouette(x=cutree(lnhclust, k = 25),dist=dist(lnnewdat)), main = paste("k = ",k), do.n.k=FALSE)
mtext("PAM(Ruspini) as in Kaufman & Rousseeuw, p.101",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()




######Visualize Low Noise Clusters with PCA#########

qplot(Comp.1,Comp.2,data=as.data.frame(lnscores),color=lnhclust25) +
  ggtitle("Heirarchical Clustering with Centroid Linkage")

library(car)



clusts3 <- kmeansruns(lnnewdat, krange = 4, criterion = 'asw', iter.max = 100, runs = 5, critout = TRUE)


qplot(Comp.1,Comp.2,data=as.data.frame(lnscores),color=as.factor(clusts3$cluster))
