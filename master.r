burial.KWL= read.delim("burials_Kiwulan.csv", header=T, sep=",")
u.burial.KWL= read.delim("upperburials_Kiwulan.csv", header=T, sep=",")
l.burial.KWL= read.delim("Lowerburials_Kiwulan.csv", header=T, sep=",")
u.burial.KWL= read.delim("upperburials_Kiwulan.csv", header=T, sep=",")
attach(u.burial.KWL)
attach(burial.KWL)
u.burials= na.omit(u.burial.KWL[13:29])
par(las=2,mar=c(6,4,4,2))
boxplot(u.burials, main= " burial goods", xlab=" Elements")
# log the original data
burials.log= sapply(u.burials, log)
par(las=2,mar=c(6,4,4,2))
boxplot(burials.log, main="burial goods", xlab="Logged Elements")
# standarize the original data
mean.elements= sapply(u.burials, mean)
sd.elements= sapply(u.burials, sd)
burials.z = t( (t(u.burials)- mean.elements)/sd.elements)
par(las=2,mar=c(6,4,4,2))
boxplot(burials.z, main="burial goods", xlab="Standarized Elements")

# correlation matrix
options('digits')
options(digits= 3)
R= cor(burials.z, use="complete.obs")
R
library("psych")
cortest.bartlett(R)
PCA= prcomp(burials.z)
summary(PCA)
eigenvalues= PCA$sdev^2
plot(1:length(eigenvalues), eigenvalues, main="Scree plot",
     xlab="Components", ylab=" Eigenvalues",type="l", col="red")
axis(1, at= 1:length(eigenvalues))
loadings= PCA$rotation^2
loadings
#byplot of pc1 and pc2(gender)
plot(PCA$x[,1], PCA$x[,2], pch=".",xlab="PC1", ylab="PC2", main="Pottery - PC1 .vs. PC2")
labels= factor(gender)
text(PCA$x[,1], PCA$x[,2], labels, cex=0.7)
abline(h=0, v=0, lty=2, col="red")

#bylot of pc1 and pc2(age)
plot(PCA$x[,1], PCA$x[,2], pch=".",xlab="PC1", ylab="PC2", main="Pottery - PC1 .vs. PC2")
labels= factor(age)
text(PCA$x[,1], PCA$x[,2], labels, cex=0.7)
abline(h=0, v=0, lty=2, col="red")

#byplot of pc1 and pc3(gender)
plot(PCA$x[,1], PCA$x[,3], pch=".",xlab="PC1", ylab="PC3", main="Pottery - PC1 .vs. PC3")
labels= factor(gender)
text(PCA$x[,1], PCA$x[,3], labels, cex=0.7)
abline(h=0, v=0, lty=2, col="red")

#bylot of pc1 and pc3(age)
plot(PCA$x[,1], PCA$x[,3], pch=".",xlab="PC1", ylab="PC3", main="Pottery - PC1 .vs. PC3")
labels= factor(age)
text(PCA$x[,1], PCA$x[,3], labels, cex=0.7)
abline(h=0, v=0, lty=3, col="red")

#byplot of pc1 and pc4(gender)
plot(PCA$x[,1], PCA$x[,4], pch=".",xlab="PC1", ylab="PC4", main="Pottery - PC1 .vs. PC4")
labels= factor(gender)
text(PCA$x[,1], PCA$x[,4], labels, cex=0.7)
abline(h=0, v=0, lty=2, col="red")

#bylot of pc1 and pc4(age)
plot(PCA$x[,1], PCA$x[,4], pch=".",xlab="PC1", ylab="PC4", main="Pottery - PC1 .vs. PC4")
labels= factor(age)
text(PCA$x[,1], PCA$x[,4], labels, cex=0.7)
abline(h=0, v=0, lty=3, col="red")

#byplot of pc1 and pc5(gender)
plot(PCA$x[,1], PCA$x[,5], pch=".",xlab="PC1", ylab="PC5", main="Pottery - PC1 .vs. PC5")
labels= factor(gender)
text(PCA$x[,1], PCA$x[,5], labels, cex=0.7)
abline(h=0, v=0, lty=2, col="red")

#bylot of pc1 and pc5(age)
plot(PCA$x[,1], PCA$x[,5], pch=".",xlab="PC1", ylab="PC5", main="Pottery - PC1 .vs. PC5")
labels= factor(age)
text(PCA$x[,1], PCA$x[,5], labels, cex=0.7)
abline(h=0, v=0, lty=3, col="red")

# cluster analysis
mydata = na.omit(u.burials)
mydata = scale(u.burials)
wss <- (nrow(u.burials)-1)*sum(apply(u.burials,2,var))
for (i in 1:17)
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:17, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean")
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")