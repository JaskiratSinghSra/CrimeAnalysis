dfs = read.csv("D:/Downloads/crimes-in-chicago/Chicago_Crimes_2012_to_2017.csv")

df = dfs[,c(7,10,11,13,14,19,21,22)]

crime0 <- na.omit(df)
library(dummies)

new_my_data <- dummy.data.frame(crime0, names = c("Primary.Type","Arrest","Domestic"))

prin_comp <- prcomp(new_my_data, scale. = T)
comp <- data.frame(prin_comp$x[,1:3])

kmeans.wss.k <- function(crime, k){
 km = kmeans(crime, k)
 return (km$tot.withinss)
 }

kmeans.dis <- function(crime, maxk){
 dis=(nrow(crime)-1)*sum(apply(crime,2,var))
 dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
 return(dis)}

maxk = 10
 dis = kmeans.dis(comp, maxk);
 plot(1:maxk, dis, type='b', xlab="Number of Clusters",
  ylab="Within groups sum of squares",
  col="blue")

final = kmeans(comp, 4)

library(rgl)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=final$clust)

sort(table(final$clust))
clust <- names(sort(table(final$clust)))

clust1 = new_my_data[final$cluster==1,];
reqcols = clust1[,c(1:33)];
sum = sort(colSums(reqcols));

par(mfrow=c(2,2))
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 7, 0, 0)) 
barplot(sum[20:33], main="Cluster 1",adj=0, horiz=T, las=2,
   xlab="No.of Crimes",cex.names=0.5,cex.axis=0.8) 

clust1 = new_my_data[final$cluster==2,];
reqcols = clust1[,c(1:33)];
sum = sort(colSums(reqcols));

mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 7, 0, 0)) 
barplot(sum[20:33], main="Cluster 2",adj=0, horiz=T, las=2,
   xlab="No.of Crimes",cex.names=0.5,cex.axis=0.8)

clust1 = new_my_data[final$cluster==3,];
reqcols = clust1[,c(1:33)];
sum = sort(colSums(reqcols));

mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 7, 0, 0)) 
barplot(sum[20:33], main="Cluster 3", horiz=T, las=2,
   xlab="No.of Crimes",cex.names=0.5,cex.axis=0.8)

clust1 = new_my_data[final$cluster==4,];
reqcols = clust1[,c(1:33)];
sum = sort(colSums(reqcols));

mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 7, 0, 0)) 
barplot(sum[20:33], main="Cluster 4", horiz=T, las=2,
   xlab="No.of Crimes",cex.names=0.5,cex.axis=0.8)