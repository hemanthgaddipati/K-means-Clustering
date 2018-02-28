library(plyr)
library(xlsx)
library(animation)
EastWestAirlines <- read.xlsx("C:/Users/SRIRAMA/Desktop/Data Science/Assignments and stuff/Clustering/EastWestAirlines.xlsx", 
                    sheet = "data")
normalized.data <- scale(EastWestAirlines[,2:12])

#elbow curve & k ~ sqrt(n/2) to decide the k value
##within sum of squares formula

kvalue = (nrow(normalized.data)-1)*sum(apply(normalized.data, 2, var))		 # Determine number of clusters by scree-plot 

for (i in 2:12) kvalue[i] = sum(kmeans(normalized.data, centers=i)$withinss)

plot(1:12, kvalue, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# selecting K for kmeans clustering using kselection

library(kselection)
k <- kselection(normalized.data[,-12], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k

library(doParallel)
registerDoParallel(cores=4)
k <- kselection(normalized.data[,-12], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k

##11 cluster solution

fit <- kmeans(normalized.data,11)
str(fit)
fit$cluster #gives the cluster number of each row
fit$centers
ani <- kmeans.ani(normalized.data,6)
final1<- data.frame(EastWestAirlines, fit$cluster)
final1
final.model <- final1[,c(ncol(final1),1:(ncol(final1)-1))]
final.model
aggregate(EastWestAirlines[,2:12], by=list(fit$cluster), FUN=mean)


# k clustering alternative for large dataset - Clustering Large Applications (Clara)

library(cluster)
data <- clara(normalized.data, 11)
clusplot(data)

