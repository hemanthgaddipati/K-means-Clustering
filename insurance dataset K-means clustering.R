### Hierarchical clustering
  Insurance_Dataset <- read.csv("C:/Users/SRIRAMA/Desktop/Data Science/Assignments and stuff/K means clustering/Insurance Dataset.csv")
  #normalize the data
  n <- scale(Insurance_Dataset[1:5])
  d <- dist(n,method = "euclidean")
  a <- hclust(d,method = "complete")
  plot(a)
  plot(a,hang=-1)
  groups <- cutree(a,k=5)
  rect.hclust(a,k=5,border = "red")
  membership <- as.matrix(groups)
  end <- data.frame(Insurance_Dataset,membership)
  FINAL <- end[,c(ncol(end),1:(ncol(end)-1))]
  View(FINAL)

### K-Means clustering

  Insurance_Dataset <- read.csv("C:/Users/SRIRAMA/Desktop/Data Science/Assignments and stuff/K means clustering/Insurance Dataset.csv")
  #data has many units so lets normalize the data
  normaldata <- scale(Insurance_Dataset[,1:5])
  View(normaldata)
  #now lets find the number of clusters or k-value
  ##within sum of squares formula method
  a <- (nrow(normaldata)-1)*sum(apply(normaldata,2,var))
  for (i in 1:5) {a[i]=sum(kmeans(normaldata,centers=i)$withinss)}
  plot(1:5,a,type = "b")
  ##kselection 
  library(kselection)
  k <- kselection(normaldata)
  k
  ##doparallel
  library(doParallel)
  registerDoParallel(cores = 4)
  ks <- kselection(normaldata)
  ks

  #now with k value lets build our cluster
  c <- kmeans(normaldata,3)  ##we went with 3 clusters because it has high BSS and less WSS
  str(c)
  c$cluster
  c$centers
  library(animation)
  ani <- kmeans.ani(normaldata,3)
  endcluster <- data.frame(Insurance_Dataset,c$cluster)
  finalmodel <- endcluster[,c(ncol(endcluster),1:(ncol(endcluster)-1))]
  write.csv(finalmodel,file = "Insurence.dataset.csv")
  getwd()
  ##cusplot
  library(cluster)
  data <- clara(normaldata,3)
  clusplot(data)
  