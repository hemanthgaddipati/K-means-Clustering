crime_data <- read.csv("C:/Users/SRIRAMA/Desktop/Data Science/Assignments and stuff/Clustering/crime_data.csv")
mydata <- crime_data
View(mydata)

#normalize the data 
normal.data <- scale(mydata[,2:5])

#elbow curve for finding k value for k-means clustering
  ##wss formula and scree plot
    kvalue = (nrow(normal.data)-1)*sum(apply(normal.data,2,var))
    for (i in 2:5) 
       {kvalue[i]=sum(kmeans(normal.data,centers = i)$withinss)}
    plot(1:5,kvalue,type = "b",xlab = "number of clusters",ylab = "within group sum of squares")
    title(sub = "scree-plot for k-means")
  
  ##using k-selection
    library(kselection)
    k <- kselection(normal.data)
    k
#using both wss and k-selection we got k as 2 clusters so lets move forward with 3 clusters
  
  ## 3 cluster solution
    fitdata <- kmeans(normal.data, 3) 
    str(fitdata)
    fitdata$cluster
    fitdata$centers
    library(animation)
    animation <- kmeans.ani(normal.data,3)
    endmodel <- data.frame(crime_data,fitdata$cluster)
    endmodel
    final1<- data.frame(EastWestAirlines, fit$cluster)
    final1
    final.model <- endmodel[,c(ncol(endmodel),1:(ncol(endmodel)-1))]
    final.model
    
    write.xlsx(final.model,file = "crimedata kmeans.xlsx")
    getwd()    
    
    ##Using clara
    library(cluster)
    data <- clara(normal.data, 3)
    clusplot(data)
    
    
    
