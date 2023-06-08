#Install necessary packages.
install.packages("readxl")
install.packages("NbClust")
install.packages("cluster")
install.packages("factoextra")
install.packages("fpc")
install.packages("clValid")



# The program begins by loading the necessary packages.
library(readxl)
library(NbClust) 
library(cluster)
library(factoextra)
library(fpc)
library(clValid)




#___________________________SUBTASK_1____________
#Load the dataset
#The "vehicals.xlsx" file is read by this line, which stores the information in a data frame titled "VehicalDetails."
VehicalDetails <- read_excel("D:/MLCourseWork/vehicles.xlsx")

#The console's cars dataframe should have 846 observations and 19 variables (18 input features and 1 output variable), therefore double-check its structure.
str(VehicalDetails)

#extract only the first 18 attributes.
#This line retrieves the first 18 properties from the'vehicles' data frame and stores them.
#Them in a new data frame called "VehicalDetails"
vehicales_data <- VehicalDetails[,2:19]
#This line removes rows containing missing values from the 'vehicales_data' data frame
#and stores the result in a new data frame called 'vehicles_data_input'
vehicales_data_input <- na.omit(vehicales_data)
# Display the data frame's initial few rows.
head(vehicales_data_input)
#Before removing outliers from the dataframe, double-check the data frame's dimensions.
dim(vehicales_data_input)
# Display the data values before removing outliers.
boxplot(vehicales_data_input)







#Detect and remove outliers
#These two lines detect and remove outliers from the 'vehicales_data_input' data frame.
#The apply functions applies the 'scale' function to each coloum of the "vehicals_data_input"
#data frame to calculate the z-scores , which are then used to identify the outliers
#The outliers are removed using logical indexing, and the result is stored in a new
#dataframe called 'vehicle_outliers_remove'
z_scores <- apply( vehicales_data_input,2,function(x) abs(scale(x,center = TRUE, scale = TRUE) ) )
vehical_outliers_remove <- vehicales_data_input[rowSums(abs(z_scores) < 3) == ncol(z_scores), ] 

#These two lines displays the dimensions of the 'vehicales_outliers_remove' data frame and
dim(vehical_outliers_remove)
#draw a box plot for data values after outlier removal.
boxplot(vehical_outliers_remove)


#Scale the data
#These lines perform feature scaling on the 'vehicales_outliers_remove' data frame using
#the 'scale' function .This standardizes each coloum to have a mean of 0 and a standard
#deviation of 1. The box lot of the scaled data is shown
vehical_outliers_remove_and_scale <- scale(vehical_outliers_remove)
#draw a box plot for data values after outlier removal.
boxplot(vehical_outliers_remove_and_scale)


#Visualize NBclust information using Euclidean distance
#These lines apply the 'NbClust' function to the scaled data to determine the optimal
#number of clusters.The 'set.seed' function ensures that the results are reproducible.
#the 'data' argument specifies the input data. the 'distance' argument specifies the distance
#measure to use ,and the 'min.nc' and 'max_nc' arguments specify the minimum and maximum numbers
#of clusters to consider, respectively.The 'method' argument specifiers the clustering algorithm
#to use , and the 'index' argument specifies the clustering validity to calculate.
set.seed(123)
nb <- NbClust(data =vehical_outliers_remove_and_scale ,diss= NULL , distance ="euclidean",
              min.nc=2 ,max.nc=10, method ="kmeans", index="all")

#This line displays a table showing the number of times each number of clusters was selected
#as the best by the different clustering validity indices.
table(nb$Best.partition)

#using Manhattan distance
#Visualize NBclust information using Manhattan distance
set.seed(123)
nb <- NbClust(data = vehical_outliers_remove_and_scale, diss= NULL ,distance="manhattan",
              min.nc= 2, max.nc =10 , method ="kmeans" , index ="all")
#Print the recommended number of clusters using the basic rule for the different methods
table(nb$Best.partition)
#Determine the ideal number of clusters for k-clustering using the elbow method.
#An empty vector is used as the variable's initial value for "wss". For loop iterates from 1 to 10.
# then the "kmeans" function is invoked with "i" as the n of clusters for each value of i.
#After that, the sum of the "withinss" valves for each iteration is kept in the "wss" vector.
#Identifying the point on the plot (the curve's "elbow") where the decline in the tetal
#within-cluster sum of squares levels out will provide the optimal number of clusters in this situation.
#he results are then plotted using the "fviz_nbclust" function from the "factoextra" package.
set.seed(123)
wss <- c()
for (i in 1:10) wss[i] <- sum(kmeans(vehical_outliers_remove_and_scale,centers = i)$withinss)  

#Determine the elbow point.
diffs <- c(0 , diff(wss))
elbow <- which(diffs < mean(diffs))[1]

#print the number of clusters at the elbow point
cat("Number of clusters at the elbow point : ", elbow , "\n")
#Plot the results
fviz_nbclust(vehical_outliers_remove_and_scale, kmeans, method = 'wss')  


#This line creates a visualization of the optimal number of clusters using the gap statistic method. The 'fviz_nbclust'
#function takes the scaled data and applies the 'kmeans' clustering algorithm with the 'gap_start' method to estimate
#the number of clusters. The 'iter.max' argument sets the maximum number of iterations for each number of clusters.

fviz_nbclust(vehical_outliers_remove_and_scale, kmeans, method ='gap_stat',iter.max =20 ) 

#This code is used to determine the optimal number of clusters for the vehicle dataset using k-means clustering and silhouette method.
#The fviz_nbclust function from the factoextra package is used to visualize the results.
fviz_nbclust(vehical_outliers_remove_and_scale,kmeans, method = 'silhouette') 

#using the preferred k value derived from the elbow and silhouette method, perform k-means clustering.used 2 as the preferred k value 
favored_k <- 2
#fit the k-means clustering model using the optimal number of clusters
kmeans_fit <- kmeans(vehical_outliers_remove_and_scale, centers = favored_k, nstart = 25)
#Visualized the clusters using a scatterplot matrix
fviz_cluster(kmeans_fit , data= vehical_outliers_remove_and_scale)
#Visualized the clusters in a cluster plot
fviz_cluster(kmeans_fit , data = vehical_outliers_remove_and_scale , ellipse.type = "euclid",
             star.plot= TRUE , repel = TRUE , ggtheme = theme_minimal())

#plot clusters 
plotcluster(vehical_outliers_remove_and_scale, kmeans_fit$cluster)

#Calculate BSS , TSS , AND WSS
BSS = kmeans_fit$betweenss
TSS = kmeans_fit$totss
WSS = kmeans_fit$withinss

# print information about the k-means clustering
kmeans_fit
cat("Centroids:\n",kmeans_fit$centers)
cat("Cluster membership:\n",kmeans_fit$cluster)
cat("BSS(Between cluster sums of squares):", BSS)
cat("TSS(Total sum of squares):", TSS)
cat("WSS(Within cluster sum of squares):", WSS)
cat("BSS/TSS ratio:", BSS/TSS)


#Create bar plot of BSS and TSS
barplot(c(BSS, TSS), names.arg = c("BSS","TSS"),
        main = "BSS , and TSS for k-means clustering",
        ylab = "Sum of squares" , col = c('red','green'))


#Create bar plot of WSS indices
barplot(WSS, names.arg= c("[1]", "[2]"),
        main ="BSS , indeces for k-means clustering ",
        ylab ="Sum of squares", col = c("blue","yellow"))

#Calculate silhouette width
sil.width <- silhouette(kmeans_fit$cluster , dist(vehical_outliers_remove_and_scale) )

#Visualized the silhouette plot
fviz_silhouette(sil.width)




#______________________SUBTASK2_________________________
#Apply PCA to the dataframe after removing outliers and scaling.
pca_output <- prcomp(vehical_outliers_remove_and_scale , center = TRUE , scale = TRUE)

#Calculate Eigenvalues & Eigenvectors
eig <- pca_output$sdev^2 #Eigenvalues
eiv <- pca_output$rotation #Eigenvectors

#Display Eigenvalues & Eigenvectors
cat("Eigenvalues:\n", eig)
cat("Eigenvectors:\n",eiv)

#produce a summary for the PCA output.
summary(pca_output)

#build a transformed dataframe.
vehicles_transform =as.data.frame(-pca_output$x[,1:7])  

#print first few rows of the dataframe
head(vehicles_transform)

# apply k-means clustering using the NbClust function on the newly transformed dataset Using the Euclidean distance 

set.seed(123)
nb <- NbClust(data = vehicels_transform ,diss= NULL , distance ="euclidean",
              min.nc=2 ,max.nc=10, method ="kmeans", index="all")
#print the recommended amount of clusters for each approach based on the majority rule. 
table(nb$Best.partition)

#Using Manhattan distance 
set.seed(123)
nb <- NbClust(data = vehicels_transform, diss= NULL ,distance="manhattan",
              min.nc= 2, max.nc =10 , method ="kmeans" , index ="all")
#print the recommended amount of clusters for each approach based on the majority rule.  
table(nb$Best.partition)


#choosing the ideal number of clusters for k-means clustering using the elbow approach.
#The initialization of the "wss" variable creates an empty vector.
#The "kmeans" function is invoked with "i" as the number of clusters for each iteration of the for loop,which iterates from 1 to 18.
#The "withinss" value is then added up for each iteration, and it is kept in the "wss" vector.

set.seed(123)
wss <- c()
for (i in 1:10) wss[i] <- sum(kmeans(vehicels_transform,centers = i)$withinss)  

#Identify the elbow point 
diffs <- c(0 , diff(wss))
elbow <- which(diffs < mean(diffs))[1]

#print the number of clusters at the elbow point 
cat("Number of clusters at the elbow point : ", elbow , "\n")

#plot results 
fviz_nbclust(vehicels_transform , kmeans , method ='wss')


#The ideal number of clusters for K-means clustering can be found using the Gap statistics approach.
#The input data matrix that we want to cluster is named "vehical_outliers_remove_and_scale."
# 'kmeans': The clustering algorithm that will be applied (for example, hierarchical clustering or kmeans).
# method = 'gap_stat': the method used to choose the optimal number.
#Number of clusters (e.g., WSS, gap statistic, silhouette width).
#The method (e.g. silhouette width, gap statistic, or WSS) to be used to determine the ideal number of clusters is method = 'gap_stat'. 
#The maximum number of iterations that will be carried out by the tering algorithm is specified by iter.max.
fviz_nbclust(vehicels_transform, kmeans , method = 'gap_stat', iter.max = 20 )

#This line of code generates a plot of the silhouette width for different numbers of clusters,
#helping to determine the optimal number of clusters to use in K-means clustering

fviz_nbclust(vehicles_transform ,kmeans , method = 'silhouette')
#k-means clustering should be done using the most preferred k value obtained through the elbow and silhouette method.
#use 2 as most preferred k value 
favoured_k <- 2 
#The optimum amount of clusters should be used to suit the k-means clustering model.
kmeans_fit_pca <- kmeans(vehicels_transform , centers = favoured_k , nstart = 25)
#Visualize the clusters using a scatterplot matrix 
fviz_cluster(kmeans_fit_pca ,data =  vehicels_transform)
#Visualize the clusters in a cluster plot
fviz_cluster(kmeans_fit_pca , data = vehicels_transform , ellipse.type ="eculid",
             star.plot = TRUE , repel = TRUE , ggtheme = theme_minimal())
#plot clusters
plotcluster(vehicles_transform, kmeans_fit_pca$cluster)

# calculate BSS, TSS AND WSS.
BSS = kmeans_fit_pca$betweenss
TSS = kmeans_fit_pca$totss
WSS = kmeans_fit_pca$withinss

# print information about the k-means clustering.
kmeans_fit_pca
cat("Centroids:\n", kmeans_fit_pca$centers)
cat("Cluster membership:\n", kmeans_fit_pca$cluster)
cat("BSS(Between Cluster Sums of Squares):", BSS)
cat("TSS(Total Sum of Squares):", TSS)
cat("WSS(Within Cluster Sums of Squares):", WSS)
cat("BSS/TSS ratio:", BSS/TSS)

# Create bar plot of BSS and TSS 
barplot( c( BSS, TSS ), names.arg = c("BSS" , "TSS"),
         main = "BSS , amd TSS for K-means clustering ",
         ylab = "Sum of squares", col = c("red","green") )

# Create bar plot of WSS indices
barplot( WSS, names.arg = c("[1]" , "[2]"),
         main = "WSS indeces for k-means clustering",
         ylab = "Sum of squares", col = c("blue","yellow") )

# Calculate silhouette width
sil.width <- silhouette(kmeans_fit_pca$cluster, dist(vehicles_transform))
         
# visualize the silhouette plot.
fviz_silhouette( sil.width )
## using the Calinski-Harabasz index, find the ideal number of clusters for this dataset.

# For a range of cluster solutions from 1 to 10, the gap statistic is calculated in the first line of code.

#utilizing the R "cluster" package's clusGap() function.



# The iter.max parameter indicates the maximum number of iterations for each set of initial centers, 
#and the nstart parameter gives the number of initial sets of cluster centers for each number of clusters.




# The calculated gap statistic values for cluster solution are contained in the gap object that results.
#With the help of the plot() function, the second line of code plots the calculated gap statistic valves under the heading "Calinski-Harabasz Index."

# Draw a Calinski-Harabasz index plot.
gap <- clusGap(vehicels_transform , FUN = kmeans ,K.max = 10 , nstart = 25, iter.max = 20 )
plot(gap, main="Calinski-Harbasz Index ")
 
#Print the optimal number of clusters 
print(gap$Tab)
