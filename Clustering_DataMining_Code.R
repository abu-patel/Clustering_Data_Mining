
#------------------------------------------------------------------------#
#---------------------------UNSUPERVISED---------------------------------#
#------------------------------------------------------------------------#
set.seed(55)
dataset<-load("cluster_data.RData")

dataset # Contains y in form of character 
class(dataset) # Character 
dim(dataset)
colnames(dataset) # Dataset contains only a single character "y" , hence colnames throws value as NULL 
head(dataset) # Hives only one value as "y"

# Perform EDA on y 
class(y)  # Y is character vector in form of matrix. ( Matrix is a special array with 2 dimensions)
dim(y) # Gives the dimension of array ( 1000 rows *784 columns/variables )
View(y)

#Now, let us perform PCA to reduce the dimension of our data
set.seed(55)
pr.out<-prcomp(y, scale= TRUE) # Scaling the data before PCA 
summary(pr.out)
names(pr.out) # Gives 5 important parameters of PCA -  standard deviation, rotation, cente- Mean, scale- scaled values , x - Principal Component vectors

# Visualising the first few principal components 
Cols<-function(vec){
  cols<-rainbow(length (unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
# Plotting the first two PC 
plot(pr.out$x[,1:2], col=Cols(y), pch=19, xlab="Z1", ylab="Z2") # The colour indicates each observation belonging to certain variable 

# Plotting the first three PC 
plot(pr.out$x[,c(1,3)], col=Cols(y), pch=19, xlab="Z1", ylab="Z3") # The observations belonging to a single 
#Observation: The PCs are not clearly separated


#Compute the variance of the PCs
pr.var<-pr.out$sdev^2
pr.var
#Plotting the cum variance explained by the PCs
pve<-pr.var/sum(pr.var)
pve
par(mfrow=c(1,4))
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of the Variance Explained", ylim=c(0,1), type="b")

#We will be using the elbow method to select the optimal PCs
# We see that the elbow occurs around when PC=400
# If we plot the the cumulative variance of only first 400 Principal Components
plot(cumsum(pve), xlab=" Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), xlim=c(0,400), type="b")

# Finding the cumulative sum of the first 400 PC 
cumsum.400<-cumsum(pve[1:400])
cumsum.400

data_pca = pr.out$x[,1:400]
#Therefore, we will be using the first 400 PCs for our analysis in this work.
#That is about 50% reduction in our data dimensions

#------ USING KMEANS TO DETERMINE OPTIMAL CLUSTER ---------------#
library(factoextra)
set.seed(15)
#We are determining the optimal cluster using silhoutte score for kmeans
#Silhouette score ranges fron 0 to 1, with 1 being the best
#For more on Silhouette score: https://vitalflux.com/kmeans-silhouette-score-explained-with-python-example/
fviz_nbclust(data_pca, kmeans, method='silhouette', k.max = 20)
#The above gives optimal cluster as 2 using Silhouette score for kmeans.
#However, the silhouette score is poor

#Using Elbow plot to determine optimal cluster for kmeans
#see: #see; https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_nbclust
fviz_nbclust(data_pca, FUN = kmeans, method = "wss", k=20)
#There is no clear elbow from the plot. Let us check the Gap stats method

#Using Gap stat to determine optimal cluster for kmeans
#https://hastie.su.domains/Papers/gap.pdf
set.seed(15)
fviz_nbclust(data_pca, FUN = kmeans, method = "gap_stat", k=20, nboot = 500)
#The optimal cluster was predicted as 9

require(vegan)
set.seed(15)
#This function is a wrapper for the kmeans function, to determine optimal 
#cluster using Calinski-Harabasz (1974) criterion
#see; https://search.r-project.org/CRAN/refmans/fpc/html/calinhara.html
fit <- cascadeKM(data_pca, 2, 20, iter = 100, criterion = 'calinski')
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
#suggests 1 as optimal number of cluster

#This function is a wrapper for the kmeans function, to determine optimal 
#cluster using simple structure index ("ssi")  criterion
set.seed(15)
fit <- cascadeKM(data_pca, 2, 20, iter = 100, criterion = 'ssi')
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
ssi.best <- as.numeric(which.max(fit$results[2,]))
cat("SSI criterion optimal number of clusters:", ssi.best, "\n")
#Suggests optimal number of clusters as 10

#------ USING HIERACHICAL CLUSTERING (HC) TO DETERMINE OPTIMAL CLUSTER --------#
set.seed(15)
#We are determining the optimal cluster using silhoutte score for HC
fviz_nbclust(data_pca, FUN = hcut, method='silhouette', k.max=20)
#The optimal cluster was predicted as 8, even though the silhouette score was very small

#Using Elbow plot to determine optimal cluster for HC
set.seed(15)
fviz_nbclust(data_pca, FUN = hcut, method='wss', k.max=20)
#There is no clear elbow, but a range of 7-8 can be selected by joining the 2 slopes
#with 2 straight lines

#Using Dunn index to determine optimal cluster for HC
set.seed(15)
library("clValid")
distance.1<-dist(data_pca)
hc.out<-hclust(distance.1)
for (i in 1:10) {
  hc.clusters<-cutree(hc.out,i)
  print(dunn(distance = distance.1, hc.clusters))
}  
#Suggest optimal cluster as 2

#Plot Dunn Index bar chart
dat_ <- c(0.825, 0.44, 0.46, 0.47, 0.51, 0.51, 0.31,0.31,0.32)
M <- c(2,3,4,5,6,7,8,9,10)
barplot(dat_,names.arg=M,xlab="cluster number",ylab="Dunn Index",col="blue",
        main="Dunn Index for HC",border="red")


#Using Gap stat to determine optimal cluster for HC
#SLOW TO RUN, TOOK 4HRS
set.seed(15)
fviz_nbclust(data_pca, FUN = hcut, method = "gap_stat", k.max=20, nboot = 500)
#The optimal cluster was selected as 20.



library(fpc)
# PAM stands for "partition around medoids" is an algorithm that searches for k representative objects in a data set (k medoids) 
# and then assigns each object to the closest medoid in order to create clusters. 
# Its aim is to minimize the sum of dissimilarities between the objects in a cluster 
# and the center of the same cluster (medoid)
# see;https://www.cs.umb.edu/cs738/pam1.pdf
set.seed(15)
pamk.best <- pamk(data_pca,krange=2:20, criterion="asw", usepam=FALSE)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
#suggests number of cluster as 2

pamk.best <- pamk(data_pca,krange=2:20, criterion="ch", usepam=FALSE)
cat("number of clusters estimated by optimum Calinski-Harabasz score:", pamk.best$nc, "\n")
#suggests number of cluster as 2



#------------ Model-based clustering---------------#
library(mclust)
# Model-based clustering based on parameterized finite Gaussian mixture models.
# Models are estimated by EM algorithm initialized by hierarchical model-based 
# agglomerative clustering. The optimal model is then selected according to BIC.
#see; https://www.rdocumentation.org/packages/mclust/versions/5.4.7/topics/Mclust
set.seed(15)
d_clust <- Mclust(data_pca, G=2:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
#suggests number of cluster as 2
par(mar = c(10, 10, 10, 10))
par("mar")
dev.off()
plot(d_clust)
d_clust$classification

#-----Affinity propagation (AP) Clustering ---------------------#
library(apcluster)
set.seed(15)
# Affinity propagation (AP) is a relatively new clustering algorithm that has been introduced by
# Brendan J. Frey and Delbert Dueck. See: https://cran.r-project.org/web/packages/apcluster/vignettes/apcluster.pdf
#The choice negDistMat(r=2) is the
#standard similarity measure used in the papers of Frey and Dueck - negative squared distances
#set.seed(15)
y.apclus <- apcluster(negDistMat(r=2), data_pca)
cat("affinity propogation optimal number of clusters:", length(y.apclus@clusters), "\n")
plot(y.apclus, data_pca[,1:2])
#suggests number of cluster as 114



###----------Clustering Using CLARA ALGORITHM -------#
# CLARA (Clustering Large Applications, (Kaufman and Rousseeuw 1990)) is 
# an extension to k-medoids (PAM) methods to deal with data containing a large 
# number of objects (more than several thousand observations) in order to reduce 
# computing time and RAM storage problem.
# see: https://www.sciencepubco.com/index.php/ijet/article/view/21472/14157
set.seed(15)
#We are determining the optimal cluster using silhoutte score for HC
fviz_nbclust(data_pca, FUN = cluster::clara, method='silhouette', k.max=20)
#Suggested optimal number of clusters as 2

set.seed(15)
#We are determining the optimal cluster using elbow plot for HC
fviz_nbclust(data_pca, FUN = cluster::clara, method='wss', k.max=20)
#There are multiple eblows, which 2 is one of them


#---Using Combination of Indices to check optimal number of clusters-----#

set.seed(15)
library("NbClust")
# NbClust package provides 30 indices for determining the number of clusters and
# proposes to user the best clustering scheme from the different results obtained
# by varying all combinations of number of clusters, distance measures, 
# and clustering methods.
# See: https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust

#Kmeans
nb <- NbClust(data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
#It concluded that According to the majority rule, the best number of clusters is  2

#All the ones below are Heirarchical based 
set.seed(15)
nb <- NbClust(data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "single")
#It concluded that According to the majority rule, the best number of clusters is  2


set.seed(15)
nb <- NbClust(data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "average")
#It concluded that According to the majority rule, the best number of clusters is  2

set.seed(15)
nb <- NbClust(data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")
#It concluded that According to the majority rule, the best number of clusters is  2

set.seed(15)
nb <- NbClust(data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")
#It concluded that According to the majority rule, the best number of clusters is  6


#CONCLUSION: Optimal Number of cluster is 2 based on the results above

#Clustering with KMeans using 2 clusters
set.seed(15)
km.out<-kmeans(data_pca,2,nstart=20)
names(km.out) # Exploring the names of the parameters after performing kmeans 
# Lets check the clusters of kmeans 
km.clusters<-km.out$cluster
km.clusters

#view clusters
fviz_cluster(km.out, data_pca[,1:2],
             palette = c("#2E9FDF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

