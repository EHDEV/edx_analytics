# Week 6 - Recitation

# Video 2
# Grayscale images - a matrix of pixel intensity values ranging from 0 (black) to 1 (white)
# Number of columns, width of image, number of rows, height
# Morph the 7x7 matrix data into a vector of 49 variables
# First step in hclust is calculate the distances
# For each element in the intensity vector, we need to calculate its distance from the 48 other elements
# 49 * 48 pairwise distances are required. However, due to symmetry we only need to calc half of them. 49 * 48/2
# (n * (n-1))/2 pairwise distance calculations
setwd('./Documents/workspace/edx_analytics/')
flower = read.csv("./data/flower.csv", header=FALSE) # Data has no header
str(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
# 50 rows and 50 columns => 50 pixels in width, 50 pixels in height

# Turn matrix into a vector (Crucial step)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 = as.vector(flower)
str(flowerVector2)

# Compute distances - 1st step in hierarchical clustering
distance = dist(flowerVector, method = "euclidean")



# Video 3

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward") # minimum variance method - minimize variance and distance within each cluster


# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
# plotting rectangles around the clusters
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters # a vector that assigns each intensity value in the data to a cluster
# Find mean intensity values of each of the clusters
tapply(flowerVector, flowerClusters, mean)

# Plot the image and the clusters
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))



# Video 4

# Let's try this with an MRI image of the brain

healthy = read.csv("./data/healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
n = 566 * 646
n# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
# distance = dist(healthyVector, method = "euclidean") # data is too large to compute distances - Avoid and use a different method of clustering

# We have an error - why?
str(healthyVector)
# 365636 values in the vector
# Let's calculate how many distances are computed

n = 365636
(n * (n-1))/2 
# 66844659430 (66 Billion) distance computations are required. We need a different clustering method

# K-means clustering algorithm - partition the data into k clusters in which each data point belongs to 
# the cluster whose mean is the nearest
# Number of clusters, randomly assign each data point to a cluster, compute k centroids, reassign each point
# to the nearest Cluster centroid, recompute cluster centroids, reassign points and recompu centroid until no
# more improvements are made



# Video 5

# 1. Specify number of clusters. But how to select K
# Ideally, each point in the image is assigned based on a tissue class or particular substance
# that are known by the medical community, like gray matter or white matter

k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000) # kmeans(data(vector), no. of clusters, maximum iterations)

str(KMC)
# Cluster vector in KMC assigns each intensity value in the healthyVector to a cluster (1, 2,3,4,5)
# Extract clusters from KMC
healthyClusters = KMC$cluster
# Mean intensity values can also be extracted from the KMC object as follows, KMC$centers
# We can also extract the size of each of our clusters
# Eg, the largest cluster is the third one, and has the least mean intensity value which corresponds
# with the darkest parts of our image - which shows that the image is mostly dark
KMC$centers[2]
str(healthyClusters)

# Plot the image with the clusters
# Output the segmented image
# First convert it back to a matrix as it was originally
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))



# Video 6

# Apply to a test image
 
tumor = read.csv("./data/tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
# Use the healthyVector that we created earlier as a training set and the tumor vector as a test set
install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector) # as.kcca(cluster model, training set)
# kcca object class -> K centroid cluster analysis
# we need to convert the clustering algorithm to a kcca object
summary(KMC.kcca)
tumorClusters = predict(KMC.kcca, newdata = tumorVector) 
str(tumorClusters)
# Visualize the clusters. Convert to matrix first
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))

