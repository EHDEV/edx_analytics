# Week 6 - Introduction to Clustering

# Video 6

# After following the steps in the video, load the data into R
movies = read.table("./data/moviesLens.txt", header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates in the movies dataset. Duplicates have the same values in each column
movies = unique(movies)

# Take a look at our data again:
str(movies)



# Video 7 - Clustering movies to make recommendations using Genre

# Two steps to hierarchical clustering. Calculate euclidean distance then cluster
# Clustering using the Genre variable not on the title variable. So we use columns 2 - 20 for clustering using Genre variable


# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering - hclust() function. 
# Ward method cares about the distance using centroid distance and variance in each of the clusters
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)

# Use your understanding of the problem to select the number of clusters you want
# Assign each of the data points (rows) to clusters it belongs to: 
clusterGroups = cutree(clusterMovies, k = 10) # k= 10 => 10 clusters

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
# Divide the data into 10 clusters and computes the average value of the Action variable in each cluster.
# By computing the average, we are actually computing the percentage of of the movies in each cluster that 
# have the Action variable as 1 (i.e. Movies in each cluster that are Action)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat the above to each Genre variable and store in a table to better analyze the data

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257] # This tells us which cluster the 257th row of the data belongs to



# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# According to our recommendation algorithm, we can recommend to Amy movies from Cluster 2

# Exercise:

cluster2g <- cutree(clusterMovies, k=2)
str(cluster2g)
summary(cluster2g)

tapply(movies$Documentary,cluster2g, mean)


