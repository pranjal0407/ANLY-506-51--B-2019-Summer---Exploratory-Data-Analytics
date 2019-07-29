#K-Means Clustering
#an example with simulated data to demonstrate how the K-means algorithm works
set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

#using the kmeans() function: It is in the stats package
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

#we can see which cluster each data point will get assigned to by looking at the cluster element of the list returned by the kmeans() function
kmeansObj$cluster

#building heatmaps from K-means solutions
#step 1 to find the K-means solution
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj <- kmeans(dataMatrix, centers = 3)

#step 2 making an image plot using the K-means clusters
par(mfrow = c(1, 2))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n", main = "Original Data")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n", main = "Clustered Data")

#https://uc-r.github.io/kmeans_clustering
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#using the built-in R data set USArrests
df <- USArrests

#remove any missing value that might be present in the data
df <- na.omit(df)

#we start by scaling/standardizing the data using the R function scale
df <- scale(df)
head(df)

#using the functions get_dist and fviz_dist from the factoextra R package for computing and visualizing the distance matrix 
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#computing k-means clustering in R
k2 <- kmeans(df, centers = 2, nstart = 25)
k2
str(k2)

#If we print the results we'll see that our groupings resulted in 2 cluster sizes of 30 and 20
k2

#We can also view our results by using fviz_cluster
fviz_cluster(k2, data = df)

#standard pairwise scatter plots can be used to illustrate the clusters compared to the original variables.
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

#using several different values of k and examine the differences in the results
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Elbow method has been wrapped up in a single function (fviz_nbclust)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#To compute the gap statistic method we can use the clusGap function which provides the gap statistic and standard error for an output
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

#We can visualize the results with fviz_gap_stat which suggests four clusters as the optimal number of clusters
fviz_gap_stat(gap_stat)

#Extracting Results
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

#Hierarchical Cluster Analysis
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

#Agglomerative Hierarchical Clustering
# Dissimilarity matrix
d <- dist(df, method = "euclidean")

#Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

#Alternatively, we can use the agnes function
# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac
#[1] 0.8531583

#Ward's method identifies the strongest clustering structure of the four methods assessed
#methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

# visualizing the dendrogram
hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Divisive Hierarchical Clustering
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2")

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

#using the cutree output to add the the cluster each observation belongs to to our original data
USArrests %>%
  mutate(cluster = sub_grp) %>%
  head

#drawing the dendrogram with a border around the 4 clusters
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

#To use cutree with agnes and diana we can perform the following
# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

#two dendrograms can be compared as well
#distance matrix
res.dist <- dist(df, method = "euclidean")

#hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

#The output of tanglegram can be customized using many other options as follow
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#Average Silhouette Method
fviz_nbclust(df, FUN = hcut, method = "silhouette")