library(datasets)
library(dendextend)

data("USArrests")
head(USArrests)

# Standardize the variables
USArrests_std <- scale(USArrests[,1:3])

# Check the summary statistics of the standardized variables
summary(USArrests_std)

# Perform hierarchical clustering using Ward's method
hc <- hclust(dist(USArrests_std), method="ward.D2")

# Visualize the dendrogram
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k=4)
plot(dend)

# Cut the dendrogram to obtain the clusters
clusters <- cutree(hc, k=4)

# Add the cluster labels to the dataset
USArrests$cluster <- factor(clusters)

# Check the cluster sizes
table(clusters)

library("ggplot2")
library("gridExtra")

# Visualize the clusters
# Create the charts for Murder, Assault and Rape
plot_1 <- ggplot(USArrests, aes(x=Murder, y=Assault, color=cluster)) +
  geom_point(size=3) +
  labs(x="Murder", y="Assault", title="Hierarchical Clustering of USArrests")

plot_2 <- ggplot(USArrests, aes(x=Rape, y=Assault, color=cluster)) +
  geom_point(size=3) +
  labs(x="Rape", y="Assault", title="Hierarchical Clustering of USArrests")

plot_3 <- ggplot(USArrests, aes(x=Rape, y=Murder, color=cluster)) +
  geom_point(size=3) +
  labs(x="Rape", y="Murder", title="Hierarchical Clustering of USArrests")






# Create a list of state names by cluster
states_by_cluster <- split(names(clusters), clusters)

# Print the list of state names by cluster
cat("States in Cluster 1:", paste(states_by_cluster[[1]], collapse=", "), "\n")
cat("States in Cluster 2:", paste(states_by_cluster[[2]], collapse=", "), "\n")
cat("States in Cluster 3:", paste(states_by_cluster[[3]], collapse=", "), "\n")
cat("States in Cluster 4:", paste(states_by_cluster[[4]], collapse=", "), "\n")