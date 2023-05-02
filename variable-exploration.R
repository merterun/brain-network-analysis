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

# Visualize the clusters
library(ggplot2)
ggplot(USArrests, aes(x=Murder, y=Assault, color=cluster)) +
  geom_point(size=3) +
  labs(x="Murder", y="Assault", title="Hierarchical Clustering of USArrests")
