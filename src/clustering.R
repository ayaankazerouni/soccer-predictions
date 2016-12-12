# import data
team.attrs = read.csv('team_attrs.csv')
team.attrs$date = as.Date(team.attrs$date)
splits = split(team.attrs, team.attrs$date)

# figure out how many clusters by calculating
# wss for 1:15 clusters, then plotting them to see
# at what point we lose benefit of more clusters
wss = (nrow(team.attrs[, 2:9]) - 1) * sum(apply(team.attrs[, 2:9], 2, var))
for (i in 2:12) wss[i] = sum(kmeans(team.attrs[, 2:9], centers = i)$withinss)

# looking at the trend in within group sum of squares, we choose k = 3
plot(1:12, wss, type='b', xlab = 'Number of clusters', ylab='Within groups sum of squares',
     main= 'No. of clusters vs. WSS')

# clustering on team attrs for all years
clusters.all = kmeans(team.attrs[, 2:9], 3)

centers = NULL
set.seed(10)
par(mfrow=c(2,3))
for (i in 1:length(splits)) {
  # perform kmeans clustering
  data = splits[[i]]
  data = data[, !(names(data) == 'date')]
  clusters = kmeans(data, 3)
  centers[[i]] = clusters$centers
  
  # plot PCA-reduced data, coloured for clusters
  pca = prcomp(data)
  plot(pca$x[, 1], pca$x[, 2], col=clusters$cluster + 1,
       pch=clusters$cluster, xlab = 'PC1', ylab = 'PC2',
       main = paste('201', i - 1, sep = ''))
}