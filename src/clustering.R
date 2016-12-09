# import data
team.attrs = read.csv('team_attrs.csv')

# figure out how many clusters by calculating
# wss for 1:15 clusters, then plotting them to see
# at what point we lose benefit of more clusters
wss = (nrow(team.attrs) - 1) * sum(apply(team.attrs, 2, var))
for (i in 2:15) wss[i] = sum(kmeans(team.attrs, centers = i)$withinss)

# looking at the trend in within group sum of squares, we choose k = 3
plot(1:15, wss, type='b', xlab = 'Number of clusters', ylab='Withing groups sum of squares')

# perform kmeans clustering
clusters = kmeans(team.attrs, 3)

# plot PCA-reduced data, coloured for clusters
pca = prcomp(team.attrs)
plot(pca$x[, 1], pca$x[, 2], col=clusters$cluster + 1, 
     pch=clusters$cluster, xlab = 'First Principal Component', ylab = 'Second Principal Component')
