require(RSQLite)
require(ggplot2)
require(devtools)
require(tidyverse)
require(broom)
require(mgcv)
require(modelr)
require(lme4)
require(factoextra)
require(plotly)



#Connect to the database
conn <- dbConnect(SQLite(), 'baseball.db')

#Get the data from the database
clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolenMaster cs LEFT JOIN catcherHittingAll ch ON cs.Catcher_Name = ch.player_name;")

#Place each catcher's name as the rowname to properly display on graph
row.names(clusteringData) <- catcherStrikesStolenMaster$Catcher_Name

#Scale the data to ensure clean clustering
dataScaled <- scale(clusteringData)

#Determine the proper number of clusters
fviz_nbclust(dataScaled, FUNcluster = kmeans, method = "wss")

#Set the randomization seed to ensure consistency among multiple runs
set.seed(36)

#Create the clusters
clusters <- kmeans(dataScaled, 4, nstart = 50)

#Plot the clusters 
ggplotly(fviz_cluster(clusters, data = dataScaled, repel = FALSE, ggtheme = theme_minimal(), main = "K-Means Clusters Among MLB Catchers", alpha = 0.2, labelsize = 7, pointsize = 1), dynamicTicks = TRUE)

#Return a list with the actual averages for the clusters
aggregate(clusteringData, by = list(cluster = clusters$cluster), mean)

#Place the catcher's cluster next to his data row
clusteringData <- clusteringData %>% mutate(cluster = clusters$cluster)
row.names(clusteringData) <- catcherStrikesStolenMaster$Catcher_Name

