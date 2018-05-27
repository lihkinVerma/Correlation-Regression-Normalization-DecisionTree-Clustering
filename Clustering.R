
#------------------------------------------------
# Kmeans clustering Technique
#------------------------------------------------

#1 reading data
ds = read.csv("uci_dataset_a.csv")
ds.features = ds
ds.features$CLASS = NULL

#2 ---------making single cluster ---------------
kmk1 <- kmeans(ds.features, 1)
kmk1

#2.1 checking cluster features
kmk1$centers
kmk1$cluster
kmk1$size
table(ds$CLASS , kmk1$cluster)

#2.2 plotting the clusters
png(file = "kmeank1result.png")
plot(ds[c("ATT1","ATT18")],col = kmk1$cluster)
png(file = "kmeank1CLASS.png")
plot(ds[c("ATT1","ATT18")],col = ds$CLASS)
dev.off()
dev.off()

#3 ---------making two clusters ---------------
kmk2 <- kmeans(ds.features, 2)
kmk2

#3.1 checking cluster features
kmk2$centers
kmk2$cluster
kmk2$size
table(ds$CLASS , kmk2$cluster)

#3.2 plotting the clusters
png(file = "kmeank2result.png")
plot(ds[c("ATT1","ATT18")],col = kmk2$cluster)
png(file = "kmeank2CLASS.png")
plot(ds[c("ATT1","ATT18")],col = ds$CLASS)
dev.off()
dev.off()

#4 ---------making three clusters ---------------
kmk3 <- kmeans(ds.features, 3)
kmk3

#4.1 checking cluster features
kmk3$centers
kmk3$cluster
kmk3$size
table(ds$CLASS , kmk3$cluster)

#4.2 plotting the clusters
png(file = "kmeank3result.png")
plot(ds[c("ATT1","ATT18")],col = kmk3$cluster)
png(file = "kmeank3CLASS.png")
plot(ds[c("ATT1","ATT18")],col = ds$CLASS)
dev.off()
dev.off()

#------------------------------------------------
# Kmeans clustering Technique
#------------------------------------------------

#------------making single cluster---------------
library(cluster)
library(factoextra)
data<-read.csv("uci_dataset_a.csv")
pam.res <- pam(data, 1)
print(pam.res)
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "norm", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


#------------making two clusters-----------------
library(cluster)
library(factoextra)
data<-read.csv("uci_dataset_a.csv")
pam.res <- pam(data, 2)
print(pam.res)
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "norm", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

