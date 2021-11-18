library(readr)
library(dplyr)
library(tidyverse)
library(FactoMineR)
library(factoextra)



path=getwd()
#importation data pour la segmentation
filename <- "dataseg2003.csv"
filename1 <- "dataseg2004.csv"
filename2 <- "data_mod.csv"
dataseg2003=read_csv2(file.path(path,"data",filename))
dataseg2004=read_csv2(file.path(path,"data",filename1))
data_mod=read_csv2(file.path(path,"data",filename2))

######################### Segmentation 2003 ##########################################
#tranformation des variables en variables quantitatives + reduction de dimension
res.famd <- FAMD(dataseg2003,ncp=30,graph = FALSE)
fviz_screeplot(res.famd,ncp=30)
datared=res.famd$ind$coord

res.famd$eig

# methode du coude
k.values <- 1:10
# fonction pour calculer "total within-cluster sum of square"
wss <- function(k) {
  kmeans(datared, k, nstart = 10 )$tot.withinss
}
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#kmean avec 5 classes
set.seed(123)
res.km2003<- kmeans(datared,5, nstart = 25)
fviz_cluster(res.km2003, data =datared,
             ellipse =FALSE,
             ggtheme = theme_bw()
)

y=res.km2003$cluster
#focus sur la classe 1
class_recap2003=table(res.km2003$cluster)
names(class_recap2003)=paste("classe",1:5)

data_classe1=dataseg2003[which(y==1),]
summary(data_classe1$DrivAge)
summary(data_classe1$VehAge)

#exportation
data_mod_2003=cbind(data_mod,classe=y)
filename <- "data_mod_2003.csv"
write_csv2(data_mod_2003,file.path(path,"data",filename))
######################### Segmentation 2004 ##########################################
#tranformation des variables en variables quantitatives + reduction de dimension
res.famd <- FAMD(dataseg2004,ncp=30,graph = FALSE)
fviz_screeplot(res.famd)
datared=res.famd$ind$coord

res.famd$eig

# methode du coude
k.values <- 1:10
# fonction pour calculer "total within-cluster sum of square"
wss <- function(k) {
  kmeans(datared, k, nstart = 10 )$tot.withinss
}
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#kmean avec 5 classes
set.seed(123)
res.km2004 <- kmeans(datared,5, nstart = 25)
fviz_cluster(res.km2004, data =datared,
             ellipse =FALSE,
             ggtheme = theme_bw()
)

y=res.km2004$cluster

#focus sur la classe 1
class_recap2004=table(res.km2004$cluster)
names(class_recap2004)=paste("classe",1:5)

data_classe1=dataseg2004[which(y==1),]
summary(data_classe1$DrivAge)
summary(data_classe1$VehAge)

#exportation
data_mod_2004=cbind(dataseg2004,classe=y)
filename <- "data_mod_2004.csv"
write_csv2(dataseg2004,file.path(path,"data",filename))


