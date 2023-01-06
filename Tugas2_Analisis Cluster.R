library(readxl)
data_IPM <- read_excel("E:/KULIAH/SEMESTER 5/Analisis Multivariat I/Data IPM Sumut.xlsx")
View(data_IPM)

X1 <- data_IPM$`Angka Harapan Hidup (Tahun)`
X2 <- data_IPM$`Harapan Lama Sekolah (Tahun)`
X3 <- data_IPM$`Pengeluaran per Kapita Disesuaikan (Ribu Rupiah)`
X4 <- data_IPM$`Rata-rata Lama Sekolah (Tahun)`
data <- data.frame(X1,X2,X3,X4)
data

#Statistika Deskriptif
statdes <- summary(data_IPM)
statdes 

data_cluster <- scale(data)
data_cluster

#k-means
kmeans <- kmeans(data_cluster,2)
kmeans
kmeans.cluster <- kmeans$cluster
cluster_kmeans <- cbind(data_cluster,kmeans.cluster)
cluster_kmeans

#k-medoid
library(cluster)
kmedoid <- pam(data_cluster,2)
kmedoid
kmedoid.cluster <- kmedoid$clustering
cluster_kmedoid <- cbind(data_cluster,kmedoid.cluster)
cluster_kmedoid

#agnes
library(cluster)
agnes <- agnes(data_cluster,metric="euclidean", method="single")
agnes
#plot dendogram
pltree(agnes,cex=0.6,hang=-1,main="Dendogram of AGNES")
agnes.cluster <- cutree(agnes,k=2)
agnes.cluster
rect.hclust(agnes,k=2,border=2:10)
#hasil cluster agnes
cluster_agnes <- cbind(data_cluster,agnes.cluster)
cluster_agnes

#diana
#metric = "euclidean" atau manhattan"
diana <- diana(data_cluster,metric="euclidean")
diana
#plot dendogram
pltree(diana,cex=0.6,hang=-1,main="Dendogram of DIANA")
diana.cluster <- cutree(diana,k=2)
diana.cluster
rect.hclust(diana,k=2,border=2:10)
#hasil cluster diana
cluster_diana <- cbind(data_cluster,diana.cluster)
cluster_diana

#hasil cluster
hasil_cluster <- cbind(data_cluster,kmeans.cluster,kmedoid.cluster,agnes.cluster,diana.cluster)
hasil_cluster
