#Library
library(psych)
library(GPArotation)
library(clValid)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyverse)
library(car)
library(readxl)
library(MVN)
library(biotools)
library(candisc)

##CLUSTER

#Import Data
data_cluster <- read_excel("E:/KULIAH/SEMESTER 5/Analisis Multivariat I/Data Tubes Anmul.xlsx")
data <- data.frame(data_cluster)
View(data)

#Statistik Deskriptif
statdes <- summary(data[,2:5])
statdes

#Histogram
hist(data$X1, main = "Histogram Openess to Experience",
     xlab = "Openess to Experience",
     ylab = "Banyak Nasabah", col = "grey")

hist(data$X2, main = "Histogram Conscientiousness",
     xlab = "Conscientiousness",
     ylab = "Banyak Nasabah", col = "grey")

hist(data$X3, main = "Histogram Extraverseion",
     xlab = "Extraverseion",
     ylab = "Banyak Nasabah", col = "grey")

hist(data$X4, main = "Histogram Agreableness", 
     xlab = "Agreableness",   
     ylab = "Banyak Nasabah", col = "grey")

hist(data$Y, main = "Histogram Tepat Waktu Membayar",
     xlab = "Tepat Waktu Membayar",
     ylab = "Banyak Nasabah", col = "grey")


#Jumlah Cluster Optimal
fviz_nbclust(data[,2:5], hcut, method = "silhouette")

#(1) Jarak Euclidean, Complete Linkage
agnes1 <- agnes(data[,2:5], metric="euclidean", method="complete")
agnes1
fviz_dend(agnes1, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Euclidean - Complete")
agnes.cluster1 <- cutree(agnes1,k=3)
agnes.cluster1

#Hasil Cluster 
cl1 <- data.frame(data,agnes.cluster1)
cl1
cluster1_1 <- filter(cl1,agnes.cluster1 == 1) ;cluster1_1
cluster1_2 <- filter(cl1,agnes.cluster1 == 2) ;cluster1_2
cluster1_3 <- filter(cl1,agnes.cluster1 == 3) ;cluster1_3

table(cluster1_1$Nasabah)
#Simpangan Baku seti  ap Cluster
sr1_cl1 = sd(c(cluster1_1$X1, cluster1_1$X2, cluster1_1$X3, cluster1_1$X4))  ;sr1_cl1
sr1_cl2 = sd(c(cluster1_2$X1, cluster1_2$X2, cluster1_2$X3, cluster1_2$X4))  ;sr1_cl2
sr1_cl3 = sd(c(cluster1_3$X1, cluster1_3$X2, cluster1_3$X3, cluster1_3$X4))  ;sr1_cl3

#Simpangan Baku dalam Cluster
SW1 <- (sr1_cl1+sr1_cl2+sr1_cl3)/3
SW1

#Simpangan Baku antar Cluster
xbar1_cl1 <- mean(c(cluster1_1$X1, cluster1_1$X2, cluster1_1$X3, cluster1_1$X4))
xbar1_cl2 <- mean(c(cluster1_2$X1, cluster1_2$X2, cluster1_2$X3, cluster1_2$X4))
xbar1_cl3 <- mean(c(cluster1_3$X1, cluster1_3$X2, cluster1_3$X3, cluster1_3$X4))
xbar1 <- (xbar1_cl1 + xbar1_cl2 + xbar1_cl3)/3  ;xbar1

SB1 <- sqrt(((xbar1_cl1-xbar1)^2+(xbar1_cl2-xbar1)^2+(xbar1_cl3-xbar1)^2)/(3-1))
SB1

#Rasio Simpangan Baku
rasio1 <- SB1/SW1  
rasio1

#(2) Jarak Euclidean, Average Linkage
agnes2 <- agnes(data[,2:5], metric="euclidean", method="average")
agnes2
fviz_dend(agnes2, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Euclidean - Average")
agnes.cluster2 <- cutree(agnes2,k=3)
agnes.cluster2

#Hasil Cluster
cl2 <- data.frame(data,agnes.cluster2)
cl2
cluster2_1 <- filter(cl2,agnes.cluster2 == 1) ;cluster2_1
cluster2_2 <- filter(cl2,agnes.cluster2 == 2) ;cluster2_2
cluster2_3 <- filter(cl2,agnes.cluster2 == 3) ;cluster2_3

#Simpangan Baku setiap Cluster
sr2_cl1 = sd(c(cluster2_1$X1, cluster2_1$X2, cluster2_1$X3, cluster2_1$X4))  ;sr2_cl1
sr2_cl2 = sd(c(cluster2_2$X1, cluster2_2$X2, cluster2_2$X3, cluster2_2$X4))  ;sr2_cl2
sr2_cl3 = sd(c(cluster2_3$X1, cluster2_3$X2, cluster2_3$X3, cluster2_3$X4))  ;sr2_cl3
 
#Simpangan Baku dalam Cluster
SW2 <- (sr2_cl1 + sr2_cl2 + sr2_cl3)/3
SW2

#Simpangan Baku antar Cluster
xbar2_cl1 <- mean(c(cluster2_1$X1, cluster2_1$X2, cluster2_1$X3, cluster2_1$X4))
xbar2_cl2 <- mean(c(cluster2_2$X1, cluster2_2$X2, cluster2_2$X3, cluster2_2$X4))
xbar2_cl3 <- mean(c(cluster2_3$X1, cluster2_3$X2, cluster2_3$X3, cluster2_3$X4))
xbar2 <- (xbar2_cl1 + xbar2_cl2 + xbar2_cl3)/3   ;xbar2

SB2 <- sqrt(((xbar2_cl1-xbar2)^2 + (xbar2_cl2-xbar2)^2 + (xbar2_cl3-xbar2)^2)/(3-1))
SB2

#Rasio Simpangan Baku
rasio2 <- SB2/SW2  
rasio2

#(3) Jarak Euclidean, Ward Linkage
agnes3 <- agnes(data[,2:5], metric="euclidean", method="ward")
agnes3
fviz_dend(agnes3, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Euclidean - Ward")
agnes.cluster3 <- cutree(agnes3,k=3)
agnes.cluster3

#Hasil Cluster
cl3 <- data.frame(data,agnes.cluster3)
cl3
cluster3_1 <- filter(cl3,agnes.cluster3 == 1) ;cluster3_1
cluster3_2 <- filter(cl3,agnes.cluster3 == 2) ;cluster3_2
cluster3_3 <- filter(cl3,agnes.cluster3 == 3) ;cluster3_3

#Simpangan Baku setiap Cluster
sr3_cl1 = sd(c(cluster3_1$X1, cluster3_1$X2, cluster3_1$X3, cluster3_1$X4))  ;sr3_cl1
sr3_cl2 = sd(c(cluster3_2$X1, cluster3_2$X2, cluster3_2$X3, cluster3_2$X4))  ;sr3_cl2
sr3_cl3 = sd(c(cluster3_3$X1, cluster3_3$X2, cluster3_3$X3, cluster3_3$X4))  ;sr3_cl3

#Simpangan Baku dalam Cluster
SW3 <- (sr3_cl1 + sr3_cl2 + sr3_cl3)/3
SW3

#Simpangan Baku antar Cluster
xbar3_cl1 <- mean(c(cluster3_1$X1, cluster3_1$X2, cluster3_1$X3, cluster3_1$X4))
xbar3_cl2 <- mean(c(cluster3_2$X1, cluster3_2$X2, cluster3_2$X3, cluster3_2$X4))
xbar3_cl3 <- mean(c(cluster3_3$X1, cluster3_3$X2, cluster3_3$X3, cluster3_3$X4))
xbar3 <- (xbar3_cl1 + xbar3_cl2 + xbar3_cl3)/3

SB3 <- sqrt(((xbar3_cl1-xbar3)^2 + (xbar3_cl2-xbar3)^2 + (xbar3_cl3-xbar3)^2)/(3-1))
SB3

#Rasio Simpangan Baku
rasio3 <- SB3/SW3  
rasio3

#(4) Jarak Manhattan, Complete Linkage
agnes4 <- agnes(data[,2:5], metric="manhattan", method="complete")
agnes4
fviz_dend(agnes4, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Manhattan - Complete")
agnes.cluster4 <- cutree(agnes4,k=3)
agnes.cluster4

#Hasil Cluster
cl4 <- data.frame(data,agnes.cluster4)
cl4
cluster4_1 <- filter(cl4,agnes.cluster4 == 1) ;cluster4_1
cluster4_2 <- filter(cl4,agnes.cluster4 == 2) ;cluster4_2
cluster4_3 <- filter(cl4,agnes.cluster4 == 3) ;cluster4_3

#Simpangan Baku setiap Cluster
sr4_cl1 = sd(c(cluster4_1$X1, cluster4_1$X2, cluster4_1$X3, cluster4_1$X4))  ;sr4_cl1
sr4_cl2 = sd(c(cluster4_2$X1, cluster4_2$X2, cluster4_2$X3, cluster4_2$X4))  ;sr4_cl2
sr4_cl3 = sd(c(cluster4_3$X1, cluster4_3$X2, cluster4_3$X3, cluster4_3$X4))  ;sr4_cl3

#Simpangan Baku dalam Cluster
SW4 <- (sr4_cl1 + sr4_cl2 + sr4_cl3)/3
SW4

#Simpangan Baku antar Cluster
xbar4_cl1 <- mean(c(cluster4_1$X1, cluster4_1$X2, cluster4_1$X3, cluster4_1$X4))
xbar4_cl2 <- mean(c(cluster4_2$X1, cluster4_2$X2, cluster4_2$X3, cluster4_2$X4))
xbar4_cl3 <- mean(c(cluster4_3$X1, cluster4_3$X2, cluster4_3$X3, cluster4_3$X4))
xbar4 <- (xbar4_cl1 + xbar4_cl2 + xbar4_cl3)/3  ;xbar4

SB4 <- sqrt(((xbar4_cl1-xbar4)^2 + (xbar4_cl2-xbar4)^2 + (xbar4_cl3-xbar4)^2)/(3-1))
SB4

#Rasio Simpangan Baku
rasio4 <- SB4/SW4  
rasio4

#(5) Jarak Manhattan, Average Linkage
agnes5 <- agnes(data[,2:5], metric="manhattan", method="average")
agnes5
fviz_dend(agnes5, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Manhattan - Average")
agnes.cluster5 <- cutree(agnes5,k=3)
agnes.cluster5

#Hasil Cluster
cl5 <- data.frame(data,agnes.cluster5)
cl5
cluster5_1 <- filter(cl5,agnes.cluster5 == 1) ;cluster5_1
cluster5_2 <- filter(cl5,agnes.cluster5 == 2) ;cluster5_2
cluster5_3 <- filter(cl5,agnes.cluster5 == 3) ;cluster5_3

#Simpangan Baku setiap Cluster
sr5_cl1 = sd(c(cluster5_1$X1, cluster5_1$X2, cluster5_1$X3, cluster5_1$X4))  ;sr5_cl1
sr5_cl2 = sd(c(cluster5_2$X1, cluster5_2$X2, cluster5_2$X3, cluster5_2$X4))  ;sr5_cl2
sr5_cl3 = sd(c(cluster5_3$X1, cluster5_3$X2, cluster5_3$X3, cluster5_3$X4))  ;sr5_cl3

#Simpangan Baku dalam Cluster
SW5 <- (sr5_cl1 + sr5_cl2 + sr5_cl3)/3
SW5

#Simpangan Baku antar Cluster
xbar5_cl1 <- mean(c(cluster5_1$X1, cluster5_1$X2, cluster5_1$X3, cluster5_1$X4))
xbar5_cl2 <- mean(c(cluster5_2$X1, cluster5_2$X2, cluster5_2$X3, cluster5_2$X4))
xbar5_cl3 <- mean(c(cluster5_3$X1, cluster5_3$X2, cluster5_3$X3, cluster5_3$X4))
xbar5 <- (xbar5_cl1 + xbar5_cl2 + xbar5_cl3)/3  ;xbar5

SB5 <- sqrt(((xbar5_cl1-xbar5)^2 + (xbar5_cl2-xbar5)^2 + (xbar5_cl3-xbar5)^2)/(3-1))
SB5

#Rasio Simpangan Baku
rasio5 <- SB5/SW5
rasio5

#(6) Jarak Manhattan, Ward Linkage
agnes6 <- agnes(data[,2:5], metric="manhattan", method="ward")
agnes6
fviz_dend(agnes6, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Manhattan - Ward")
agnes.cluster6 <- cutree(agnes6,k=3)
agnes.cluster6

#Hasil Cluster
cl6 <- data.frame(data,agnes.cluster6)
cl6
cluster6_1 <- filter(cl6,agnes.cluster6 == 1) ;cluster6_1
cluster6_2 <- filter(cl6,agnes.cluster6 == 2) ;cluster6_2
cluster6_3 <- filter(cl6,agnes.cluster6 == 3) ;cluster6_3

#Simpangan Baku setiap Cluster
sr6_cl1 = sd(c(cluster6_1$X1, cluster6_1$X2, cluster6_1$X3, cluster6_1$X4))  ;sr6_cl1
sr6_cl2 = sd(c(cluster6_2$X1, cluster6_2$X2, cluster6_2$X3, cluster6_2$X4))  ;sr6_cl2
sr6_cl3 = sd(c(cluster6_3$X1, cluster6_3$X2, cluster6_3$X3, cluster6_3$X4))  ;sr6_cl3

#Simpangan Baku dalam Cluster
SW6 <- (sr6_cl1 + sr6_cl2 + sr6_cl3)/3
SW6

#Simpangan Baku antar Cluster
xbar6_cl1 <- mean(c(cluster6_1$X1, cluster6_1$X2, cluster6_1$X3, cluster6_1$X4))
xbar6_cl2 <- mean(c(cluster6_2$X1, cluster6_2$X2, cluster6_2$X3, cluster6_2$X4))
xbar6_cl3 <- mean(c(cluster6_3$X1, cluster6_3$X2, cluster6_3$X3, cluster6_3$X4))
xbar6 <- (xbar6_cl1 + xbar6_cl2 + xbar6_cl3)/3  ;xbar6

SB6 <- sqrt(((xbar2_cl1-xbar2)^2 + (xbar2_cl2-xbar2)^2 + (xbar2_cl3-xbar2)^2)/(3-1))
SB6

#Rasio Simpangan Baku
rasio6 <- SB6/SW6
rasio6

#(7) Jarak Mahalanobis, Complete Linkage
cov = cov(data[,2:5])
mh = D2.dist(data[,2:5], cov)
agnes7 <- agnes(mh, method="complete")
agnes7
fviz_dend(agnes7, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Mahalanobis - Complete")
agnes.cluster7 <- cutree(agnes7,k=3)
agnes.cluster7

#Hasil Cluster
cl7 <- data.frame(data,agnes.cluster7)
cl7
cluster7_1 <- filter(cl7,agnes.cluster7 == 1) ;cluster7_1
cluster7_2 <- filter(cl7,agnes.cluster7 == 2) ;cluster7_2
cluster7_3 <- filter(cl7,agnes.cluster7 == 3) ;cluster7_3

#Simpangan Baku setiap Cluster
sr7_cl1 = sd(c(cluster7_1$X1, cluster7_1$X2, cluster7_1$X3, cluster7_1$X4))  ;sr7_cl1
sr7_cl2 = sd(c(cluster7_2$X1, cluster7_2$X2, cluster7_2$X3, cluster7_2$X4))  ;sr7_cl2
sr7_cl3 = sd(c(cluster7_3$X1, cluster7_3$X2, cluster7_3$X3, cluster7_3$X4))  ;sr7_cl3

#Simpangan Baku dalam Cluster
SW7 <- (sr7_cl1 + sr7_cl2 + sr7_cl3)/3
SW7

#Simpangan Baku antar Cluster
xbar7_cl1 <- mean(c(cluster7_1$X1, cluster7_1$X2, cluster7_1$X3, cluster7_1$X4))
xbar7_cl2 <- mean(c(cluster7_2$X1, cluster7_2$X2, cluster7_2$X3, cluster7_2$X4))
xbar7_cl3 <- mean(c(cluster7_3$X1, cluster7_3$X2, cluster7_3$X3, cluster7_3$X4))
xbar7 <- (xbar7_cl1 + xbar7_cl2 + xbar7_cl3)/3  ;xbar7

SB7 <- sqrt(((xbar7_cl1-xbar7)^2 + (xbar7_cl2-xbar7)^2 + (xbar7_cl3-xbar7)^2)/(3-1))
SB7

#Rasio Simpangan Baku
rasio7 <- SB7/SW7
rasio7

#(8) Jarak Mahalanobis, Average Linkage
cov = cov(data[,2:5])
mh = D2.dist(data[,2:5], cov)
agnes8 <- agnes(mh, method="average")
agnes8
fviz_dend(agnes8, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Mahalanobis - Average")
agnes.cluster8 <- cutree(agnes8,k=3)
agnes.cluster8

#Hasil Cluster
cl8 <- data.frame(data,agnes.cluster8)
cl8
cluster8_1 <- filter(cl8,agnes.cluster8 == 1) ;cluster8_1
cluster8_2 <- filter(cl8,agnes.cluster8 == 2) ;cluster8_2
cluster8_3 <- filter(cl8,agnes.cluster8 == 3) ;cluster8_3

#Simpangan Baku setiap Cluster
sr8_cl1 = sd(c(cluster8_1$X1, cluster8_1$X2, cluster8_1$X3, cluster8_1$X4))  ;sr8_cl1
sr8_cl2 = sd(c(cluster8_2$X1, cluster8_2$X2, cluster8_2$X3, cluster8_2$X4))  ;sr8_cl2
sr8_cl3 = sd(c(cluster8_3$X1, cluster8_3$X2, cluster8_3$X3, cluster8_3$X4))  ;sr8_cl3

#Simpangan Baku dalam Cluster
SW8 <- (sr8_cl1 + sr8_cl2 + sr8_cl3)/3
SW8

#Simpangan Baku antar Cluster
xbar8_cl1 <- mean(c(cluster8_1$X1, cluster8_1$X2, cluster8_1$X3, cluster8_1$X4))
xbar8_cl2 <- mean(c(cluster8_2$X1, cluster8_2$X2, cluster8_2$X3, cluster8_2$X4))
xbar8_cl3 <- mean(c(cluster8_3$X1, cluster8_3$X2, cluster8_3$X3, cluster8_3$X4))
xbar8 <- (xbar8_cl1 + xbar8_cl2 + xbar8_cl3)/3  ;xbar8

SB8 <- sqrt(((xbar8_cl1-xbar8)^2 + (xbar8_cl2-xbar8)^2 + (xbar8_cl3-xbar8)^2)/(3-1))
SB8

#Rasio Simpangan Baku
rasio8 <- SB8/SW8
rasio8

#(9) Jarak Mahalanobis, Ward Linkage
cov = cov(data[,2:5])
mh = D2.dist(data[,2:5], cov)
agnes9 <- agnes(mh, method="ward")
agnes9
fviz_dend(agnes9, k=3,show_labels = TRUE, rect = TRUE, cex = 0.5, main = "Cluster with Mahalanobis - Ward")
agnes.cluster9 <- cutree(agnes9,k=3)
agnes.cluster9

#Hasil Cluster
cl9 <- data.frame(data,agnes.cluster9)
cl9
cluster9_1 <- filter(cl9,agnes.cluster9 == 1) ;cluster9_1
cluster9_2 <- filter(cl9,agnes.cluster9 == 2) ;cluster9_2
cluster9_3 <- filter(cl9,agnes.cluster9 == 3) ;cluster9_3

#Simpangan Baku setiap Cluster
sr9_cl1 = sd(c(cluster9_1$X1, cluster9_1$X2, cluster9_1$X3, cluster9_1$X4))  ;sr9_cl1
sr9_cl2 = sd(c(cluster9_2$X1, cluster9_2$X2, cluster9_2$X3, cluster9_2$X4))  ;sr9_cl2
sr9_cl3 = sd(c(cluster9_3$X1, cluster9_3$X2, cluster9_3$X3, cluster9_3$X4))  ;sr9_cl3

#Simpangan Baku dalam Cluster
SW9 <- (sr9_cl1 + sr9_cl2 + sr9_cl3)/3
SW9

#Simpangan Baku antar Cluster
xbar9_cl1 <- mean(c(cluster9_1$X1, cluster9_1$X2, cluster9_1$X3, cluster9_1$X4))
xbar9_cl2 <- mean(c(cluster9_2$X1, cluster9_2$X2, cluster9_2$X3, cluster9_2$X4))
xbar9_cl3 <- mean(c(cluster9_3$X1, cluster9_3$X2, cluster9_3$X3, cluster9_3$X4))
xbar9 <- (xbar9_cl1 + xbar9_cl2 + xbar9_cl3)/3  ;xbar9

SB9 <- sqrt(((xbar9_cl1-xbar9)^2 + (xbar9_cl2-xbar9)^2 + (xbar9_cl3-xbar9)^2)/(3-1))
SB9

#Rasio Simpangan Baku
rasio9 <- SB9/SW9
rasio9

#Perbandingan Rasio Simpangan Baku
Rasio <- c(rasio1, rasio2, rasio3, rasio4, rasio5, rasio6, rasio7, rasio8, rasio9)
Kombinasi <- c("Euclidean-Complete", "Euclidean-Average", "Euclidean-Ward", 
              "Manhattan-Complete", "Manhattan-Average", "Manhattan-Ward",
              "Mahalanobis-Complete", "Mahalanobis-Average", "Mahalanobis-Ward")

rasiosd <- data.frame(Kombinasi, Rasio)
rasiosd
#Rasio tertinggi dimiliki oleh kombinasi(1)

##DISKRIMINAN

#Data Cluster dengan Jarak Euclidean, Complete Linkage
diskriminan1 <- cluster1_1[,2:6]
diskriminan1
diskriminan2 <- cluster1_2[,2:6]
diskriminan2
diskriminan3 <- cluster1_3[,2:6]
diskriminan3

#(1) Analisis Diskriminan Cluster1

#Cek Outlier
hasildata1 <- mvn(diskriminan1[,1:4],multivariateOutlierMethod = "adj", 
                 showNewData = TRUE,showOutliers = TRUE,multivariatePlot = 'qq')
hasildata1
databaru1 <- hasildata1$newData
databaru1
hasil1 <- mvn(databaru1,showOutliers = TRUE, multivariatePlot = 'qq')
hasil1
d1 <- data.frame(diskriminan1)
dataakhir1 <- d1[c(1:4,6:13,15:18,20,22:23,25:27),1:5]
dataakhir1

#Uji Asumsi Homogenitas Ragam Peragam
boxM(dataakhir1[,1:4], dataakhir1$Y)

#Uji Perbedaan Rata-rata 
X_cl1 <- as.matrix(dataakhir1[,1:4])
wilk1 <- manova(cbind(dataakhir1$X1, dataakhir1$X2, dataakhir1$X3, dataakhir1$X4) ~ dataakhir1$Y)
summary(wilk1,test="Wilks")

#Korelasi dan multikolinieritas
kor1 <- cor(X_cl1)
kor1

#Kontribusi Variabel Prediktor terhadap Variabel Respon
cc1 <- candisc(wilk1)
cc1

#Model diskriminan
modellda1 <- lda(dataakhir1$Y ~., data = dataakhir1) 
modellda1

#Uji Variabel Pembeda terkuat
pembedaterkuat1 <-modellda1$scaling*sd_pooled(modellda1$scaling)
pembedaterkuat1

#Tabel Klasifikasi
prediksi1 <- predict(modellda1,dataakhir1)
conf1 <- table(actual=dataakhir1$Y, predicted=prediksi1$class)
conf1

#Uji Ketepatan Model
hitrasio1 <- sum(diag(prop.table(conf1)))
hitrasio1

#(2) Analisis Diskriminan Cluster2

#Cek Outlier
hasildata2 <- mvn(diskriminan2[,1:4],multivariateOutlierMethod = "adj",
                  showNewData = TRUE,showOutliers = TRUE,multivariatePlot = 'qq')
hasildata2
#tidak ada oulier

#Uji Asumsi Homogenitas Ragam Peragam
X_cl2 <- as.matrix(diskriminan2[,1:4])
boxM(X_cl2, diskriminan2$Y)

#Uji Perbedaan Rata-rata 
wilk2 <- manova(X_cl2 ~ diskriminan2$Y, data=diskriminan2)
summary(wilk2,test="Wilks")

#Korelasi dan multikolinieritas
kor2 <- cor(X_cl2)
kor2

#Kontribusi Variabel Prediktor terhadap Variabel Respon
cc2 <- candisc(wilk2)
cc2

#Model diskriminan
modellda2 <- lda(diskriminan2$Y ~., data = diskriminan2) 
modellda2
D2 <- 1.52*X1 + 1.34*X2 + 0.97*X3 + 0.58*X4

#Uji Variabel Pembeda terkuat
pembedaterkuat2 <-modellda2$scaling*sd_pooled(modellda2$scaling)
pembedaterkuat2

#Tabel Klasifikasi
prediksi2 <- predict(modellda2,diskriminan2)
conf2 <- table(actual=diskriminan2$Y, predicted=prediksi2$class)
conf2

#Uji Ketepatan Model
hitrasio2 <- sum(diag(prop.table(conf2)))
hitrasio2

#(3) Analisis Diskriminan Cluster3

#Cek Outlier
hasildata3 <- mvn(diskriminan3[,1:4], multivariateOutlierMethod = "adj", 
                  showNewData = TRUE,showOutliers = TRUE)
hasildata3

#Uji Asumsi Homogenitas Ragam Peragam
X_cl3 <- as.matrix(diskriminan3[,1:4])
boxM(X_cl3, diskriminan3$Y)

#Uji Perbedaan Rata-rata 
wilk3 <- manova(X_cl3 ~ diskriminan3$Y, data=diskriminan3)
summary(wilk3,test="Wilks")

#Korelasi dan multikolinieritas
kor3 <- cor(X_cl3)
kor3

#Kontribusi Variabel Prediktor terhadap Variabel Respon
cc3 <- candisc(wilk3)
cc3

#Model diskriminan
modellda3 <- lda(diskriminan3$Y ~., data = diskriminan3) 
modellda3

#Uji Variabel Pembeda terkuat
pembedaterkuat3 <-modellda3$scaling*sd_pooled(modellda3$scaling)
pembedaterkuat3

#Prediksi
prediksi3 <- predict(modellda3,diskriminan3)
conf3 <- table(actual=diskriminan3$Y, predicted=prediksi3$class)
conf3

#Uji Ketepatan Model
hitrasio3 <- sum(diag(prop.table(conf3)))
hitrasio3

#Gabungan Model Diskriminan
Diskriminan <- data.frame(modellda1$scaling,modellda2$scaling,modellda3$scaling)
names(Diskriminan) <- c("Cluster1","Cluster2","Cluster3")
Diskriminan