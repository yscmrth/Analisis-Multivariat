# Impor data
library(readxl)
DataPCA <- read_excel("E:/KULIAH/SEMESTER 5/Analisis Multivariat I/PCA/Data Indikator Kemiskinan Jawa Timur.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric"))
View(DataPCA)

# Penentuan banyaknya komponen utama
## Proporsi kumulatif varians
pca <- prcomp(x = DataPCA[, c(-1)], center = TRUE, scale. = TRUE)
summary(pca)

## Nilai Eigen
Kemiskinan_cor <- cor(DataPCA[ ,-1]) #tidak mengambil kolom nama kabupaten/kota
Kemiskinan_eig <- eigen(Kemiskinan_cor)

## Scree plot
scree_data <- data.frame(eigen_value = eigen(Kemiskinan_cor)$values, PC = 1:7)
plot(x = scree_data$PC, y = scree_data$eigen_value, type = 'b',
     xlab = 'Komponen Utama ke-', ylab = 'Varians (Nilai Eigen)', main = 'Scree Plot')


# Menghitung Koefisien Komponen Utama
pca <- prcomp(x = DataPCA[, c(-1)], center = TRUE, scale. = TRUE)
pca