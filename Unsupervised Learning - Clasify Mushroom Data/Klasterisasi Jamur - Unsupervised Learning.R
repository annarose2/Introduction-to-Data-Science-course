Mushroom <- read.csv("DataMushrom.csv", sep= ";")
glimpse(Mushroom)
colnames(Mushroom)<-c("Classes", "Cap.Shape", "Cap.Surface", "Cap.Color",	"Bruises", "Odor",	"Gill.Attach",	"Gill.Spacing",	"Gill.Size",	"Gill.Color",	"Stalk.Shape",	"Stalk.Root",	"Stalk.SAR",	"Stalk.SBR",	"Stalk.CAR",	"Stalk.CBR", "Veil.Type",	"Veil.Color",	"Ring.Number",	"Ring.Type",	"Spore.PC",	"Population",	"Habitat")
## We make each variable as a factor
target <- as.matrix(Mushroom[,1])
dataM<-subset(Mushroom, select = -Classes)
print(dataM)
dataMushroom <- subset(dataM, select = -Veil.Type)
library(dplyr)
encoded_data <- dataMushroom
for (col in colnames(encoded_data)) {
  if (is.character(encoded_data[[col]])) {
    encoded_data[[col]] <- as.integer(as.factor(encoded_data[[col]]))
  } else if (is.logical(encoded_data[[col]])) {
    encoded_data[[col]] <- as.integer(encoded_data[[col]])
  }
}
print(encoded_data)
#Matriks
cor_matrix <- cor(encoded_data)

# Menampilkan matriks korelasi
print(cor_matrix)
# Plot matriks korelasi menggunakan heatmap
heatmap(cor_matrix, col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Matriks Korelasi")
#summary 
summary(encoded_data)
#Memeriksa jumlah klaster yang layak
library(factoextra)
#fviz_nbclust(encoded_data, kmeans, method = "wss") # Elbow plot
fviz_nbclust(encoded_data, kmeans, method = "silhouette") # Silhouette plot
#fviz_nbclust(encoded_data, kmeans, method = "gap_stat") # Gap statistic
Klaster <- kmeans(encoded_data, centers = 2, algorithm = "Hartigan-Wong")
Klaster
plot(encoded_data[, 1], encoded_data[, 2], col = Klaster$cluster, pch = 19,
     xlab = "Variabel X", ylab = "Variabel Y", main = "Hasil Klaster K-Means")

# Menambahkan label klaster pada plot
legend("topright", legend = unique(Klaster$cluster), col = unique(Klaster$cluster),
       pch = 19, title = "Klaster")
#KMode
# Mengkonversi data frame ke dalam format matrix
#encoded_matrix <- as.matrix(encoded_data)

# Menentukan jumlah klaster yang diinginkan
#num_clusters <- 2

# Melakukan clustering dengan K-Modes
#kmodes_result <- kmodes(encoded_matrix, num_clusters)

# Menampilkan hasil clustering
#print(kmodes_result)
# Menambahkan kolom Klaster ke dalam encoded_data
encoded_data$Klaster <- Klaster$cluster

# Menampilkan beberapa baris pertama data prediksi dan label asli
print(encoded_data[, c("Klaster", "Class")])

