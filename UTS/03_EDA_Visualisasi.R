# VISUALISASI DATA – RUMUSAN MASALAH 1

library(ggplot2)
library(dplyr)
library(sf)

data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
View(data)

# Membersihkan dan menyederhanakan nama kolom agar lebih mudah digunakan
data_clean <- data %>%
  rename(
    TB_Paru = Jumlah.Kasus.Penyakit...TB.Paru,
    Diare = Jumlah.Kasus.Penyakit...Diare,
    DBD = Jumlah.Kasus.Penyakit...Demam.Berdarah.Dengue..DBD.,
    Pneumonia = Jumlah.Kasus.Penyakit...Pneumonia,
    Malaria = Jumlah.Kasus.Penyakit...Malaria..Suspek.
  )


# Top 10 Provinsi dengan Kasus Tertinggi per Penyakit)
buat_bar <- function(df, kolom, nama_penyakit, warna) {
  df %>%
    arrange(desc(!!sym(kolom))) %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(Provinsi, !!sym(kolom)),
               y = !!sym(kolom))) +
    geom_bar(stat = "identity", fill = warna) +
    geom_text(aes(label = !!sym(kolom)), hjust = -0.2, size = 3) +
  
  coord_flip() +
    labs(
      title = paste("Top 10 Provinsi dengan Kasus", nama_penyakit, "Tertinggi (2018)"),
      x = "Provinsi",
      y = paste("Jumlah Kasus", nama_penyakit)
    ) +
    # Mengatur batas sumbu y agar ada ruang untuk teks
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
    theme_minimal()
}

# Membuat dan menyimpan plot untuk setiap penyakit
plot_TB     <- buat_bar(data_clean, "TB_Paru", "TB Paru", "#1F618D")
plot_Diare  <- buat_bar(data_clean, "Diare", "Diare", "#117864")
plot_DBD    <- buat_bar(data_clean, "DBD", "DBD", "#B03A2E")
plot_Pneu   <- buat_bar(data_clean, "Pneumonia", "Pneumonia", "#6C3483")
plot_Mal    <- buat_bar(data_clean, "Malaria", "Malaria", "#CA6F1E")

# Menampilkan plot
print(plot_TB)
print(plot_Diare)
print(plot_DBD)
print(plot_Pneu)
print(plot_Mal)

# Pastikan library sudah dimuat
library(ggplot2)
library(dplyr)

# Asumsi 'data_clean' sudah berisi kolom Provinsi, Penyakit, dan Region

# Fungsi untuk membuat plot bar dengan facet grid berdasarkan Region
buat_bar_facet <- function(df, kolom, nama_penyakit) {
  ggplot(df, aes(x = reorder(Provinsi, !!sym(kolom)), 
                 y = !!sym(kolom), 
                 fill = Region)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = !!sym(kolom)), hjust = -0.15, size = 2.5) +
    coord_flip() +
    labs(
      title = paste("Persebaran Kasus", nama_penyakit, "di Setiap Provinsi (Dikelompokkan per Region)"),
      x = NULL, 
      y = paste("Jumlah Kasus", nama_penyakit)
    ) +
    # INI BAGIAN KUNCINYA
    facet_wrap(~ Region, scales = "free_y", ncol = 2) + 
    theme_minimal() +
    theme(
      legend.position = "none", 
      axis.text.y = element_text(size = 8) 
    )
}

# Membuat plot untuk setiap penyakit
plot_tb     <- buat_bar_facet(data_clean, "TB_Paru", "TB Paru")
plot_diare  <- buat_bar_facet(data_clean, "Diare", "Diare")
plot_dbd    <- buat_bar_facet(data_clean, "DBD", "DBD")
plot_pneumo <- buat_bar_facet(data_clean, "Pneumonia", "Pneumonia")
plot_malaria<- buat_bar_facet(data_clean, "Malaria", "Malaria")

# Tampilkan semua plot satu per satu
print(plot_tb)
print(plot_diare)
print(plot_dbd)
print(plot_pneumo)
print(plot_malaria)


#RUMUSAN MASALAH 2
# Memuat library yang dibutuhkan
library(dplyr)
library(corrplot)

# 1. Pilih hanya kolom numerik penyakit
data_penyakit <- data_clean %>%
  select(TB_Paru, Diare, DBD, Pneumonia, Malaria)

# 2. Hitung matriks korelasi
matriks_korelasi <- cor(data_penyakit)

# 3. Tampilkan matriks korelasi di console
print(matriks_korelasi)

# 1. Pilih hanya kolom numerik penyakit untuk dianalisis korelasinya
data_penyakit <- data_clean %>%
  select(TB_Paru, Diare, DBD, Pneumonia, Malaria)

# 2. Hitung matriks korelasi antar penyakit
matriks_korelasi <- cor(data_penyakit)

# 3. Buat visualisasi heatmap
corrplot(
  matriks_korelasi,
  method = "color",         # Tampilkan korelasi sebagai warna
  type = "upper",           # Tampilkan hanya bagian atas matriks agar tidak redundant
  addCoef.col = "black",    # Tambahkan nilai koefisien korelasi di atas warna
  tl.col = "black",         # Warna teks label
  tl.srt = 45,              # Rotasi label sebesar 45 derajat agar mudah dibaca
  title = "\n\nHeatmap Korelasi Antar Jenis Penyakit Menular (2018)", # Tambah judul
  mar = c(0,0,2,0)          # Atur margin untuk judul
)


#----------------------------------------------------------------------
# ANALISIS CLUSTER HIERARKI (METODE: AVERAGE LINKAGE)
#----------------------------------------------------------------------

# 1. Memuat Library yang Dibutuhkan
library(dplyr)
library(ggplot2)
library(factoextra)


data_cluster <- data %>%
  select(Provinsi, TB.Paru, Diare, DBD, Pneumonia, Malaria) %>%
  column_to_rownames(var = "Provinsi")

# 3. Penskalaan Data (Standardisasi)
data_scaled <- scale(data_cluster)

# 4. Melakukan Analisis Cluster Hierarki
# ====================================================================
# PERUBAHAN UTAMA DI SINI: method = "average"
# ====================================================================
hierarki_cluster_avg <- hclust(dist(data_scaled, method = "euclidean"), method = "average")

# 5. Membuat dan Menampilkan Dendrogram
fviz_dend(hierarki_cluster_avg,
          k = 3, # Sesuaikan jumlah cluster jika perlu
          cex = 0.7,
          main = "Dendrogram Cluster Hierarki (Average Linkage)",
          xlab = "Provinsi",
          ylab = "Jarak (Average Linkage)",
          k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"),
          rect = TRUE,
          rect_border = c("#2E9FDF", "#E7B800", "#FC4E07"),
          rect_fill = TRUE,
          horiz = FALSE)

# 6. Menyimpan Hasil Cluster ke Data Awal
cluster_groups_avg <- cutree(hierarki_cluster_avg, k = 3)

data_final_avg <- data %>%
  mutate(Cluster_Avg = cluster_groups_avg)

# 7. Menganalisis Profil Setiap Cluster
profil_cluster_avg <- data_final_avg %>%
  group_by(Cluster_Avg) %>%
  summarise(
    Rata_TB = mean(TB.Paru),
    Rata_Diare = mean(Diare),
    Rata_DBD = mean(DBD),
    Rata_Pneumonia = mean(Pneumonia),
    R

