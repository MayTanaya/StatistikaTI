# MENAMBAHKAN VARIABEL KATEGORIKAL #
library(dplyr)
library(ggplot2)
library(reshape2)
library(e1071)

# 1. Baca data
data <- read.csv2(file.choose(), header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))

# 1. Tambahkan kolom kategori region berdasarkan provinsi
data_imputed$Region <- case_when(
  data_imputed$Provinsi %in% c("Aceh","Sumatera Utara","Sumatera Barat","Riau","Jambi",
                               "Sumatera Selatan","Bengkulu","Lampung","Kepulauan Riau","Kepulauan Bangka Belitung") ~ "Sumatera",
  data_imputed$Provinsi %in% c("Banten","DKI Jakarta","Jawa Barat","Jawa Tengah","DI Yogyakarta","Jawa Timur") ~ "Jawa",
  data_imputed$Provinsi %in% c("Kalimantan Barat","Kalimantan Tengah","Kalimantan Selatan","Kalimantan Timur","Kalimantan Utara") ~ "Kalimantan",
  data_imputed$Provinsi %in% c("Sulawesi Utara","Sulawesi Tengah","Sulawesi Selatan","Sulawesi Tenggara","Sulawesi Barat","Gorontalo") ~ "Sulawesi",
  data_imputed$Provinsi %in% c("Bali","Nusa Tenggara Barat","Nusa Tenggara Timur") ~ "Bali-Nusa Tenggara",
  data_imputed$Provinsi %in% c("Maluku","Maluku Utara") ~ "Maluku",
  data_imputed$Provinsi %in% c("Papua","Papua Barat") ~ "Papua",
  TRUE ~ "Lainnya"
)

# 2. Jadikan sebagai faktor (kategori)
data_imputed$Region <- as.factor(data_imputed$Region)

# Cek hasil
table(data_imputed$Region)

data_imputed$Total_Kasus <- rowSums(
  data_imputed[, 2:11],  # ambil semua kolom penyakit
  na.rm = TRUE
)

# 3. Buat fungsi kategorisasi berbasis kuartil
kategori_kuartil <- function(x) {
  q1 <- quantile(x, 0.33, na.rm = TRUE)
  q2 <- quantile(x, 0.66, na.rm = TRUE)
  cut(
    x,
    breaks = c(-Inf, q1, q2, Inf),
    labels = c("Rendah", "Sedang", "Tinggi")
  )
}

# 4. Kategorikan TOTAL KASUS (semua penyakit)
data_imputed$Kategori_TotalKasus <- kategori_kuartil(data_imputed$Total_Kasus)

# 5. Pilih 2 penyakit utama untuk dikategorikan
data_imputed$Kategori_DBD <- kategori_kuartil(data_imputed$Jumlah.Kasus.Penyakit...Demam.Berdarah.Dengue..DBD.)
data_imputed$Kategori_Diare <- kategori_kuartil(data_imputed$Jumlah.Kasus.Penyakit...Diare)

# Cek hasil akhir
View(data_imputed)

# 6. Simpan hasil data kategorisasi
write.csv(data_imputed, "D:/Kuliah/Semester 3/Statistika TI/UTS/UTS_Data_Kategorisasi.csv", row.names = FALSE)
