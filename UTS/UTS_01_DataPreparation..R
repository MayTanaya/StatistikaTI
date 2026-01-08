library(dplyr)
library(e1071)

# 1. Baca data
data <- read.csv2(file.choose(), header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))

# 2. Cek struktur
str(data)
View(data)

# 3. Ubah kolom Provinsi jadi character
data$Provinsi <- as.character(data$Provinsi)

# CEK MISSING VALUE sebelum olah data
colSums(is.na(data))
sum(is.na(data))

# 3. Ambil baris provinsi aja
data_provinsi <- as.data.frame(data[1:34, ])

# 5. Hapus kolom yang full NA
data_provinsi <- data_provinsi[, colSums(is.na(data_provinsi)) < nrow(data_provinsi)]

# CEK MISSING VALUE setelah bersihin kolom
colSums(is.na(data_provinsi))
sum(is.na(data_provinsi))

# CEK SKEWNESS sebelum imputasi
for (col in names(data_provinsi)) {
  if (is.numeric(data_provinsi[[col]])) {
    sk <- skewness(data_provinsi[[col]], na.rm = TRUE)
    cat(col, "→ Skewness:", round(sk, 2), "\n")
  }
}

# 6. Fungsi imputasi
impute_value <- function(x) {
  if (is.numeric(x)) {
    sk <- skewness(x, na.rm = TRUE)
    if (!is.na(sk) && abs(sk) < 0.5) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    } else {
      x[is.na(x)] <- median(x, na.rm = TRUE)
    }
  } else {
    mode_val <- names(which.max(table(x)))
    x[is.na(x)] <- mode_val
  }
  return(x)
}

# 7. Terapkan imputasi
data_imputed <- data_provinsi %>% mutate(across(everything(), impute_value))

# CEK MISSING VALUE setelah imputasi
colSums(is.na(data_imputed))
sum(is.na(data_imputed))

# 8. Lihat hasil akhir
sapply(data_imputed, class)
summary(data_imputed)
View(data_imputed)

# 9. Simpan hasil
write.csv(data_imputed, "D:/Kuliah/Semester 3/Statistika TI/UTS/data_uts_clean.csv", row.names = FALSE)
View(data_imputed)


