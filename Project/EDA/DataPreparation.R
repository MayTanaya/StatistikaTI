install.packages("e1071")

library(dplyr)
library(ggplot2)
library(e1071)
library(corrplot)

# 2. Baca dataset
data <- read.csv(file.choose(""), header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))


# 3. Fungsi convert simbol -> numeric
to_numeric_clean <- function(x) {
  x <- as.character(x)
  x <- gsub("%", "", x)
  x <- gsub("\\$", "", x)
  x <- gsub(",", "", x)
  as.numeric(x)
}

cols_to_convert <- c("Density..P.Km2.", "Land.Area.Km2.", "Armed.Forces.size",
                     "Co2.Emissions", "CPI", "CPI.Change....", 
                     "Forested.Area....", "Gasoline.Price", "GDP",
                     "Gross.primary.education.enrollment....", 
                     "Gross.tertiary.education.enrollment....",
                     "Minimum.wage", "Out.of.pocket.health.expenditure",
                     "Population", "Population..Labor.force.participation....",
                     "Tax.revenue....", "Total.tax.rate", "Unemployment.rate",
                     "Urban_population")

data[cols_to_convert] <- lapply(data[cols_to_convert], to_numeric_clean)

# 4. Fungsi imputasi
impute_value <- function(x) {
  if (is.numeric(x)) {
    sk <- skewness(x, na.rm = TRUE)
    if (abs(sk) < 0.5) x[is.na(x)] <- mean(x, na.rm = TRUE)
    else x[is.na(x)] <- median(x, na.rm = TRUE)
  } else {
    mode_val <- names(which.max(table(x)))
    x[is.na(x)] <- mode_val
  }
  return(x)
}

# Terapkan imputasi ke seluruh dataset
data_clean <- data %>% mutate(across(everything(), impute_value))


# 5. Hitung GDP per capita + kategori income
data_clean <- data_clean %>%
  mutate(
    Income_cat = cut(GDP / Population,
                     breaks = c(-Inf, 1145, 4515, 14005, Inf),
                     labels = c("Low Income", "Lower-middle Income", "Upper-middle Income", "High Income"))
  )


# 6. Ambil kolom asli yang dibutuhkan untuk analisis
cols_needed <- c("Life.expectancy", "Physicians.per.thousand", "Birth.Rate",
                 "Fertility.Rate", "GDP", "Co2.Emissions", "Gross.tertiary.education.enrollment....", "Income_cat")

# 7. Visualisasi distribusi 
num_cols <- cols_needed[sapply(data_clean[cols_needed], is.numeric)]

for (col in num_cols) {
  print(
    ggplot(data_clean, aes(x = .data[[col]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      geom_density(color = "red", linewidth = 1) +
      ggtitle(paste("Distribusi", col))
  )
}

View(data_clean)

write.csv(data_clean, "D:/data_clean4.csv", row.names = FALSE)
colSums(is.na(data_clean))

