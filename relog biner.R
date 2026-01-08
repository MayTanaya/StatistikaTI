data=read.csv(file.choose(""),header = TRUE,stringsAsFactors = TRUE)
data

#melihat sruktur data
str(data)

# Mengubah variabel kategori menjadi faktor
# Gender sebagai faktor
data$Gender <- as.factor(data$Gender)

# Ubah Dataset jadi 0/1
# 1 = sakit → 1
# 2 = normal → 0
data$Dataset <- ifelse(data$Dataset == 1, 1, 0)
data$Dataset <- factor(data$Dataset)

str(data)


#Split data training testing
library(dplyr)
set.seed(303)
intrain <- sample(nrow(data), nrow(data)*0.7)
data_train <- data[intrain,]
data_test <- data[-intrain,]

levels(data$Dataset)

#pembentukan model
model <- glm(
  Dataset ~ Age + Total_Bilirubin + Direct_Bilirubin +
    Alkaline_Phosphotase + Alamine_Aminotransferase +
    Aspartate_Aminotransferase + Total_Protiens + Albumin +
    Albumin_and_Globulin_Ratio,
  family = binomial,
  data = data_train
)

model


#Uji serentak
library(pscl)
pR2(model)
qchisq(0.95,10)

#Uji Parsial
summary(model)

#pembentukan model ke-2
model2 <- glm(
  Dataset ~ Age + Total_Protiens + Albumin,
  family = binomial,
  data = data_train
)

summary(model2)


#Uji GoF
library(generalhoslem)
mod2 <- model.frame(model2)
logitgof(mod2$Dataset, fitted(model2))

#Prediksi
library(ggplot2)
data_test$prob_data <- predict(model2, type = "response", newdata = data_test)
ggplot(data_test, aes(x=prob_data)) +
  geom_density(lwd=0.5) +
  labs(title = "Distribusi Probabilitas Data Prediksi Penyakit Liver") +
  theme_minimal()

# Mencari threshold optimal
library(pROC)
roc_curve <- roc(data_test$Dataset, data_test$prob_data)  
optimal_threshold <- coords(roc_curve, "best", ret="threshold")
print(optimal_threshold)

#data_test$pred_data <- factor(ifelse(data_test$prob > 0.72, "2", "1"))
#data_test[1:10, c("pred_data","kerjabakti")]
cutoff <- optimal_threshold

data_test$pred_data <- factor(ifelse(data_test$prob > 0.758, "1", "0"))
data_test[1:10, c("pred_data","Dataset")]



#Evaluasi Model
library(caret)
confusionMatrix(data_test$Dataset, data_test$pred_data)

#precision = TP/(TP+FP)
precision <- 42/(42+18) # sesuaikan output
precision
#F1 Score = 2*(Sensi*preci)/(sensi+preci)
F1Score <- 2*(0.4516 *0.7)/(0.4516 + 0.7) #sesuaikan
F1Score

#Odds Ratio
exp(coef(model2))
