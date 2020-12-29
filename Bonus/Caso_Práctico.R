

# TAREA #2: IMPORTAR LIBRERÍAS Y DATASETS

library(tidyverse)

ad_df <- read_csv("advertising.csv")
ad_df
str(ad_df)
summary(ad_df)


# TAREA #3: VISUALIZAR EL DATASET

# Veamos si falta algun dato en el dataset, esperemos que no!
library(naniar)
vis_miss(ad_df)
sum(is.na(ad_df))

ad_df %>% ggplot(aes(x = TV)) +
  geom_histogram(bins = 30, fill = "red") +
  labs(title = "TV")

ad_df %>% ggplot(aes(x = Radio)) +
  geom_histogram(bins = 30, fill = "red") +
  labs(title = "Radio")

ad_df %>% ggplot(aes(x = Newspaper)) +
  geom_histogram(bins = 30, fill = "red") +
  labs(title = "Newspaper")

ad_df %>% ggplot(aes(x = Sales)) +
  geom_histogram(bins = 30, fill = "red") +
  labs(title = "Sales")

pairs(ad_df, pch = 19)

library(corrplot)
correlations <- cor(ad_df)
corrplot(correlations, method = "color", tl.col = "black", addCoef.col = "black")


# TAREA #4: CREAR EL DATASET DE ENTRENAMIENTO Y DE TESTING Y HACER LA LIMPIEZA DE DATOS

head(ad_df, 5)

library(caTools)
set.seed(123)
split <- sample.split(ad_df$Sales, SplitRatio = .8)
training_set <- subset(ad_df, split == T)
testing_set <- subset(ad_df, split == F)


# TAREA #5: ENTRENAR EL MODELO
regressor <- lm(Sales ~., data = training_set)
regressor$coefficients[1]
regressor$coefficients[2:4]
summary(regressor)


# TAREA #6: EVALUAR EL MODELO
y_predict <- predict(regressor, newdata = testing_set[1:3])
y_predict
testing_set$Sales

plot(testing_set$Sales, y_predict, type = 'p', lwd = 6, col = "red",
     xlab = 'True Value (Ground Truth)', ylab = 'Model Predictions', 
     main = 'Model Predictions')

library(Metrics)
RMSE <- rmse(testing_set$Sales, y_predict)
MSE <- mse(testing_set$Sales, y_predict)
MAE <- mae(testing_set$Sales, y_predict)
MAPE <- mape(testing_set$Sales, y_predict) * 100 
