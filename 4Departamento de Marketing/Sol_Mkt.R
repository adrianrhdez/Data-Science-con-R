# Data Source: https://www.kaggle.com/arjunbhasin2013/ccdata

# TAREA #1: ENTENDER EL ENUNCIADO DEL PROBLEMA Y EL CASO PRÁCTICO


# TAREA #2: IMPORTAR LAS LIBRERÍAS Y LOS DATASETS


library(tidyverse)

# Incluye el link al archivo CSV donde has almacenado tu dataset
creditcard_df <- read_csv("Marketing_data.csv")
# CUSTID: Identificación del titular de la tarjeta de crédito
# BALANCE: Cantidad de saldo que queda en la cuenta del cliente para hacer compras
# BALANCE_FREQUENCY: Frecuencia de la actualización del saldo, puntuación entre 0 y 1 (1 = actualizado con frecuencia, 0 = no actualizado con frecuencia)
# PURCHASES: Cantidad de compras realizadas desde la cuenta
# ONEOFFPURCHASES: Importe máximo de compra realizado en una sola vez
# INSTALLMENTS_PURCHASES: Importe de la compra realizada en cuotas
# CASH_ADVANCE: Anticipo otorgado al usuario
# PURCHASES_FREQUENCY: frecuencia con la que se realizan las compras, puntuación entre 0 y 1 (1 = compras frecuentes, 0 = compras no frecuentes)
# PURCHASES_FREQUENCY: Frecuencia de las Compras se están realizando, puntuación entre 0 y 1 (1 = compra con frecuencia, 0 = no compra con frecuencia)
# ONEOFF_PURCHASES_FREQUENCY: Con qué frecuencia las compras se realizan de una sola vez (1 = compra con frecuencia, 0 = no compra con frecuencia)
# PURCHASES_INSTALLMENTS_FREQUENCY: Con qué frecuencia se realizan las compras a plazos (1 = se realizan con frecuencia, 0 = no se realizan con frecuencia)
# CASH_ADVANCE_FREQUENCY: con qué frecuencia el gasto se paga por adelantado
# CASH_ADVANCE_TRX: número de transacciones realizadas con "Efectivo por adelantado"
# PURCHASES_TRX: número de transacciones de compras realizadas
# CREDIT_LIMIT: límite de tarjeta de crédito para el usuario
# PAYMENTS: Número de pagos realizados por el usuario
# MINIMUM_PAYMENTS: cantidad mínima de pagos realizados por el usuario
# PRC_FULL_PAYMENT: porcentaje del pago total pagado por el usuario
# TENURE: Años que el usuario lleva usando el servicio de tarjeta de crédito

creditcard_df
str(creditcard_df)

summary(creditcard_df)
# El balance medio es $1564 
# La frecuencia del balance se actualiza bastante a menudo, en promedio ~0.9
# El promedio de las compras es $1000
# El importe máximo de compra no recurrente es en promedio ~$600
# El promedio de la frecuencia de las compras está cerca de 0.5
# El promedio de ONEOFF_PURCHASES_FREQUENCY, PURCHASES_INSTALLMENTS_FREQUENCY, y CASH_ADVANCE_FREQUENCY es en general bajo
# El promedio del límite de crédito es ~ 4500
# El porcentaje de pago completo es 15%
# Los clientes llevan de promedio en el servicio 11 años

# Vamos a investigar quien ha hecho una compra de $40761!
creditcard_df %>% filter(ONEOFF_PURCHASES == 40761.25)

max(creditcard_df$CASH_ADVANCE)

# Vamos a ver quien pago por anticipado $47137!
# Este cliente hizo un total de 123 transacciones por adelantado!!
# Nunca paga sus compras completamente con la tarjeta
creditcard_df %>% filter(CASH_ADVANCE == max(CASH_ADVANCE))


# TAREA #3: VISUALIZACIÓN DEL DATASET

# Comprobemos a ver si tenemos datos faltantes, esperemos que no!
library(naniar)
vis_miss(creditcard_df)
sum(is.na(creditcard_df))
is.na(creditcard_df)

# Vamos a rellenar los datos faltantes con el promedio del campo 'MINIMUM_PAYMENT' 
library(Hmisc)
creditcard_df$MINIMUM_PAYMENTS <- impute(creditcard_df$MINIMUM_PAYMENTS, mean)

# Vamos a rellenar los datos faltantes con el promedio del campo 'CREDIT_LIMIT' 
creditcard_df$CREDIT_LIMIT <- impute(creditcard_df$CREDIT_LIMIT, mean)

vis_miss(creditcard_df)
sum(is.na(creditcard_df))

# Varifiquemos si tenemos entradas duplicadas en nuestros datos 
sum(duplicated(creditcard_df))

# Podemos deshacernos del campo Customer ID ya que no nos sirve para nada 
creditcard_df$CUST_ID <- NULL
head(creditcard_df)

# distplot combina la función matplotlib.hist con la de seaborn kdeplot()
# KDE Plot representa la Kernel Density Estimate
# KDE se utiliza para visualizar la densidad de una probabilidad de una variable continua. 
# KDE nos muestra la densidad de una probabilidad para diferentes valores de una variable continua. 

# El balance promedio es  $1500
# 'Balance_Frequency' para muchos usuarios se actualiza muy frecuentemente ~1
# Para el campo 'PURCHASES_FREQUENCY', hay dos grupos diferentes de clientes
# Para los campos 'ONEOFF_PURCHASES_FREQUENCY' y 'PURCHASES_INSTALLMENT_FREQUENCY' la gran mayoría de usuarios no pagan todo de golpe ni a plazos
# Muy pocos clientes pagan su deuda al completo 'PRC_FULL_PAYMENT'~0
# El promedio del límite del crédito está entorno de los $4500
# La mayoría de clientes llevan ~11 años usando el servicio
creditcard_df %>% ggplot(aes(x = BALANCE)) + 
  geom_histogram(aes(y=..density..)) +
  geom_density() 

# sns.pairplot(creditcard_df)
# Hay correlación entre 'PURCHASES' y ONEOFF_PURCHASES & INSTALMENT_PURCHASES 
# Se ve una tendencia entre 'PURCHASES' y 'CREDIT_LIMIT' & 'PAYMENTS'

library(corrplot)
correlations <- cor(creditcard_df)
corrplot(correlations, method = "color", tl.col = "black", tl.srt = 45, tl.cex = .5,
         addCoef.col = "black", addcolorlabel = "no", order = "AOE",
         number.cex = .5, is.corr = T)


# TAREA #4: ENTENDER LA TEORÍA DETRÁS DE K-MEANS


# TAREA #5: ENCONTRAR EL NÚMERO ÓPTIMO DE CLISTERS UTILIZANDO EL MÉTODO DEL CODO
# - El método del codo es un método heurístico de interpretación y validación de la coherencia dentro del análisis de clustering diseñado para ayudar a encontrar el número apropiado de clusters en un conjunto de datos.
# - Si el gráfico de líneas se parece a un brazo, entonces el "codo" en el brazo es el valor de k que es el mejor.
# - Source:
#     https://en.wikipedia.org/wiki/Elbow_method_(clustering)
#     https://www.geeksforgeeks.org/elbow-method-for-optimal-value-of-k-in-kmeans

# Empecemos por escalar primero el dataset
creditcard_df_scaled <- scale(creditcard_df)
creditcard_df_scaled <- as_tibble(creditcard_df_scaled)
length(creditcard_df_scaled)
creditcard_df_scaled

wcss <- vector()
for (i in 1:20){
  wcss[i] <- sum(kmeans(creditcard_df_scaled, i)$withinss)
}
plot(1:20, wcss, type = 'b', main = "Encontrar el número óptimo de Clusters",
     xlab = "Clusters", ylab = "WCSS(k)")
# Con el gráfico podemos ver que en 4 clusters es donde se forma el codo de la curva.
# Sin embargo, los valores no se reducen a una forma lineal hasta el 8º cluster. 
# Elijamos pues un número de clusters igual a 8.


# TAREA #6: APLICAR EL MÉTODO DE K-MEANS

kmeans <- kmeans(creditcard_df_scaled, 8)

# Para entender mejor estos valores, vamos a aplicar la transformación inversa.
cluster_centers <- data.frame(kmeans$centers)
cluster_centers
'
library(scales)
rescale(cluster_centers$BALANCE)

library(ggfortify)
unscale(scale(creditcard_df$BALANCE))
unscale(scale(creditcard_df))
unscale(creditcard_df_scaled)
unscale(cluster_centers, scale = cluster_centers)
unscale(scale(cluster_centers))
'


# Primer Cluster de Clientes (Transactors): Esos son los clientes que pagan la menor cantidad de cargos por intereses y tienen cuidado con su dinero, Clúster con el saldo más bajo ($ 104) y anticipo en efectivo ($ 303), Porcentaje de pago completo = 23%
# Segundo Cluster de Clientes (Revolvers) que usan tarjeta de crédito como préstamo (sector más lucrativo): saldo más alto ($ 5000) y anticipo en efectivo (~ $ 5000), baja frecuencia de compra, alta frecuencia de anticipo en efectivo (0.5), transacciones de anticipo en efectivo alto (16) y bajo porcentaje de pago (3%)
# Tercer Cluster de Clientes (VIP/Prime): límite de crédito alto $ 16K y porcentaje más alto de pago completo, objetivo para aumentar el límite de crédito y aumentar los hábitos de gasto
# Cuarto Cluster de Clientes (low tenure): estos son clientes con baja antigüedad (7 años), saldo bajo

y_kmeans <- kmeans$cluster 

# Concatenamos las etiquetas de los clusters con el dataset riginal 
creditcard_df_cluster <- cbind(creditcard_df, y_kmeans)
head(creditcard_df_cluster)

# Visualizamos histogramas para cada cluster 


# TAREA 7: APLICAR ANÁLISIS DE LAS COMPONENTES PRINCIPALES Y VISUALIZAR LOS RESULTADOS

# Obtenemos las componentes principales
library(caret)
library(e1071)
pca <- preProcess(x = creditcard_df_scaled, method = "pca", pcaComp = 2)
pca
pca_df <- predict(pca, creditcard_df_scaled)
pca_df

# Concatenamos las etiquetas de los clusters con el dataframe de las componentes principales
pca_df$cluster <- y_kmeans
head(pca_df)

aggregate(pca_df, by = list(cluster = pca_df$cluster), mean)
library(factoextra)
fviz_cluster(kmeans, data = pca_df[,1:2], geom = c("text"), labelsize = 7)


# TAREA #8: ENTENDER LA TEORÍA Y LA INTUICIÓN DETRÁS DE LOS AUTOENCODERS


# TAREA #9: APLICAR LOS AUTOENCODERS (REDUCIR LA DIMENSIÓN DE LOS DATOS CON LOS AUTOENCODERS)








