# Data Source: https://www.kaggle.com/arjunbhasin2013/ccdata

# TAREA #1: ENTENDER EL ENUNCIADO DEL PROBLEMA Y EL CASO PR�CTICO


# TAREA #2: IMPORTAR LAS LIBRER�AS Y LOS DATASETS


library(tidyverse)

# Incluye el link al archivo CSV donde has almacenado tu dataset
creditcard_df <- read_csv("Marketing_data.csv")
# CUSTID: Identificaci�n del titular de la tarjeta de cr�dito
# BALANCE: Cantidad de saldo que queda en la cuenta del cliente para hacer compras
# BALANCE_FREQUENCY: Frecuencia de la actualizaci�n del saldo, puntuaci�n entre 0 y 1 (1 = actualizado con frecuencia, 0 = no actualizado con frecuencia)
# PURCHASES: Cantidad de compras realizadas desde la cuenta
# ONEOFFPURCHASES: Importe m�ximo de compra realizado en una sola vez
# INSTALLMENTS_PURCHASES: Importe de la compra realizada en cuotas
# CASH_ADVANCE: Anticipo otorgado al usuario
# PURCHASES_FREQUENCY: frecuencia con la que se realizan las compras, puntuaci�n entre 0 y 1 (1 = compras frecuentes, 0 = compras no frecuentes)
# PURCHASES_FREQUENCY: Frecuencia de las Compras se est�n realizando, puntuaci�n entre 0 y 1 (1 = compra con frecuencia, 0 = no compra con frecuencia)
# ONEOFF_PURCHASES_FREQUENCY: Con qu� frecuencia las compras se realizan de una sola vez (1 = compra con frecuencia, 0 = no compra con frecuencia)
# PURCHASES_INSTALLMENTS_FREQUENCY: Con qu� frecuencia se realizan las compras a plazos (1 = se realizan con frecuencia, 0 = no se realizan con frecuencia)
# CASH_ADVANCE_FREQUENCY: con qu� frecuencia el gasto se paga por adelantado
# CASH_ADVANCE_TRX: n�mero de transacciones realizadas con "Efectivo por adelantado"
# PURCHASES_TRX: n�mero de transacciones de compras realizadas
# CREDIT_LIMIT: l�mite de tarjeta de cr�dito para el usuario
# PAYMENTS: N�mero de pagos realizados por el usuario
# MINIMUM_PAYMENTS: cantidad m�nima de pagos realizados por el usuario
# PRC_FULL_PAYMENT: porcentaje del pago total pagado por el usuario
# TENURE: A�os que el usuario lleva usando el servicio de tarjeta de cr�dito

creditcard_df
str(creditcard_df)

summary(creditcard_df)
# El balance medio es $1564 
# La frecuencia del balance se actualiza bastante a menudo, en promedio ~0.9
# El promedio de las compras es $1000
# El importe m�ximo de compra no recurrente es en promedio ~$600
# El promedio de la frecuencia de las compras est� cerca de 0.5
# El promedio de ONEOFF_PURCHASES_FREQUENCY, PURCHASES_INSTALLMENTS_FREQUENCY, y CASH_ADVANCE_FREQUENCY es en general bajo
# El promedio del l�mite de cr�dito es ~ 4500
# El porcentaje de pago completo es 15%
# Los clientes llevan de promedio en el servicio 11 a�os

# Vamos a investigar quien ha hecho una compra de $40761!
creditcard_df %>% filter(ONEOFF_PURCHASES == 40761.25)

max(creditcard_df$CASH_ADVANCE)

# Vamos a ver quien pago por anticipado $47137!
# Este cliente hizo un total de 123 transacciones por adelantado!!
# Nunca paga sus compras completamente con la tarjeta
creditcard_df %>% filter(CASH_ADVANCE == max(CASH_ADVANCE))


# TAREA #3: VISUALIZACI�N DEL DATASET

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

# distplot combina la funci�n matplotlib.hist con la de seaborn kdeplot()
# KDE Plot representa la Kernel Density Estimate
# KDE se utiliza para visualizar la densidad de una probabilidad de una variable continua. 
# KDE nos muestra la densidad de una probabilidad para diferentes valores de una variable continua. 

# El balance promedio es  $1500
# 'Balance_Frequency' para muchos usuarios se actualiza muy frecuentemente ~1
# Para el campo 'PURCHASES_FREQUENCY', hay dos grupos diferentes de clientes
# Para los campos 'ONEOFF_PURCHASES_FREQUENCY' y 'PURCHASES_INSTALLMENT_FREQUENCY' la gran mayor�a de usuarios no pagan todo de golpe ni a plazos
# Muy pocos clientes pagan su deuda al completo 'PRC_FULL_PAYMENT'~0
# El promedio del l�mite del cr�dito est� entorno de los $4500
# La mayor�a de clientes llevan ~11 a�os usando el servicio
creditcard_df %>% ggplot(aes(x = BALANCE)) + 
  geom_histogram(aes(y=..density..)) +
  geom_density() 

# sns.pairplot(creditcard_df)
# Hay correlaci�n entre 'PURCHASES' y ONEOFF_PURCHASES & INSTALMENT_PURCHASES 
# Se ve una tendencia entre 'PURCHASES' y 'CREDIT_LIMIT' & 'PAYMENTS'

library(corrplot)
correlations <- cor(creditcard_df)
corrplot(correlations, method = "color", tl.col = "black", tl.srt = 45, tl.cex = .5,
         addCoef.col = "black", addcolorlabel = "no", order = "AOE",
         number.cex = .5, is.corr = T)


# TAREA #4: ENTENDER LA TEOR�A DETR�S DE K-MEANS


# TAREA #5: ENCONTRAR EL N�MERO �PTIMO DE CLISTERS UTILIZANDO EL M�TODO DEL CODO
# - El m�todo del codo es un m�todo heur�stico de interpretaci�n y validaci�n de la coherencia dentro del an�lisis de clustering dise�ado para ayudar a encontrar el n�mero apropiado de clusters en un conjunto de datos.
# - Si el gr�fico de l�neas se parece a un brazo, entonces el "codo" en el brazo es el valor de k que es el mejor.
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
plot(1:20, wcss, type = 'b', main = "Encontrar el n�mero �ptimo de Clusters",
     xlab = "Clusters", ylab = "WCSS(k)")
# Con el gr�fico podemos ver que en 4 clusters es donde se forma el codo de la curva.
# Sin embargo, los valores no se reducen a una forma lineal hasta el 8� cluster. 
# Elijamos pues un n�mero de clusters igual a 8.


# TAREA #6: APLICAR EL M�TODO DE K-MEANS

kmeans <- kmeans(creditcard_df_scaled, 8)

# Para entender mejor estos valores, vamos a aplicar la transformaci�n inversa.
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


# Primer Cluster de Clientes (Transactors): Esos son los clientes que pagan la menor cantidad de cargos por intereses y tienen cuidado con su dinero, Cl�ster con el saldo m�s bajo ($ 104) y anticipo en efectivo ($ 303), Porcentaje de pago completo = 23%
# Segundo Cluster de Clientes (Revolvers) que usan tarjeta de cr�dito como pr�stamo (sector m�s lucrativo): saldo m�s alto ($ 5000) y anticipo en efectivo (~ $ 5000), baja frecuencia de compra, alta frecuencia de anticipo en efectivo (0.5), transacciones de anticipo en efectivo alto (16) y bajo porcentaje de pago (3%)
# Tercer Cluster de Clientes (VIP/Prime): l�mite de cr�dito alto $ 16K y porcentaje m�s alto de pago completo, objetivo para aumentar el l�mite de cr�dito y aumentar los h�bitos de gasto
# Cuarto Cluster de Clientes (low tenure): estos son clientes con baja antig�edad (7 a�os), saldo bajo

y_kmeans <- kmeans$cluster 

# Concatenamos las etiquetas de los clusters con el dataset riginal 
creditcard_df_cluster <- cbind(creditcard_df, y_kmeans)
head(creditcard_df_cluster)

# Visualizamos histogramas para cada cluster 


# TAREA 7: APLICAR AN�LISIS DE LAS COMPONENTES PRINCIPALES Y VISUALIZAR LOS RESULTADOS

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


# TAREA #8: ENTENDER LA TEOR�A Y LA INTUICI�N DETR�S DE LOS AUTOENCODERS


# TAREA #9: APLICAR LOS AUTOENCODERS (REDUCIR LA DIMENSI�N DE LOS DATOS CON LOS AUTOENCODERS)








