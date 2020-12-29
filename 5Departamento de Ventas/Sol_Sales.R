

# TAREA #1: ENTENDER EL ENUNCIADO DEL PROBLEMA Y EL CASO PRÁCTICO


# TAREA #2: IMPORTAR LAS LIBRERÍAS Y LOS DATASETS

library(tidyverse)

# - TAREA #2.1: IMPORTAR EL DATASET DE VENTAS

sales_train_df <- read_csv("train.csv")
problems(sales_train_df)
sales_train_df <- read_csv("train.csv", col_types = cols(StateHoliday = 'c'))
head(sales_train_df, 5)
# Casi un millón de observaciones
# 1115 tiendas únicas 
# Notemos que las ventas es la variable objetivo (la que intentamos predecir)

# Id:  ID de transacción (combinación de la tienda y la fecha)
# Store: identificador único de la tienda
# Sales: ventas diarias, esta es la variable objetivo
# Customers: número de clientes de un día dado
# Open: Booleano para indicar si la tienda estaba abierta o cerrada (0 = cerrada, 1 = abierta)
# Promo: describe si la tienda tenía algún tipo de promoción ese día o no
# StateHoliday: indica si el día era festivo o no (a = vacaciones públicas, b = vacaciones de Pascua, c = Navidades, 0 = No era festivo)
# SchoolHoliday: indica si (Store, Date) se ve afectado por el cierre de las escuelas públicas
# Fuente original de los datos: https://www.kaggle.com/c/rossmann-store-sales/data

tail(sales_train_df, 10)

str(sales_train_df)
# 9 columnas en total 
# 8 características, cada una con 1017209 puntos de datos
# 1 variable objetivo (ventas)

summary(sales_train_df)
# Cantidad de ventas promedio por día = 5773 Euros, ventas mínimas por día = 0, ventas máximas por día = 41551
# Número medio de clientes = 633, número mínimo de clientes = 0, número máximo de clientes = 7388


# - TAREA #2.2: IMPORTAR LA INFORMACIÓN SOBRE LAS TIENDAS

store_info_df <- read_csv("store.csv")
# StoreType: categoría que indica el tipo de tienda (a, b, c, d)
# Assortment:  a = básico, b = extra, c = extedido
# CompetitionDistance (en metros): distancia a la tienda de la competencia más cercana
# CompetitionOpenSince [Mes/Año]:  fecha en que abrió la competencia
# Promo2: Promo2 es una promoción continuada y consecutiva en algunas tiendas (0 = la tienda no participa, 1 = la tienda participa)
# Promo2Since [Año/Semana]: fecha en la que la tienda empieza a participar en la Promo2
# PromoInterval: describe los intervalos consecutivos donde la Promo2 empieza, indicando los meses en los que empieza la misma. P.e. "Feb,May,Aug,Nov" significa que cada nueva ronda de promoción empieza en Febrero, Mayo, Agosto, Noviembre de cualquier año de esa tienda

head(store_info_df, 5)

# Hagamos lo mismo con los datos store_info_df
# Hay que tener en cuenta que el data frame anterior incluye las transacciones registradas por día (en millones)
# Este data frame solo incluye información sobre las 1115 tiendas exclusivas que forman parte de este estudio
str(store_info_df)

# De media, la distancia de la competencia es de 5404 metros (5,4 kms)
summary(store_info_df)


# TAREA #3: EXPLORAR EL DATASET

# - TAREA #3.1: EXPLORAR EL DATASET DE VENTAS

# Veamos si nos faltan datos, ¡esperemos que no sea así!
library(naniar)
vis_miss(sales_train_df, warn_large_data = F)
sum(is.na(sales_train_df))

# Promedio de 600 clientes por día, el máximo es 4500 (¡tenga en cuenta que no podemos ver el valor atípico en 7388!)
# Los datos se distribuyen por igual en varios días de la semana (~ 150000 observaciones x 7 días = ~ 1,1 millones de observaciones)
# Las tiendas están abiertas ~ 80% del tiempo
# Los datos se distribuyen por igual entre todas las tiendas (sin sesgo)
# La promoción # 1 se ejecutó aproximadamente el 40% del tiempo
# Ventas promedio alrededor de 5000-6000 Euros
# Las vacaciones escolares duran alrededor del 18% del tiempo

max(sales_train_df$Customers)

# ¡Veamos cuántas tiendas están abiertas y cerradas!
closed_train_df <- sales_train_df %>% filter(Open == 0)
open_train_df <- sales_train_df %>% filter(Open == 1)

# Contemos el número de tiendas que están abiertas y cerradas
length(sales_train_df$Store) #Total
length(open_train_df$Store) #Número de tiendas abiertas
length(closed_train_df$Store) #Número de tiendas cerradas
(length(closed_train_df$Store) / length(sales_train_df$Store)) * 100 #Porcentaje de tiendas cerradas

# nos quedamos solo con las tiendas abiertas y eliminamos las tiendas cerradas
sales_train_df <- sales_train_df %>% filter(Open == 1)
sales_train_df

# Eliminemos la columna open ya que ahora no tiene sentido
sales_train_df$Open <- NULL
sales_train_df

# Ventas promedio = 6955 Euros, número promedio de clientes = 762 (ha subido)
summary(sales_train_df)

# - TAREA #3.2: EXPLORAR LOS DATOS DE LA INFORMACIÓN DE LAS TIENDAS

# ¡Veamos si falta algún dato en el data frame de información de la tienda!
vis_miss(store_info_df)
sum(is.na(store_info_df))

# Echemos un vistazo a los valores faltantes en la 'CompetitionDistance'
# Solo faltan 3 filas
store_info_df %>% filter(is.na(CompetitionDistance)) 

# Echemos un vistazo a los valores faltantes en el 'CompetitionOpenSinceMonth'
# Faltan muchas filas = 354 (casi un tercio de las 1115 tiendas)
store_info_df %>% filter(is.na(CompetitionOpenSinceMonth)) 

store_info_df %>% filter(Promo2 == 0) 

# Parece que si 'promo2' es cero, 'promo2SinceWeek', 'Promo2SinceYear' y la información de 'PromoInterval' se establece en cero
# Hay 354 filas donde 'CompetitionOpenSinceYear' y 'CompetitionOpenSinceMonth' falta
# Establezcamos estos valores en ceros

store_info_df <- store_info_df %>% 
  replace_na(list(Promo2SinceWeek = 0, Promo2SinceYear = 0, 
                  PromoInterval = 0, CompetitionOpenSinceYear = 0, 
                  CompetitionOpenSinceMonth = 0)) 

vis_miss(store_info_df)
sum(is.na(store_info_df))

# Hay 3 filas con valores de 'CompetitionDistance' que faltan, llenémoslas con valores promedio de la columna 'CompetitionDistance'
library(Hmisc)
store_info_df$CompetitionDistance <- impute(store_info_df$CompetitionDistance, mean)

vis_miss(store_info_df)
sum(is.na(store_info_df))

# la mitad de las tiendas participan en la promoción 2
# la mitad de las tiendas tienen su competencia a una distancia de 0-3000 m (3 kms de distancia)

# - TAREA #3.3: EXPLOREMOS EL DATASET COMBINADO
# Combinemos ambos data frames en función de 'store'
sales_train_all_df <- sales_train_df %>% full_join(store_info_df, by = 'Store') 
sales_train_all_df <- sales_train_all_df %>% arrange(Store)

write.table(sales_train_all_df, "test.csv", quote =  FALSE, sep = ",", row.names = FALSE) 

sales_train_all_df

library(corrplot)
correlations <- cor(sales_train_all_df[,-c(3,7,9,10,11,17)])
correlations
# los clientes y la promoción se correlacionan positivamente con las ventas
# Promo2 no parece ser efectivo en absoluto

corrplot(correlations, method = "color", tl.col = "black", tl.srt = 45, tl.cex = .5,
         addCoef.col = "black", addcolorlabel = "no", order = "AOE",
         number.cex = .5, is.corr = T)
# Los clientes / Promo2 y las ventas están fuertemente correlacionados

# Separemos el año y pongámoslo en una columna separada
library(lubridate)
sales_train_all_df$Year <- year(sales_train_all_df$Date)
sales_train_all_df

sales_train_all_df$Month <- month(sales_train_all_df$Date)
sales_train_all_df$Day <- day(sales_train_all_df$Date)
sales_train_all_df

# Echemos un vistazo a las ventas promedio y la cantidad de clientes por mes.
# 'groupby' funciona muy bien al agrupar todos los datos que comparten la misma columna del mes, luego obtener la media de la columna de ventas
# Parece que las ventas y el número de clientes alcanzan su punto máximo en el período de Navidad

axis <- sales_train_all_df %>% group_by(Month) %>% 
  summarise(mean(Sales)) %>% 
  ggplot(aes(x = Month, y = `mean(Sales)`)) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "Ventas promedio por Mes", y = NULL)
axis

axis <- sales_train_all_df %>% group_by(Month) %>% 
  summarise(mean(Customers)) %>% 
  ggplot(aes(x = Month, y = `mean(Customers)`)) +
  geom_point(color = "blue", shape = 17) +
  geom_line(color = "blue") +
  labs(title = "Clientes promedio por Mes", y = NULL)
axis


# Echemos un vistazo a las ventas y a los clientes por día del mes.
# El número mínimo de clientes suele rondar el día 24 del mes.
# La mayoría de los clientes y las ventas son alrededor del 30 y el 1 del mes
axis <- sales_train_all_df %>% group_by(Day) %>% 
  summarise(mean(Sales)) %>% 
  ggplot(aes(x = Day, y = `mean(Sales)`)) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "Ventas promedio por Día", y = NULL)
axis

axis <- sales_train_all_df %>% group_by(Day) %>% 
  summarise(mean(Customers)) %>% 
  ggplot(aes(x = Day, y = `mean(Customers)`)) +
  geom_point(color = "blue", shape = 17) +
  geom_line(color = "blue") +
  labs(title = "Clientes promedio por Día", y = NULL)
axis

# Hagamos lo mismo para el día de la semana (notemos que 7 = domingo)
axis <- sales_train_all_df %>% group_by(DayOfWeek) %>% 
  summarise(mean(Sales)) %>% 
  ggplot(aes(x = DayOfWeek, y = `mean(Sales)`)) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "Ventas promedio por día de la semana", y = NULL)
axis

axis <- sales_train_all_df %>% group_by(DayOfWeek) %>% 
  summarise(mean(Customers) )%>% 
  ggplot(aes(x = DayOfWeek, y = `mean(Customers)`)) +
  geom_point(color = "blue", shape = 17) +
  geom_line(color = "blue") +
  labs(title = "Clientes promedio por día de la semana", y = NULL)
axis

ax <- sales_train_all_df %>% group_by(Date, StoreType) %>%
  summarise(mean(Sales)) %>% 
  pivot_wider(names_from = StoreType, values_from = `mean(Sales)`) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = a, colour = 'a')) +
  geom_line(aes(y = b, colour = 'b')) +
  geom_line(aes(y = c, colour = 'c')) +
  geom_line(aes(y = d, colour = 'd')) +
  scale_color_manual(values = c('blue', 'orange', 'green','red')) +
  labs(color = 'StoreType', y = NULL)
ax

sales_train_all_df %>% 
  ggplot(aes(y = Sales)) +
  geom_col(aes(x = as.character(Promo), fill = as.character(Promo)), show.legend = FALSE) +
  labs(x = 'Promo')

sales_train_all_df %>% 
  ggplot(aes(y = Customers)) +
  geom_col(aes(x = as.character(Promo), fill = as.character(Promo)), show.legend = FALSE) +
  labs(x = 'Promo')

sales_train_all_df %>% 
  ggplot(aes(y = Sales)) +
  geom_violin(aes(x = as.character(Promo), fill = as.character(Promo)), show.legend = FALSE) +
  labs(x = 'Promo')

sales_train_all_df %>% 
  ggplot(aes(y = Customers)) +
  geom_violin(aes(x = as.character(Promo), fill = as.character(Promo)), show.legend = FALSE) +
  labs(x = 'Promo')


# TAREA # 4: ENTENDER LA INTUICIÓN DETRÁS DE FACEBOOK PROPHET


# TAREA # 5: ENTRENAR AL MODELO PARTE A

library(prophet)

tienda_10 <- sales_train_all_df %>% filter(Store == 10) %>% 
  select(Date, Sales) %>% 
  rename(ds = Date, y = Sales) %>% view()

m <- prophet(tienda_10)
m

future <- make_future_dataframe(m, periods = 60)
future

forecast <- predict(m, future)
forecast
head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)


# TAREA # 6: ENTRENAR AL MODELO PARTE B

'StateHoliday: indica si el día era festivo o no (a = vacaciones públicas, b = vacaciones de Pascua holiday, c = Navidades, 0 = No era festivo)
SchoolHoliday: indica si (Store, Date) se ve afectado por el cierre de las escuelas públicas'

library(prophet)

# tienda_10 <- sales_train_all_df %>% filter(Store == 10) %>% 
#   select(Date, Sales) %>% 
#   rename(ds = Date, y = Sales) %>% view()
# 
# m <- prophet(tienda_10)
# m
# 
# future <- make_future_dataframe(m, periods = 60)
# future
# 
# forecast <- predict(m, future)
# forecast
# head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# 
# plot(m, forecast)
# prophet_plot_components(m, forecast)
# dyplot.prophet(m, forecast)

# Obtener todas las fechas relacionadas con las vacaciones escolares
school_holidays <- sales_train_all_df %>% filter(SchoolHoliday == 1) %>% 
  select(Date) %>% arrange(Date) 
length(school_holidays$Date)

school_holidays <- unique(school_holidays)
length(school_holidays$Date)

# Obtener todas las fechas correspondientes a los festivos estatales
state_holidays <- sales_train_all_df %>% filter(StateHoliday == 'a' | StateHoliday == 'b' | StateHoliday == 'c' | StateHoliday == 'd') %>% 
  select(Date) %>% arrange(Date) 
length(state_holidays$Date)

state_holidays <- unique(state_holidays)
length(state_holidays$Date)

school_holidays <- tibble(ds = school_holidays$Date,
                          holiday = 'school_holiday')
school_holidays

state_holidays <- tibble(ds = state_holidays$Date, 
                         holiday = 'state_holiday')
state_holidays

# Concatenamos las vacaciones escolares y los festivos estatales
school_state_holidays <- rbind(state_holidays, school_holidays)

# Hagamos predicciones usando días festivos para una tienda específica
tienda_6 <- sales_train_all_df %>% filter(Store == 6) %>% 
  select(Date, Sales) %>% 
  rename(ds = Date, y = Sales) %>% view()

m_2 <- prophet(tienda_6, holidays = school_state_holidays)
m_2

future_2 <- make_future_dataframe(m_2, periods = 90)
future_2

forecast_2 <- predict(m_2, future_2)
forecast_2
head(forecast_2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast_2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m_2, forecast_2)
prophet_plot_components(m_2, forecast_2)
dyplot.prophet(m_2, forecast_2)





##############################################################################

def sales_predictions(Store_ID, sales_df, periods):
  sales_df = sales_df[sales_df['Store'] == Store_ID]
sales_df = sales_df[['Date', 'Sales']].rename(columns = {'Date': 'ds', 'Sales': 'y'})
sales_df = sales_df.sort_values('ds')

model    = Prophet()
model.fit(sales_df)
future   = model.make_future_dataframe(periods = periods)
forecast = model.predict(future)
figure   = model.plot(forecast, xlabel = "Fecha", ylabel = "Ventas")
figure2  = model.plot_components(forecast)

sales_predictions(10, sales_train_all_df, 60)



def sales_predictions(Store_ID, sales_df, holidays, periods):
  sales_df = sales_df[sales_df['Store'] == Store_ID]
sales_df = sales_df[['Date', 'Sales']].rename(columns = {'Date': 'ds', 'Sales': 'y'})
sales_df = sales_df.sort_values('ds')

model    = Prophet(holidays=holidays)
model.fit(sales_df)
future   = model.make_future_dataframe(periods = periods)
forecast = model.predict(future)
figure   = model.plot(forecast, xlabel = "Fecha", ylabel = "Ventas")
figure2  = model.plot_components(forecast)

sales_predictions(6, sales_train_all_df, school_state_holidays, 90)






