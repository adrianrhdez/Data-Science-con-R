
# TAREA #1: ENTENDER EL ENUNCIADO DEL PROBLEMA Y EL CASO PRÁCTICO


# TAREA #2: IMPORTAR LAS LIBRERÍAS Y LOS DATASETS

library(tidyverse)

employee_df <- read_csv("Human_Resources.csv")
employee_df
head(employee_df, 5)
tail(employee_df, 5)
str(employee_df) #35 características en total, cada una con 1470 puntos de datos
summary(employee_df)


# TAREA #3: VISUALIZACIÓN DEL DATASET

#Reemplazamos las columnas 'Attritition', 'Over18 y 'Overtime' por enteros antes de poder llevar a cabo cualquier visualización
employee_df$Attrition <- if_else(employee_df$Attrition == 'Yes', 1, 0)
employee_df$OverTime <- if_else(employee_df$OverTime == 'Yes', 1, 0)
employee_df$Over18 <- if_else(employee_df$Over18 == 'Yes', 1, 0)  
head(employee_df, 4)

#Veamos si nos faltan datos, ¡afortunadamente no es así!
library(naniar)
vis_miss(employee_df)
sum(is.na(employee_df))

"Histograma de todas las variables"
#Algunas características como 'MonthlyIncome' y 'TotalWorkingYears' tienen una distribución con una cola muy larga (long tail distribution)
#Tiene sentido que nos desagamos de 'EmployeeCount' y 'Standardhours' ya que estos campos no cambian de un empleado a otro

#Tiene sentido deshacerse de 'EmployeeCount' , 'Standardhours' y 'Over18' ya que son campos que no cambian de un empleado a otro
#Eliminemos 'EmployeeNumber' también
employee_df[,c(9, 27, 22, 10)] <- NULL

#Veamos cuantos empleados dejan la empresa!
left_df <- employee_df %>% filter(Attrition == 1) 
stayed_df <- employee_df %>% filter(Attrition == 0)

#Contamos el número de empleados que se quedaron y que se fueron
#Parece que estamos ante un conjunto de datos desequilibrado
length(employee_df$Age) #Total
length(left_df$Age) #Número de empleados que dejan la empresa 
(length(left_df$Age) / length(employee_df$Age)) * 100 #Porcentaje de empleados que dejan la empresa
length(stayed_df$Age) #Número de empleados que permanecen en la empresa
(length(stayed_df$Age) / length(employee_df$Age)) * 100 #Porcentaje de empleados que se quedan en la empresa

summary(left_df)
#Comparemos la media y el error estándar de los empleados que se quedaron y se fueron
#'age': la edad media de los empleados que se quedaron es mayor en comparación con los que se fueron
#'DailyRate': El rate diario de los empleados que se quedaron es mayor
#'DistanceFromHome': Los empleados que se quedan viven más cerca del trabajo
#'EnvironmentSatisfaction' & 'JobSatisfaction': Los empleados que se quedan están en general más satisfechos con sus trabajos
#'StockOptionLevel': Los empleados que se quedan tienen un mayor nivel de stocks options

summary(stayed_df)

library(corrplot)
correlations <- cor(employee_df[,-c(3,5,8,10,14,16)])
corrplot(correlations, method = "color", tl.col = "black", tl.srt = 45, tl.cex = .5,
         addCoef.col = "black", addcolorlabel = "no", order = "AOE",
         number.cex = .5, is.corr = T)
#Job level está altamente correlacionado con el número total de horas de trabajo
#Monthly income está altamente correlacionado con Job level

employee_df %>% ggplot(aes(Age)) +
  geom_bar(aes(fill = factor(Attrition)), position = "dodge")

employee_df %>% ggplot(aes(JobRole)) +
  geom_bar(aes(fill = factor(Attrition)), position = "dodge")

employee_df %>% ggplot(aes(MaritalStatus)) +
  geom_bar(aes(fill = factor(Attrition)), position = "dodge")

employee_df %>% ggplot(aes(JobInvolvement)) +
  geom_bar(aes(fill = factor(Attrition)), position = "dodge")

employee_df %>% ggplot(aes(JobLevel)) +
  geom_bar(aes(fill = factor(Attrition)), position = "dodge")

#Los empleados solteros tienden a irse en comparación con los casados ??????y divorciados
#Los representantes de ventas tienden a irse en comparación con cualquier otro departamento
#Los empleados menos involucrados tienden a dejar la empresa
#Los menos experimentados (nivel laboral bajo) tienden a dejar la empresa



plot(density(left_df$DistanceFromHome), ylim = c(0, .09),  xlim = c(-8, 38),
     yaxt = "n", xtaxt = "n", main = "", ylab = "", xlab = "", col = "red", )  
par(new = T)
plot(density(stayed_df$DistanceFromHome), ylab = "Density", main = "",
     xlab = "Distancia desde Casa al Trabajo", col = "blue", ylim = c(0, .09), xlim = c(-8, 38))

plot(density(left_df$YearsWithCurrManager), ylim = c(0, .20),  xlim = c(-5, 20),
     yaxt = "n", xtaxt = "n", main = "", ylab = "", xlab = "", col = "red", )  
par(new = T)
plot(density(stayed_df$YearsWithCurrManager), ylab = "Density", main = "",
     xlab = "Años con su Manager Actual", col = "blue", ylim = c(0, .20), xlim = c(-5, 20))

plot(density(left_df$TotalWorkingYears), ylim = c(0, .09),  xlim = c(-5, 50),
     yaxt = "n", xtaxt = "n", main = "", ylab = "", xlab = "", col = "red", )  
par(new = T)
plot(density(stayed_df$TotalWorkingYears), ylab = "Density", main = "",
     xlab = "Número total de años de Trabajo", col = "blue", ylim = c(0, .09), xlim = c(-5, 50))

#Veamos el Gender vs. Monthly Income
employee_df %>% ggplot(aes(x = MonthlyIncome, y = Gender)) +
  geom_boxplot() 

#Veamos el monthly income vs. job role
employee_df %>% ggplot(aes(x = MonthlyIncome, y = JobRole)) +
  geom_boxplot() 


# TAREA #4: CREAR LOS DATA SETS DE ENTRENAMIENTO Y TESTING Y LLEVAR A CABO LA LIMPIEZA DE DATOS

head(employee_df, 3)

X_cat <- employee_df %>% select(BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus)
X_cat

library(fastDummies)
X_cat <- dummy_cols(X_cat, remove_selected_columns = T)

#Notemos que hemos elminado la columna target, 'Atrittion'
numerical <- employee_df %>% 
  select(Age, DailyRate, DistanceFromHome, Education, EnvironmentSatisfaction, 
         HourlyRate, JobInvolvement, JobLevel, JobSatisfaction, MonthlyIncome, 
         MonthlyRate,	NumCompaniesWorked,	OverTime,	PercentSalaryHike, 
         PerformanceRating, RelationshipSatisfaction,	StockOptionLevel,	
         TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany, 
         YearsInCurrentRole, YearsSinceLastPromotion,	YearsWithCurrManager)#, Attrition) 
numerical
  
all_numerical <- cbind(numerical, X_cat)
all_numerical <- all_numerical %>% mutate(Attrition = employee_df$Attrition) 
all_numerical

library(metan)
all_numerical <- all_numerical %>% resca(new_min = 0, new_max = 1, keep = F) %>% view()
all_numerical[,1:8] <- NULL
all_numerical$Attrition_res <- factor(all_numerical$Attrition_res)
all_numerical

# TAREA #5: ENTENDER LA TEORÍA DETRÁS DE LA REGRESIÓN LOGÍSTICA, REDES NEURONALES ALRTIFICIALES Y CLASIFICADORES DE BOSQUES ALEATORIOS


# TAREA #7: ENTRENAR Y EVALUAR UN CLASIFICADOR UTILIZANDO LA REGRESIÓN LOGÍSTICA

library(caTools)
set.seed(123)
split <- sample.split(all_numerical$Attrition_res, SplitRatio = .7494)
training_set <- subset(all_numerical, split == T)
testing_set <- subset(all_numerical, split == F)

model <- glm(Attrition_res ~., data = training_set, family = binomial)

prob_pred <- predict(model, type = "response", newdata = testing_set[1:50])
prob_pred
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

library(caret)
cm <- confusionMatrix(factor(y_pred), testing_set$Attrition_res)
cm
cm$table


# TAREA #8: ENTRENAR Y EVALUR UN CLASIFICADOR UTILIZANDO BOSQUES ALEATORIOS

library(randomForest)
model <-  randomForest(x = training_set[,-51], y = training_set$Attrition_res)

y_pred <- predict(model, newdata = testing_set[,-51])
y_pred

cm <- confusionMatrix(factor(y_pred), testing_set$Attrition_res)
cm
cm$table


# TAREA #8: ENTRENAR Y EVALUAR UN CLASIFICADOR UTILIZANDO DEEP LEARNING
library(h2o)
h2o.init(nthreads = -1)
model <- h2o.deeplearning(y = "Attrition_res", 
                          training_frame = as.h2o(training_set),
                          activation = "Rectifier", hidden = c(500, 500),
                          epochs = 100,
                          train_samples_per_iteration = -2)


model


prob_pred <- h2o.predict(model, newdata = as.h2o(testing_set[,-51]))
prob_pred
y_pred <- (prob_pred[,1])
y_pred <- as.vector(y_pred)
y_pred

cm <- confusionMatrix(factor(y_pred), testing_set$Attrition_res)
cm
cm$table

h2o.shutdown()
Y





