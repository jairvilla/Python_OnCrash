file.choose()
setwd("~/2- Python_Projects_21/Python_OnCrash/code")

#****************************************************
# Modelo de regresion Logistica
# Data: PimaIndiansDiabetes2
# Tarea: Clasificacion de paciente con/sin diabetes
# ***************************************************

# Paquetes

install.packages("MASS")
install.packages("mlbench")

# cargamos las librerias 
library(MASS)
library(mlbench)
??MASS          # Info. paquete

# data ---
data("PimaIndiansDiabetes2")
diabetes <- PimaIndiansDiabetes2
?PimaIndiansDiabetes2
names(diabetes)

# descripcion de los datos
head(diabetes, 6)
str(diabetes)
library(skimr)
# estadisticas del data set 
summary(diabetes)
skim(diabetes)

# Omitir datos faltantes 

diabetes <- na.omit(diabetes)

dim(diabetes)
dim(data1)


# Modelo de RLogistic
model1 <- glm(diabetes ~.,family = binomial, data= diabetes ) # si queremos utilizar todas las varaibles 

summary(model1)

#   glm (y ~ X1 + x2 + xn ...)

model2 <- glm(diabetes ~ glucose + pedigree + mass,
              family = binomial,data=diabetes)

summary(model2)

# Info. Modelo
model1$fitted.values

# Evaluacion del Modelo

library(dplyr)
# selecciona 20 observaciones del dataset original
prueba <- diabetes %>%
  sample_n(20, replace = FALSE)
dim(prueba)
head(prueba, 5)
str(prueba)
# quita el outcome(diabetes)

prueba1 <- prueba %>%
  select(-diabetes)
dim(prueba1)
head(prueba1,5)

# predicion 
?predict
pred1 <- predict(model1, newdata = prueba1, type = "response")
pred2 <- ifelse(pred1 > 0.5, "pos", "neg")
pred2 <- factor(pred2)

typeof(pred2)
actual <- factor(prueba$diabetes)
actual

prediction <- data.frame(pred2,actual)
prediction

mean(pred2==actual)

# Paso 5: Evaluación (Métricas) 
library(caret)

confusionMatrix(pred2,actual, positive = "pos")
??confusionMatrix

# Paso 6: Curva Roc 
install.packages("pROC")
library(pROC)

# Curva Roc
library(pROC)
plot.roc(prueba$diabetes, pred1,
         main="Curva ROC del Model", percent=TRUE,
         ci=TRUE, of="thresholds", # compute  the threshold of prediction 
         thresholds="best", # select the (best) threshold
         print.thres="best", 
         print.auc=TRUE,ci.auc=TRUE) # a



