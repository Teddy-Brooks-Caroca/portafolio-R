# 6. Modelos estadísticos y machine learning básico

# 6.1 Ajusta un modelo de regresión lineal para predecir el peso basado en la altura.

predecir_peso_por_altura <- lm(peso ~ altura,data = datos_clientes_general)

summary(predecir_peso_por_altura)

'''
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -85.80      28.52  -3.009  0.00464 ** 
altura         92.51      16.61   5.571 2.21e-06 ***

Peso = (Intercept + altura) * Altura(cm)

Una persona que mide 1.83 pesaría aprox.83.49 kls
'''

# 6.2 Ajusta un modelo de regresión logística para clasificar a los clientes en “Saludable” o “No saludable” según su IMC.

tabla_completa_clientes <- left_join(datos_clientes_general,df_historial, by = "ID")

tabla_completa_clientes$estado_salud <- ifelse(
  tabla_completa_clientes$IMC >= 18.5 & tabla_completa_clientes$IMC <= 24.9 &
    tabla_completa_clientes$Presion == "Normal" &
    tabla_completa_clientes$Colesterol == "Normal",
  "Saludable",
  "No saludable"
)

tabla_completa_clientes$estado_salud <- factor(tabla_completa_clientes$estado_salud)

tabla_completa_clientes$estado_salud <- relevel(tabla_completa_clientes$estado_salud, ref = "Saludable")


modelo_salud <- glm(estado_salud ~ IMC, data = tabla_completa_clientes,family = binomial)

summary(modelo_salud)

predicciones_modelo_salud <- predict(modelo_salud, type = "response")

predicciones_modelo_salud

'''
Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept) -17.6560     8.6737  -2.036   0.0418 *
IMC           0.8499     0.3816   2.227   0.0259 *
'''

# 6.3 Divide la base de datos en conjunto de entrenamiento y prueba.

muestra <- floor(0.8 * nrow(datos_clientes_general))
seleccion <- sample(1:nrow(datos_clientes_general), size = muestra)

train <- datos_clientes_general[seleccion, ]
test <- datos_clientes_general[-seleccion, ]

# 6.4 Evalúa la precisión del modelo de regresión logística con una matriz de confusión.

clase_predicha <- ifelse(predicciones_modelo_salud > 0.5, "No saludable", "Saludable")
clase_predicha <- factor(clase_predicha, levels = c("Saludable", "No saludable"))

table(Real = tabla_completa_clientes$estado_salud, Predicho = clase_predicha)

# 6.5 Aplica kmeans() para agrupar clientes en clusters según peso e IMC.

kmeans_peso_IMC <- datos_clientes_general %>%
  select(peso,
         IMC)

kmeans_peso_IMC$peso<- scale(datos_clientes_general$peso)
kmeans_peso_IMC$IMC <- scale(datos_clientes_general$IMC)

k = 3

resultado_kmeans <- kmeans(kmeans_peso_IMC, centers =  k)

datos_clientes_general$cluster_kmeans <- resultado_kmeans$cluster

datos_clientes_general <- datos_clientes_general %>%
  mutate(cluster_kmeans = case_when(
    cluster_kmeans == 1 ~ "Bajo IMC",
    cluster_kmeans == 2 ~ "IMC Medio",
    cluster_kmeans == 3 ~ "Alto IMC"
  ))

ggplot(datos_clientes_general, aes(x = IMC, y = peso, color = as.factor(cluster_kmeans))) +
  geom_point(size = 3) +
  labs(title = "Clusters de clientes según IMC y peso",
       x = "IMC", y = "Peso",
       color = "Cluster") +
  theme_minimal()

# 6.6 Realiza una validación cruzada del modelo de regresión.

#Validar la regresión lineal del 6.1

library(caret)


control <- trainControl(method = "cv", number = 5)

modelo_cv <- train(
  peso ~ altura,
  data = datos_clientes_general,
  method = "lm",
  trControl = control
)

print(modelo_cv)

#Validar la regresión logística del 6.2

modelo_cv_log <- train(
  estado_salud ~ IMC + edad,
  data = tabla_completa_clientes, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)
)

print(modelo_cv_log)

# 6.7 Usa randomForest para predecir la asistencia semanal de los clientes.

modelo_asistencia_rf <- randomForest(frec_semanal ~ ., data = datos_clientes_general)

importance(modelo_asistencia_rf)

varImpPlot(modelo_asistencia_rf)

'''
Call:
 randomForest(formula = frec_semanal ~ ., data = datos_clientes_general) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 4

          Mean of squared residuals: 0.7220527
                    % Var explained: 55.97
'''

#:::::::::::::::::::::::::::::::: FIN PORTAFOLIO :::::::::::::::::::::::::::::::::::::::::::::::
