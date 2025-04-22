# 3. Análisis exploratorio de datos

# 3.1 Calcula el promedio, mediana, mínimo y máximo de edad y peso de los clientes.

mean(datos_clientes_general$edad) #36
mean(datos_clientes_general$peso) #73

median(datos_clientes_general$edad) #33
median(datos_clientes_general$peso) #71

min(datos_clientes_general$edad) #19
min(datos_clientes_general$peso) #55

max(datos_clientes_general$edad) #65
max(datos_clientes_general$peso) #95

# 3.2 Genera un resumen estadístico general del data frame.

install.packages("psych")
library(psych)
library(dplyr)

summary(datos_clientes_general)
describe(datos_clientes_general)


# Ejemplo con peso
datos_clientes_general %>%
  summarise(
    media_peso = mean(peso),
    mediana_peso = median(peso),
    desv_peso = sd(peso),
    min_peso = min(peso),
    max_peso = max(peso)
  )

# 3.3 Crea una tabla de frecuencias del objetivo de entrenamiento.

tabla_frecuencia <- table(datos_clientes_general$objetivo)

View(tabla_frecuencia)

tabla_frec_relativa <- prop.table(tabla_frecuencia)

View(tabla_frec_relativa)


# 3.4 Identifica valores faltantes en el dataset y reemplázalos con la media de la columna correspondiente.

nuevos_datos <- data.frame(
  cliente = c("Sebastián Ruiz", NA, "Paula Varela", "Andrés Paredes", "Javiera Díaz", "Felipe Mena", "Nicole Soto", "Francisco León", "Bárbara Núñez", NA,
              "Gonzalo Bravo", "Daniela Silva", NA, "Luis Carrasco", "Fernanda Meza", "Rodrigo Palma", "Claudia Herrera", "Alejandro Gil", NA, "Pablo Quintana"),
  edad = c(27, 36, NA, 50, 44, 39, 33, 30, 28, 35, 60, NA, 31, 26, 48, NA, 38, 45, 29, 41),
  peso = c(71, 75, 68, 77, 70, 69, NA, 81, 74, 79, NA, 66, 72, 83, 64, 60, 76, 88, 65, NA),
  altura = c(1.70, 1.72, 1.68, 1.75, NA, 1.65, 1.73, NA, 1.60, 1.77, 1.79, 1.74, 1.67, 1.70, 1.69, 1.62, 1.76, NA, 1.71, 1.68),
  frec_semanal = c(4, 3, 5, 6, 4, NA, 2, 3, 5, 3, 4, 6, 2, 3, 5, 4, NA, 5, 3, 2),
  objetivo = c("Acondicionamiento", "Musculación", "Perdida", "Musculación", "Acondicionamiento", "Perdida", "Musculación", "Perdida", NA, "Acondicionamiento",
               "Perdida", "Acondicionamiento", "Musculación", "Musculación", NA, "Perdida", "Acondicionamiento", "Perdida", "Musculación", "Perdida"),
  IMC = c(24.5, 25.3, 24.1, 25.1, NA, 25.3, 22.5, NA, 28.9, 25.2, 24.9, 21.8, 25.8, 28.7, 22.4, 22.9, NA, 26.0, 23.5, 25.6),
  observacion_IMC = c("Normal", "Sobrepeso", "Normal", "Sobrepeso", NA, "Sobrepeso", "Normal", NA, "Sobrepeso", "Sobrepeso",
                      "Normal", "Normal", "Sobrepeso", "Sobrepeso", "Normal", "Normal", NA, "Sobrepeso", "Normal", "Sobrepeso")
)

datos_clientes_general <- rbind(datos_clientes_general,nuevos_datos)

is.na(datos_clientes_general)

sum(is.na(datos_clientes_general))

datos_clientes_general$cliente[is.na(datos_clientes_general$cliente)] <- "No informa"
datos_clientes_general$edad[is.na(datos_clientes_general$edad)] <- 0
datos_clientes_general$peso[is.na(datos_clientes_general$peso)] <- mean(datos_clientes_general$peso,na.rm = TRUE)
datos_clientes_general$altura[is.na(datos_clientes_general$altura)] <- mean(datos_clientes_general$altura,na.rm = TRUE)
datos_clientes_general$frec_semanal[is.na(datos_clientes_general$frec_semanal)] <- mean(datos_clientes_general$frec_semanal,na.rm = TRUE)
datos_clientes_general$objetivo[is.na(datos_clientes_general$objetivo)] <- "No informa"
datos_clientes_general$IMC[is.na(datos_clientes_general$IMC)] <- mean(datos_clientes_general$IMC,na.rm = TRUE)
datos_clientes_general$observacion_IMC[is.na(datos_clientes_general$observacion_IMC)] <- "Sin datos"

sum(is.na(datos_clientes_general))

# 3.5 Usa ggplot2 para crear un histograma de pesos de los clientes.

library(ggplot2)

#Con RBase
barplot(datos_clientes_general$peso,
        names.arg = datos_clientes_general$cliente,
        main = "Distribución de pesos x cliente",
        col = "lightblue")

#Con GGPlot
ggplot(datos_clientes_general, aes(x = peso)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribución de pesos de los clientes",
       x = "Peso (kg)",
       y = "Cantidad de clientes") +
  theme_minimal()


# 3.6 Crea un boxplot de IMC segmentado por objetivo de entrenamiento.

#Con RBase
boxplot(IMC ~ objetivo,datos_clientes_general,
        main = "Distribución de IMC x objetivo",
        xlab = "Objetivo",
        ylab = "IMC")

#Con GGPlot
ggplot(datos_clientes_general,aes(x = objetivo, y = IMC)) + 
  geom_boxplot() + 
  labs(title = "Distribución de IMC x objetivo",x = "Obejtivo", y = "IMC") + 
  theme_minimal() 

# 3.7 Calcula la correlación entre peso y altura.

correlacion_peso_altura <- lm(peso ~ altura,data = datos_clientes_general)
summary(correlacion_peso_altura)

ggplot(data = datos_clientes_general) + 
  geom_point(aes(x = altura, y = peso),
             alpha = .4,
             color = "#5184b9",
             size = 4) +
  labs(x = "Altura", y = "Peso") + 
  geom_abline(aes(intercept = -85.00,
                  slope = 92.06,
                  color = "red"))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
