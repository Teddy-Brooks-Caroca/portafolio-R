## PORTAFOLIO MANIPULACIÓN DE DATOS CON R ## 

# 1. Creación y manipulación de bases de datos en R

# 1.1 Crea un data frame con información de 10 clientes de un gimnasio, 
# incluyendo nombre, edad, peso, altura, frecuencia semanal de asistencia y 
# objetivo de entrenamiento.

datos_clientes_gym <- data.frame(
  cliente = c('Juan Rojas','María Sepulveda','Jean Munizaga','Esther Cevallos','Laura Hulmen','Leonardo Fals','Jessika Astorga','Fresia Astorga','Carlos Concha','Pedro Ravello'),
  edad = c(25,45,32,65,23,31,42,32,52,19),
  peso = c(72,65,83,55,61,73,89,92,63,58),
  altura = c(1.72,1.63,1.90,1.56,1.65,1.68,1.77,1.82,1.76,1.75),
  frec_semanal = c(3,3,4,6,5,5,4,7,3,2),
  objetivo = c('Acondicionamiento','Musculación','Acondicionamiento','Perdida','Musculación','Perdida','Perdida','Musculación','Acondicionamiento','Acondicionamiento')
  )

str(datos_clientes_gym)

View(datos_clientes_gym)

# 1.2 Agrega una nueva columna al data frame que calcule el IMC (Índice de Masa Corporal).

datos_clientes_gym$IMC <- datos_clientes_gym$peso / (datos_clientes_gym$altura^2)

View(datos_clientes_gym)

# 1.3 Filtra los clientes cuyo IMC esté fuera del rango saludable (18.5 - 24.9).

clientes_fuera_rango <- datos_clientes_gym[datos_clientes_gym$IMC < 18.5 | datos_clientes_gym$IMC > 24.9, ]


View(clientes_fuera_rango)

# 1.4 Ordena los clientes según su peso de forma descendente.

datos_clientes_gym %>%
  arrange(desc(peso))

# 1.5 Crea una nueva variable categórica que clasifique a los clientes en 
# “Bajo peso”, “Normal”, “Sobrepeso” y “Obesidad” según su IMC.

datos_clientes_gym$observacion_IMC <- case_when(
  datos_clientes_gym$IMC < 18.5 ~ "Bajo peso",
  datos_clientes_gym$IMC >= 18.5 & datos_clientes_gym$IMC <= 24.9 ~ "Normal",
  datos_clientes_gym$IMC >= 25 & datos_clientes_gym$IMC <= 29.9 ~ "Sobrepeso",
  datos_clientes_gym$IMC >= 30 ~ "Obesidad"
)

# 1.6 Usa dplyr para seleccionar solo los clientes que asisten más de 3 veces por semana.

datos_clientes_gym %>%
  filter(frec_semanal > 3)

# 1.7 Agrupa los datos por objetivo de entrenamiento y calcula el promedio de IMC en cada grupo.

datos_clientes_gym %>%
  group_by(objetivo) %>%
  summarise(promedio_IMC = mean(IMC))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# 2. Importación y exportación de datos

# 2.1 Guarda la base de datos en un archivo CSV y vuelve a leerlo en R

write.csv(datos_clientes_gym,"datos_clientes.csv",row.names = FALSE)

read.csv("datos_clientes.csv")

# 2.2 Guarda la base de datos en formato Excel y vuelve a leerlo en R.

library(writexl)
library(readxl)

write_xlsx(datos_clientes_gym,"datos_clientes.xlsx")

read_excel("datos_clientes.xlsx")

# 2.3 Exporta solo los clientes con sobrepeso a un archivo separado.

 clientes_sobrepeso <- datos_clientes_gym %>%
   filter(observacion_IMC == "Sobrepeso")
 
 write.csv(clientes_sobrepeso,"clientes_sobrepeso.csv",row.names = FALSE)
 
 read.csv("clientes_sobrepeso.csv")

# 2.4 Importa un dataset desde una URL externa y realiza un resumen descriptivo.
 
library(readr)
 
 url_iris <- "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv"
 
 iris_data <- read_csv(url_iris)
 
 head(iris_data)
 
 summary(iris_data)
 
 str(iris_data)
 
View(iris_data)

# 2.5 Concatena datos de dos gimnasios diferentes en un solo data frame.

datos_clientes_oriente <- data.frame(
  cliente = c('Ana Beltrán','Marco Vidal','Soledad Rivas','Esteban Gutiérrez','Diana Pizarro',
              'Camilo Orellana','Javiera Méndez','Matías Cañas','Francisca Duarte','Rodrigo Palma'),
  edad = c(29, 38, 34, 41, 26, 50, 31, 24, 45, 36),
  peso = c(68, 85, 62, 90, 58, 95, 70, 64, 77, 80),
  altura = c(1.68, 1.82, 1.60, 1.88, 1.62, 1.75, 1.70, 1.69, 1.73, 1.80),
  frec_semanal = c(3, 4, 5, 2, 4, 3, 6, 5, 3, 4),
  objetivo = c('Musculación', 'Perdida', 'Acondicionamiento', 'Musculación', 'Perdida',
               'Acondicionamiento', 'Musculación', 'Acondicionamiento', 'Perdida', 'Musculación')
)

datos_clientes_oriente$IMC <- datos_clientes_oriente$peso / (datos_clientes_oriente$altura^2)

datos_clientes_oriente$observacion_IMC <- case_when(
  datos_clientes_oriente$IMC < 18.5 ~ "Bajo peso",
  datos_clientes_oriente$IMC >= 18.5 & datos_clientes_oriente$IMC <= 24.9 ~ "Normal",
  datos_clientes_oriente$IMC >= 25 & datos_clientes_oriente$IMC <= 29.9 ~ "Sobrepeso",
  datos_clientes_oriente$IMC >= 30 ~ "Obesidad"
)

datos_clientes_general <- rbind(datos_clientes_gym, datos_clientes_oriente)

View(datos_clientes_general)

# 2.6 Guarda la base de datos en formato JSON y vuelve a leerla en R.

library(jsonlite)

write_json(datos_clientes_general, "datos_clientes_general.json", pretty = TRUE)

fromJSON("datos_clientes_general.json")

# 2.7 Convierte la base de datos a formato de lista y accede a elementos específicos.

datos_clientes_lista <- as.list(datos_clientes_general)

str(datos_clientes_lista)

View(datos_clientes_lista)

# Por nombre de columna (vector)
datos_clientes_lista$cliente
datos_clientes_lista[["edad"]]

# Por índice
datos_clientes_lista$cliente[[2]]
datos_clientes_lista[[3]]

# Usar lapply o sapply para operaciones en la lista
mean(datos_clientes_lista$IMC)
sapply(datos_clientes_lista, length)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

3. Análisis exploratorio de datos
Calcula el promedio, mediana, mínimo y máximo de edad y peso de los clientes.

Genera un resumen estadístico general del data frame.

Crea una tabla de frecuencias del objetivo de entrenamiento.

Identifica valores faltantes en el dataset y reemplázalos con la media de la columna correspondiente.

Usa ggplot2 para crear un histograma de pesos de los clientes.

Crea un boxplot de IMC segmentado por objetivo de entrenamiento.

Calcula la correlación entre peso y altura.

4. Transformación y manipulación de datos
Aplica mutate() para calcular la edad en meses en vez de años.

Aplica case_when() para reclasificar los clientes según la frecuencia semanal de asistencia.

Divide la base de datos en dos subconjuntos: clientes con IMC normal y clientes con IMC fuera de rango.

Aplica pivot_longer() y pivot_wider() para transformar el dataset.

Realiza una normalización de la columna de peso.

Une dos bases de datos distintas sobre clientes y sus historiales médicos.

Genera una tabla resumen con group_by() y summarise().

5. Visualización de datos
Genera un gráfico de barras con la cantidad de clientes por objetivo de entrenamiento.

Crea un scatterplot entre peso y altura, diferenciando por color según el objetivo de entrenamiento.

Agrega etiquetas y títulos a un gráfico de dispersión de IMC vs. edad.

Usa facet_wrap() para dividir un gráfico de dispersión por categorías de IMC.

Crea un gráfico de líneas con la evolución de peso de un cliente a lo largo de 6 meses.

Personaliza los colores de un gráfico de barras con scale_fill_manual().

Exporta un gráfico en formato PNG y guárdalo en tu computadora.

6. Modelos estadísticos y machine learning básico
Ajusta un modelo de regresión lineal para predecir el peso basado en la altura.

Ajusta un modelo de regresión logística para clasificar a los clientes en “Saludable” o “No saludable” según su IMC.

Divide la base de datos en conjunto de entrenamiento y prueba.

Evalúa la precisión del modelo de regresión logística con una matriz de confusión.

Aplica kmeans() para agrupar clientes en clusters según peso e IMC.

Realiza una validación cruzada del modelo de regresión.

Usa randomForest para predecir la asistencia semanal de los clientes.