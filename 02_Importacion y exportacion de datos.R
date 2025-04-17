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