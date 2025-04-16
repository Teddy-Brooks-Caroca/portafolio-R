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