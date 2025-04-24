# 4. Transformación y manipulación de datos

# 4.1 Aplica mutate() para calcular la edad en meses en vez de años.

datos_clientes_general %>%
  mutate(edad_meses = edad * 12)

# 4.2 Aplica case_when() para reclasificar los clientes según la frecuencia semanal de asistencia.

datos_clientes_general$observacion_asistencia <- case_when(
  datos_clientes_general$frec_semanal <= 3 ~ "Baja asistencia",
  datos_clientes_general$frec_semanal >= 4 & datos_clientes_general$frec_semanal <= 5 ~ "Asistencia optima",
  datos_clientes_general$frec_semanal >= 6 & datos_clientes_general$frec_semanal <= 7 ~ "Alta asistencia",
  datos_clientes_general$frec_semanal > 7 ~ "Sobre asistencia"
)

View(datos_clientes_general)


# 4.3 Divide la base de datos en dos subconjuntos: clientes con IMC normal y clientes con IMC fuera de rango.

clientes_IMC_normal <- datos_clientes_general %>%
  filter(observacion_IMC == "Normal")

count(clientes_IMC_normal)

clientes_IMC_fuera_rango <- datos_clientes_general %>%
  filter(observacion_IMC %in% c("Sobrepeso","Obesidad","Sin datos"))

count(clientes_IMC_fuera_rango)

# 4.4 Aplica pivot_longer() y pivot_wider() para transformar el dataset.

datos_largos <- datos_clientes_general |>
  pivot_longer(cols = edad:altura, names_to = "datos", values_to = "Registros")

datos_anchos <- datos_largos |>
  pivot_wider(names_from = datos, values_from = Registros)

# 4.5 Realiza una normalización de la columna de peso.

sum(is.na(datos_clientes_general$peso))

datos_clientes_general$peso_norm <- with(datos_clientes_general, 
                                         (peso - min(peso, na.rm = TRUE)) / 
                                           (max(peso, na.rm = TRUE) - min(peso, na.rm = TRUE))
)

datos_clientes_general$peso_z <- scale(datos_clientes_general$peso)

# 4.6 Une dos bases de datos distintas sobre clientes y sus historiales médicos.

df_historial <- data.frame(
  ID = 1:nrow(datos_clientes_general),
  Presion = sample(c("Normal", "Alta", "Baja"), size = nrow(datos_clientes_general), replace = TRUE),
  Colesterol = sample(c("Normal", "Alto"), size = nrow(datos_clientes_general), replace = TRUE)
)

left_join(datos_clientes_general,df_historial, by = "ID") 

# 4.7 Genera una tabla resumen con group_by() y summarise().

datos_clientes_general %>%
  group_by(objetivo)%>%
  summarise(
    num_clientes = n(),
    promedio_asistencia = mean(frec_semanal),
    minimo_peso = min(peso),
    maximo_peso = max(peso)
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::