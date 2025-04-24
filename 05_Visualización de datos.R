# 5. Visualización de datos

# 5.1 Genera un gráfico de barras con la cantidad de clientes por objetivo de entrenamiento.

agrupados_por_objetivo <- datos_clientes_general %>%
  group_by(objetivo) %>%
  summarise(cantidad_clientes = n())

barplot(
  height = agrupados_por_objetivo$cantidad_clientes,
  names.arg = agrupados_por_objetivo$objetivo,
  col = "steelblue",
  main = "Cantidad de clientes por objetivo",
  xlab = "Objetivo de entrenamiento",
  ylab = "Cantidad de clientes",
  las = 1.5
)

ggplot(agrupados_por_objetivo,aes(x = objetivo, y = cantidad_clientes)) +
  geom_bar(stat = "identity", fill = "steelblue",width = 0.5) + 
  labs(title = "Cantidad de clientes por objetivo",
       x = "Objetivo de entrenamiento",
       y = "Cantidad de clientes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.2 Crea un scatterplot entre peso y altura, diferenciando por color según el objetivo de entrenamiento.

plot(datos_clientes_general$peso,datos_clientes_general$altura,
     main = "Relación peso x altura",
     xlab = "Peso (en kls.)",
     ylab = "Altura (en mts.)",
     pch = 19,
     col = "blue")

ggplot(datos_clientes_general, aes(x = peso, y = altura, color = objetivo)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre peso y altura",
    x = "Peso (kg)",
    y = "Altura (m)",
    color = "Objetivo"
  ) +
  theme_minimal()


# 5.3 Agrega etiquetas y títulos a un gráfico de dispersión de IMC vs. edad.

ggplot(datos_clientes_general, aes(x = IMC, y = edad, color = observacion_IMC)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
  labs(
    title = "Relación entre IMC y edad",
    subtitle = "Colores según la observación nutricional",
    x = "IMC",
    y = "Edad",
    color = "Observacion_IMC"
  ) +
  theme_minimal(base_size = 13)

# 5.4 Usa facet_wrap() para dividir un gráfico de dispersión por categorías de IMC.

ggplot(datos_clientes_general, aes(x = IMC, y = edad, color = observacion_IMC)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_text(aes(label = cliente), hjust = 1.2, vjust = 0.5, size = 3, show.legend = FALSE) +
  labs(
    title = "Relación entre IMC y Edad según observación nutricional",
    x = "Índice de Masa Corporal (IMC)",
    y = "Edad (años)",
    color = "Observacion IMC"
  ) +
  theme_minimal(base_size = 13) + 
  facet_wrap(~observacion_IMC)

# 5.5 Crea un gráfico de líneas con la evolución de peso de un cliente a lo largo de 6 meses.

evolucion_peso_P001 <- data.frame(
  mes = c("Enero", "Enero", "Enero",
          "Febrero", "Febrero", "Febrero",
          "Marzo", "Marzo", "Marzo",
          "Abril", "Abril", "Abril",
          "Mayo", "Mayo", "Mayo",
          "Junio", "Junio", "Junio"),
  peso = c(72.0, 71.8, 72.2,
           71.5, 71.3, 71.6,
           71.0, 70.8, 71.1,
           70.5, 70.6, 70.4,
           70.2, 70.0, 70.1,
           69.8, 69.9, 69.7)
)

evolucion_peso_P001$mes <- factor(
  evolucion_peso_P001$mes,
  levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")
)


ggplot(evolucion_peso_P001, aes(x = mes, y = peso, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2.5) +
  labs(
    title = "Evolución del peso de P001 en 6 meses",
    x = "Mes",
    y = "Peso (kg)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.6 Personaliza los colores de un gráfico de barras con scale_fill_manual().

ggplot(datos_clientes_general, aes(x = objetivo, fill = objetivo)) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "Acondicionamiento" = "skyblue",
      "Musculación" = "tomato",
      "Perdida" = "palegreen"
    )
  ) +
  labs(title = "Clientes por objetivo de entrenamiento") +
  theme_minimal()

# 5.7 Exporta un gráfico en formato PNG y guárdalo en tu computadora.

grafico_objetivo <- ggplot(datos_clientes_general, aes(x = objetivo, fill = objetivo)) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "Acondicionamiento" = "skyblue",
      "Musculación" = "tomato",
      "Perdida" = "palegreen"
    )
  ) +
  labs(title = "Clientes por objetivo de entrenamiento") +
  theme_minimal()

ggsave("grafico_objetivo.png",plot = grafico_objetivo, width = 8, height = 5, dpi = 300)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::