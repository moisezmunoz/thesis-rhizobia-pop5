#Grafica de las áreas de los halos de inhibición 

# ─────────────────────────────────────────────
# Instalar y cargar paquetes necesarios
# ─────────────────────────────────────────────
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("dunn.test")) install.packages("dunn.test")

library(ggplot2)
library(dplyr)
library(dunn.test)

# ─────────────────────────────────────────────
# Cargar y preparar los datos
# ─────────────────────────────────────────────
Area <- c("105.48","75.51","92.38","75.28","66.43","70.89","44.16","46.56","45.32",
          "66.68","54.68","67.96","76.96","101.08","89.77", "117.04","100.24","94.04",
          "129.7","167.59","169.77","185.06","196.11","198.32","211.08","226.84","237.84",
          "156.16","195.96","194.62","216.32","223.06","240.04","277.68","271.83","239.16")

Cepas <- c(rep("CH2410", 9), rep("MIM1", 9), rep("CFNEI156", 9), rep("C1", 9))

datos <- data.frame(Cepas, Area)
datos$Area <- as.numeric(as.character(datos$Area))

# Verificar si hubo errores en conversión
if (any(is.na(datos$Area))) {
  print("Existen valores NA en la columna 'Area' después de la conversión.")
}

# ─────────────────────────────────────────────
# Pruebas estadísticas
# ─────────────────────────────────────────────

# Shapiro-Wilk para normalidad
shapiro.test(datos$Area)

# Kruskal-Wallis para comparar medianas entre grupos
kruskal <- kruskal.test(Area ~ Cepas, data = datos)
kruskal

# ─────────────────────────────────────────────
# Calcular media, desviación estándar y error estándar por cepa
# ─────────────────────────────────────────────
resumen <- datos %>%
  group_by(Cepas) %>%
  summarise(
    media = mean(Area, na.rm = TRUE),
    desviacion_estandar = sd(Area, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(error_estandar = desviacion_estandar / sqrt(n))

print(resumen)

# ─────────────────────────────────────────────
# Gráfica boxplot con puntos y colores por cepa
# ─────────────────────────────────────────────

grafico <- ggplot(datos, aes(x = Cepas, y = Area, fill = Cepas)) + 
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = 0.2), 
              color = "black", alpha = 0.6, size = 2) + 
  labs(
    title = "Comparación de las áreas de halos de inhibición entre las cepas rizobiales reguladas por POP5", 
    x = "Cepa", 
    y = "Área (mm²)"
  ) + 
  theme_minimal() +
  scale_fill_manual(values = c(
    "CH2410" = "#FF7256", 
    "MIM1" = "dodgerblue1",
    "CFNEI156" = "darkolivegreen4", 
    "C1" = "mediumpurple2"
  )) +
  theme(
    plot.title = element_text(family = "Times", face = "bold", size = 12, hjust = 0.5),
    axis.title.x = element_text(family = "Times", size = 12, face = "italic"),
    axis.title.y = element_text(family = "Times", size = 12, face = "italic"),
    axis.text.x = element_text(family = "Times", size = 10),
    axis.text.y = element_text(family = "Times", size = 12),
    legend.title = element_text(family = "Times", face = "bold", size = 12), 
    legend.text = element_text(family = "Times", size = 12), 
    strip.text = element_text(family = "Times", face = "bold", size = 14)
  )

# Mostrar gráfico
print(grafico)

# Guardar gráfico como PNG
ggsave("grafico.png", plot = grafico, width = 8, height = 6, dpi = 300)

# ─────────────────────────────────────────────
# Prueba post-hoc con Dunn y corrección Bonferroni
# ─────────────────────────────────────────────
dunn <- dunn.test(datos$Area, datos$Cepas, method = "bonferroni")
















