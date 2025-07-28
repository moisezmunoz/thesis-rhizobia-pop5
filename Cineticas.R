# Gráficas de densidad óptica de la cinética de 48h de las 8 cepas rizosféricas

# ─────────────────────────────────────────────
# Cargar paquetes necesarios
# ─────────────────────────────────────────────

# Instalar si no están instalados
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dunn.test")) install.packages("dunn.test")

# Cargar las librerías
library(readr)
library(dplyr)
library(ggplot2)
library(dunn.test)

# ─────────────────────────────────────────────
# Cargar y preparar los datos
# ─────────────────────────────────────────────

# Leer tabla de OD desde archivo .tsv
cinetica <- read_tsv("/Users/osvaldo/Downloads/COD.tsv")

# Revisar estructura de los datos
head(cinetica)

# Convertir valores de OD a numéricos (por si vienen con comas)
cinetica$OD <- as.numeric(gsub(",", ".", cinetica$OD))
str(cinetica$OD)

# ─────────────────────────────────────────────
# Cálculo de media de OD por cepa y tiempo
# ─────────────────────────────────────────────

ODmean <- cinetica %>%
  group_by(Tiempo, Cepa) %>%
  summarise(OD_media = mean(OD), .groups = "drop")

View(ODmean)

# ─────────────────────────────────────────────
# Gráfica de cinética por cepa
# ─────────────────────────────────────────────

grafica_cinetica <- ggplot(ODmean, aes(x = Tiempo, y = OD_media, color = Cepa)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  # Esta barra de error aplica la desviación estándar de *toda la tabla*, no por grupo
  # Si quieres SD por grupo, deberías calcularla como hiciste con la media
  geom_errorbar(aes(ymin = OD_media - sd(cinetica$OD), ymax = OD_media + sd(cinetica$OD)), width = 0.1) +
  facet_wrap(~ Cepa) +
  ggtitle("Cinéticas de crecimiento bacteriano") +
  labs(
    color = "Cepas rizosféricas",
    x = "Tiempo (h)", 
    y = "OD (600 nm)"
  ) +
  theme(
    plot.title = element_text(family = "Times", face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(family = "Times", size = 12, face = "italic"),
    axis.title.y = element_text(family = "Times", size = 12, face = "italic"),
    axis.text.x = element_text(family = "Times", size = 12),
    axis.text.y = element_text(family = "Times", size = 12),
    legend.title = element_text(family = "Times", face = "bold", size = 12), 
    legend.text = element_text(family = "Times", size = 12), 
    strip.text = element_text(family = "Times", face = "bold", size = 14)
  )

grafica_cinetica

# ─────────────────────────────────────────────
# Pruebas estadísticas
# ─────────────────────────────────────────────

# Prueba de normalidad (Shapiro-Wilk)
shapiro.test(cinetica$OD)

# Prueba de Kruskal-Wallis para comparar OD entre cepas
kruskal.test(OD ~ Cepa, data = cinetica)

# Prueba post-hoc de Dunn con corrección de Bonferroni
dunn.test(cinetica$OD, cinetica$Cepa, method = "bonferroni")

# ─────────────────────────────────────────────
# Boxplot de OD por cepa
# ─────────────────────────────────────────────

ggplot(cinetica, aes(x = Cepa, y = OD, fill = Cepa)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparación de las cinéticas de crecimiento bacteriano de cada cepa", 
    y = "OD (600 nm)", 
    x = "Cepas rizosféricas"
  ) +
  theme(
    plot.title = element_text(family = "Times", face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(family = "Times", size = 12, face = "italic"),
    axis.title.y = element_text(family = "Times", size = 12, face = "italic"),
    axis.text.x = element_text(family = "Times", size = 10),
    axis.text.y = element_text(family = "Times", size = 12),
    legend.title = element_text(family = "Times", face = "bold", size = 12), 
    legend.text = element_text(family = "Times", size = 12),
    strip.text = element_text(family = "Times", face = "bold", size = 14)
  )


