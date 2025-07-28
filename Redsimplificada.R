# ──────────────────────────────────────────────────────
# Red ecológica de la comunidad sintética rizobacteriana
# ──────────────────────────────────────────────────────

# Cargar e instalar paquetes si es necesario
if (!require("igraph")) install.packages("igraph")
if (!require("reshape2")) install.packages("reshape2")

library(igraph)
library(reshape2)

# ─────────────────────────────────────────────
# Matriz de interacción entre cepas
# ─────────────────────────────────────────────
rix <- matrix(c(
  -1, 0.5, 1, 0.5, 1, 1, 0.5, 1,
  0.5, -1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, -1, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, -1, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, -1, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, -1, 0.5, 0.5,
  0, 0, 0.5, 0.5, 0.5, 0.5, -1, 0,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, -1
), nrow = 8, byrow = TRUE)

nombres <- c("POP5","AM1","MIM1","SP7","CH24-10","CFNEI156","CCGE2031","C1")
colnames(rix) <- rownames(rix) <- nombres

# ─────────────────────────────────────────────
# Construcción de aristas válidas según criterios
# ─────────────────────────────────────────────

edges_list <- list()

for (i in 1:8) {
  for (j in 1:8) {
    if (i == j) next  # Omitir autointeracciones
    val1 <- rix[i, j]
    val2 <- rix[j, i]
    
    if (val1 == -1) next  # Excluir -1
    if (val1 == 0.5 && (val2 == 0 || val2 == 1)) next  # Excluir ambigua
    edges_list[[length(edges_list) + 1]] <- list(from = nombres[i], to = nombres[j], weight = val1)
  }
}

edges <- do.call(rbind, lapply(edges_list, as.data.frame))

# ─────────────────────────────────────────────
# Crear grafo dirigido con igraph
# ─────────────────────────────────────────────
g <- graph_from_data_frame(edges, directed = TRUE)

# Grosor de las aristas personalizado
E(g)$width <- 2  # Valor por defecto

# Aumentar grosor de aristas desde POP5
for (e in E(g)) {
  from <- ends(g, e)[1]
  to <- ends(g, e)[2]
  if (from == "POP5") {
    if (to == "C1") E(g)[e]$width <- 6
    else if (to == "CFNEI156") E(g)[e]$width <- 5
    else if (to == "MIM1") E(g)[e]$width <- 4
    else if (to == "CH24-10") E(g)[e]$width <- 3
  }
}

# ─────────────────────────────────────────────
# Estética: colores por especie y tipo de interacción
# ─────────────────────────────────────────────
V(g)$species <- V(g)$name
species_colors <- c(
  "POP5" = "#FF6A6A", 
  "AM1" = "hotpink1",
  "MIM1" = "#FFA500",
  "SP7" = "#FFB5C5", 
  "CH24-10" = "#7A67EE",
  "CFNEI156" = "olivedrab2", 
  "CCGE2031" = "#00BFFF", 
  "C1" = "#FFD700"
)
V(g)$color <- species_colors[V(g)$species]

# Función para asignar color según tipo de interacción
asignar_color <- function(weight) {
  if (weight == 0.5) return("#EDEDED")   # Neutra
  if (weight == 1) return("#FF3030")     # Antagónica
  if (weight == 0) return("#1C86EE")     # Negativa
  return("gray")                         # Por defecto
}
E(g)$color <- sapply(E(g)$weight, asignar_color)

# ─────────────────────────────────────────────
# Graficar red ecológica
# ─────────────────────────────────────────────
plot(g,
     vertex.label = NA,
     vertex.size = 15,
     edge.width = E(g)$width,
     main = "Interacciones ecológicas en la comunidad sintética rizosférica",
     family = "serif")  # Puedes cambiar a "sans" si serif no está disponible

# Leyendas
legend("topright", legend = c("Antagónica", "Negativa", "Neutra"),
       col = c("#FF3030", "#1C86EE", "#EDEDED"),
       lwd = 2, title = "Tipo de interacción ecológica", bty = "n", text.font = 3)

legend("bottomright", legend = names(species_colors), col = species_colors,
       pch = 21, pt.bg = species_colors,
       title = "Cepas rizosféricas", bty = "n", text.font = 3)

