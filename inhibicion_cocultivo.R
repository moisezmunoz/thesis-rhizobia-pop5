#Red de inhibición de POP5 en cocultivo 

# ─────────────────────────────────────────────
# Paquetes necesarios
# ─────────────────────────────────────────────
if (!require("visNetwork")) install.packages("visNetwork")
if (!require("htmltools")) install.packages("htmltools")

library(visNetwork)
library(htmltools)

# ─────────────────────────────────────────────
# Definir nodos (bacterias en red)
# ─────────────────────────────────────────────
nodes <- data.frame(
  id = 1:5,
  label = c("POP5", "A+E", "B+E", "D+E", "F+E"),
  group = c(
    "ANTAGONISTA", 
    "AM1+CFNEI156", 
    "MIM1+CFNEI156", 
    "CH24-10+CFNEI156", 
    "C1+CFNEI156"
  ),
  color = c("#FB8072", "#BC80BD", "#CCEBC5", "#FDB462", "#FCCDE5")
)

# ─────────────────────────────────────────────
# Definir aristas (interacciones de inhibición)
# ─────────────────────────────────────────────
edges <- data.frame(
  from = rep(1, 4),  # POP5 inhibe a todos los demás
  to = 2:5,
  arrows = "to",
  color = "#D73027"
)

# ─────────────────────────────────────────────
# Visualizar red
# ─────────────────────────────────────────────
visNetwork(nodes, edges, 
           main = "Inhibición en co-cultivo", 
           submain = "Cepas rizosféricas crecidas en pares e inhibidas por POP5") %>%
  visGroups(groupname = "ANTAGONISTA", color = "#FB8072") %>%
  visGroups(groupname = "AM1+CFNEI156", color = "#BC80BD") %>%
  visGroups(groupname = "MIM1+CFNEI156", color = "#CCEBC5") %>%
  visGroups(groupname = "CH24-10+CFNEI156", color = "#FDB462") %>%
  

  