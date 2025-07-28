#Instalar las siguienrtes paqueteias con su respectiva librería para cargar los programas y comandos que vamos a utilizar.
install.packages("igraph")
install.packages("reshape2")

library(igraph)
library(reshape2)

#matriz de 8x8 con valores -1, 0, 0.5 y 1
matrizinteraccion <- matrix(c(
  -1, 0.5, 1, 0.5, 1, 1, 0, 1,
  0.5, -1, 0.5, 0.5, 0.5, 0.5, 0, 0.5,
  0.5, 0.5, -1, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, -1, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, -1, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, -1, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, -1, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0, -1
), nrow = 8, byrow = TRUE)

#Convertir la matriz de interaccion de 8X8 a un data frame.
edges <- melt(matrizinteraccion)
colnames(edges) <- c("row", "col", "weight")
edges <- edges[edges$weight > -1, ]  # Filtrar las interacciones nulas

#Crear un grafo bipartito
g <- graph_from_data_frame(d=edges, directed=FALSE)

#Asignar nombres a los vértices si no existen
if (is.null(V(g)$name)) {
  V(g)$name <- as.character(1:vcount(g))
}

#Definir una función para asignar colores según el peso
asignar_color <- function(weight) {
  if (weight == -1) {
    return("white")
  } else if (weight == 0.5) {
    return("gray")
  } else if (weight == 1) {
    return("#FF4040")
  } else if (weight == 0) {
    return("dodgerblue1") 
  }
}

#Aplicar la función a cada interacción en el gráfico
E(g)$color <- sapply(E(g)$weight, asignar_color)

#Aplicar e individualizar los nodos por diferenes colores para identificar la cepa.
colores_nodos <- c("red", "salmon", "green", "orange", "yellow", 
                   "purple", "pink", "blue")
V(g)$color <- colores_nodos


#Para desarrollar la red usamos la siguiente línea de comando.
plot(g, vertex.label=NA, edge.width=2, main="Interacción ecológica de la comunidad sintética rizobial") 
         
#Para agregar leyenda de las cepas rizobiales, seguimos la siguiente línea.
#legend("bottomleft", legend=c("POP5", "MIM1", "AM1", "C1", "CFNEI156", "SP7", "CH24-10", "CCGE2031"),
       #col=colores_nodos, 
       #pch=21, 
       #pt.bg=colores_nodos, 
       #pt.cex=2, 
       #cex=0.8, 
       #bty="n", 
       #ncol=1, 
      #title="Cepas rizobiales")
  
#Finalmente, agregamos la leyenda que. corresponde al tipo de interacción ecológica
#legend("topright", legend=c("Neutra", "Antagónica"), 
       #col=c("dodgerblue1", "#FF4040"), lty=1, lwd=2, 
       #title="Interacción ecológica")

