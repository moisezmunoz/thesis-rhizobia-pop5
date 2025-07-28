rix <- matrix(c(
  0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  1, 0.5, 0, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0.5,
  1, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 
  1, 0.5, 0.5, 0.5, 0.5, 0, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 0.5,
  1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0
), nrow=8, byrow=TRUE)

print(rix)

install.packages("bipartite")
library('bipartite')

colnames(rix) <- c("POP5+", "AM1+", "MIM1+", "SP7+", "CH2410+", "CFNEI156+", "CCGE2031+", "C1+")
row.names(rix) <- c("POP5", "AM1", "MIM1", "SP7", "CH2410", "CFNEI156", "CCGE2031", "C1")

View(rix)

asignar_color <- function(valor) {
  if (valor == 0) {
    return("white")
  } else if (valor == 0.5) {
    return("#E0FFFF")
  } else if (valor == 1) {
    return("#FF4040")
  } else {
    return("gray")  # Por si acaso hay otros valores
  }
}

# Aplicar la función a cada elemento de la matriz para crear una matriz de colores
colores <- apply(rix, c(2, 1), asignar_color)

# Imprimir la matriz de colores para verificar
print(colores)

plotweb(rix, col.high = "#CD6839", col.low = "#FF8247", col.interaction = colores)
par(family="serif", font.main=2)
title("Interacción global de la comunidad sintética rizobial", cex.main = 1.2)
