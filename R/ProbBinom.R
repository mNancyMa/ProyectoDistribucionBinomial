#' Probabilidad Binomial
#'
#' Describe el número de éxitos al realizar n experimentos independientes entre sí, acerca de una variable aleatoria
#'
#' @param y indica la variable de la distribución binomial
#' @param p indica la probabilidad
#' @return Devuelve una lista con el coeficiente, la probabilidad binomial, la probabilidad acumulativa y las graficas correspondientes a las probabilidades
#' @export
#'
#' @examples
#' \dontrun{
#' # Directorio
#' directorio <- "C:\\Users\\CICEM\\OneDrive\\Escritorio\\ProbabilidadBinomial\\Prueba\\Datos.csv"
# Cargamos los datos
#'datos <- read.csv(directorio)
#'# Imprimimos las primeras filas
#'head(datos)
#'Elegimos los datos de la primera columna
#'y <-  datos[, 1]
#'y
#' Elegimos una probabilidad de éxito del 0.5
#' p = 0.5
#' Importamos la librería
#'library(ProbBinom)
#' Probamos la función
#'resultado <- FuncBin(y, p)
#'resultado
#' }
FuncBin <- function(y, p=0.5) {
  n <- length(y) # Número de ensayos
  x <- sum(y) #Número de éxitos
  # Coeficiente Binomial
  coeficiente_binomial <- factorial(n) / (factorial(x) * factorial(n - x))
  # Probabilidad
  probabilidad <- coeficiente_binomial * p^x * (1-p)^(n - x)
  # Probabilidad Acumulada
  k <- 0:x
  coeficiente_binomial_i <- factorial(n) / (factorial(k) * factorial(n - k))
  exitos <- p^k
  fracasos <- (1-p)^(n-k)
  probabilidades <- coeficiente_binomial_i*exitos*fracasos
  probabilidad_acumulada <- cumsum(probabilidades)

  tabla = data.frame(cbind(x=x, Coeficiente = coeficiente_binomial,
                           Probabilidad = probabilidad,
                           x_i = k, coeficiente_i = coeficiente_binomial_i,
                           Probabilidades = probabilidades,
                           Acumulada = probabilidad_acumulada
  ))
  # Gráfico de barras para Y
  histograma <- hist(y, main = "Número de éxitos y fracasos en Y", lwd=2,col = "purple")
  # Gráfico de probabilidades
  GrafProb <- plot(x=k, y = probabilidades, lwd = 2,
                   main = "Gráfico De Probabilidades", col= "blue" )
  # Gráfico para la acumulada
  GrafAcum <- plot(x=k, y = probabilidad_acumulada, type="l",
                   main="Probabilidad Acumulada", lwd=2, xlab = "x_i", col="green")
  return(list(tabla, histograma, GrafAcum))
}







