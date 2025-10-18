simular_juego <- function(objetivo_chupetines = 10, usar_regla_B = FALSE, max_iter = 1000) {
  caramelos <- c(cafe = 15, naranja = 15, verde = 15)
  chupetines <- 0
  iter <- 0
  
  while (chupetines < objetivo_chupetines && iter < max_iter) {
    if (all(caramelos >= 1)) {
      caramelos <- caramelos - 1
      chupetines <- chupetines + 1
      iter <- iter + 1
      next
    }
    if (!usar_regla_B || chupetines == 0 || sum(caramelos) == 0) stop("No se puede continuar")
    tipo <- sample(names(caramelos)[caramelos > 0], 1)
    caramelos[tipo] <- caramelos[tipo] - 1
    chupetines <- chupetines - 1
    nuevos <- sample(names(caramelos), 4, replace = TRUE)
    caramelos <- caramelos + table(factor(nuevos, levels = names(caramelos)))
    iter <- iter + 1
  }
  list(interacciones = iter, chupetines = chupetines, caramelos_finales = caramelos)
}

set.seed(NULL)
resultado_optimo <- simular_juego(objetivo_chupetines = 10, usar_regla_B = FALSE)
print(resultado_optimo)

