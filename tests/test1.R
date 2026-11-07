library(Numetron)

f <- function(x) {
  exp(-x^2)
}

a <- 0
b <- 2

gauche <- integrate_left(f, a, b, n = 100)
droite <- integrate_right(f, a, b, n = 100)
milieu <- integrate_midpoint(f, a, b, n = 100)
trapeze <- integrate_trapezoid(f, a, b, n = 100)
simpson <- integrate_simpson_13(f, a, b, n = 100)

gauche$estimate
droite$estimate
milieu$estimate
trapeze$estimate
simpson$estimate


F <- function(x) {
  x^4/4 + 2*x^3/3 - 5*x^2/2 + 3*x
}

exacte <- F(2) - F(0)

exacte



resultats <- data.frame(
  Methode = c(
    "Rectangles gauche",
    "Rectangles droite",
    "Point milieu",
    "Trapezes",
    "Simpson 1/3"
  ),
  Approximation = c(
    gauche$estimate,
    droite$estimate,
    milieu$estimate,
    trapeze$estimate,
    simpson$estimate
  )
)

resultats$Erreur <- abs(
  resultats$Approximation - exacte
)

print(resultats)
