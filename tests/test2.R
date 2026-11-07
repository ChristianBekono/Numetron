
# Toutes les methodes d'integration numerique

# 1. Charger le package
library(Numetron)

# 2. Definir la fonction a integrer
f <- function(x) {
  x^3 + 2*x^2 - 5*x + 3
}

# 3. Definition des bornes bornes
a <- 0
b <- 2

# 4. Calculer la valeur exacte
F <- function(x) {
  x^4/4 + 2*x^3/3 - 5*x^2/2 + 3*x
}

valeur_exacte <- F(b) - F(a)

cat("\n====================================\n")
cat("VALEUR EXACTE :", valeur_exacte, "\n")
cat("====================================\n")

# 5. Fonction pour extraire le resultat
extraire_estimation <- function(resultat) {
  if (is.list(resultat) && !is.null(resultat$estimate)) {
    return(as.numeric(resultat$estimate))
  }
  return(as.numeric(resultat)[1])
}

# 6. Executer toutes les methodes
resultats <- list(
  "Rectangles gauche" = integrate_left(f, a, b, n = 100),
  "Rectangles droite" = integrate_right(f, a, b, n = 100),
  "Point milieu" = integrate_midpoint(f, a, b, n = 100),
  "Trapezes" = integrate_trapezoid(f, a, b, n = 100),
  "Simpson 1/3" = integrate_simpson_13(f, a, b, n = 100),
  "Simpson 3/8" = integrate_simpson_38(f, a, b, n = 99),
  "Gauss Legendre 2 points" = integrate_gauss_legendre(f, a, b, points = 2),
  "Gauss Legendre 3 points" = integrate_gauss_legendre(f, a, b, points = 3),
  "Romberg" = integrate_romberg(f, a, b),
  "Adaptative" = integrate_adaptive(f, a, b)
)

# 7. Construire le tableau comparatif
estimations <- sapply(resultats, extraire_estimation)

tableau <- data.frame(
  Methode = names(estimations),
  Approximation = as.numeric(estimations),
  Valeur_exacte = valeur_exacte,
  Erreur_absolue = abs(as.numeric(estimations) - valeur_exacte)
)

# 8. Afficher les resultats
cat("\n========== COMPARAISON DES METHODES ==========\n")
print(tableau, row.names = FALSE, digits = 10)

# 9. Comparaison avec la fonction integree de R
reference_R <- stats::integrate(f, lower = a, upper = b)$value

cat("\nReference stats::integrate() :", reference_R, "\n")
cat("Valeur exacte                :", valeur_exacte, "\n")
