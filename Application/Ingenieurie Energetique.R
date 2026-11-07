#=====================================================
 # APPLICATION 1 : INGENIERIE THERMIQUE
#Calcul de l'energie consommee par un chauffage
#=====================================================

library(Numetron)

#1. Modele de puissance
P <- function(t) {
2 + 2.4*t
}

#2. Intervalle de fonctionnement
a <- 0
b <- 1

#3. Valeur exacte
E_exacte <- 3.2

#4. Application des methodes numeriques
resultats_thermiques <- list(
"Rectangles gauche" = integrate_left(P, a, b, n = 100),
"Rectangles droite" = integrate_right(P, a, b, n = 100),
"Point milieu" = integrate_midpoint(P, a, b, n = 100),
"Trapezes" = integrate_trapezoid(P, a, b, n = 100),
"Simpson 1/3" = integrate_simpson_13(P, a, b, n = 100),
"Simpson 3/8" = integrate_simpson_38(P, a, b, n = 99),
"Gauss 2 points" = integrate_gauss_legendre(P, a, b, points = 2),
"Gauss 3 points" = integrate_gauss_legendre(P, a, b, points = 3),
"Romberg" = integrate_romberg(P, a, b),
"Adaptative" = integrate_adaptive(P, a, b)
)

#5. Extraire les estimations
extraire <- function(x) {
if (is.list(x) && !is.null(x$estimate)) { return(as.numeric(x$estimate))
}
as.numeric(x)[1]
}

estimations <- sapply(resultats_thermiques, extraire)

#6. Construire le tableau comparatif
tableau_thermique <- data.frame(
Methode = names(estimations),
Energie_kWh = as.numeric(estimations),
Energie_exacte = E_exacte,
Erreur_absolue = abs(as.numeric(estimations) - E_exacte)
)

#7. Afficher les resultats
print(tableau_thermique, row.names = FALSE)

#8. Visualiser la puissance au cours du temps
temps <- seq(0, 1, length.out = 100)
puissance <- P(temps)

graph <- plot(
temps, puissance,
type = "l",
lwd = 2,
col = "blue",
main = "Puissance du chauffage industriel",
xlab = "Temps (heures)",
ylab = "Puissance (kW)"
)

#9. Afficher l'energie calculee
cat("Energie exacte :", E_exacte, "kWh\n")
graph
