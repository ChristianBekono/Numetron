#=====================================================
#  APPLICATION 2 : INGENIERIE HYDRAULIQUE
#Calcul du volume d'eau pompe
#=====================================================

library(Numetron)

#1. Modele du debit
Q <- function(t) {
10 + 0.2*t
}

#2. Intervalle de pompage
a <- 0
b <- 30

#3. Valeur exacte
V_exacte <- 390

#4. Application des methodes numeriques
resultats_hydrauliques <- list(
"Rectangles gauche" = integrate_left(Q, a, b, n = 100),
"Rectangles droite" = integrate_right(Q, a, b, n = 100),
"Point milieu" = integrate_midpoint(Q, a, b, n = 100),
"Trapezes" = integrate_trapezoid(Q, a, b, n = 100),
"Simpson 1/3" = integrate_simpson_13(Q, a, b, n = 100),
"Simpson 3/8" = integrate_simpson_38(Q, a, b, n = 99),
"Gauss 2 points" = integrate_gauss_legendre(Q, a, b, points = 2),
"Gauss 3 points" = integrate_gauss_legendre(Q, a, b, points = 3),
"Romberg" = integrate_romberg(Q, a, b),
"Adaptative" = integrate_adaptive(Q, a, b)
)

#5. Extraire les estimations
extraire <- function(x) {
if (is.list(x) && !is.null(x$estimate)) { return(as.numeric(x$estimate))
}
as.numeric(x)[1]
}

estimations <- sapply(resultats_hydrauliques, extraire)

#6. Construire le tableau comparatif
tableau_hydraulique <- data.frame(
Methode = names(estimations),
Volume_m3 = as.numeric(estimations),
Volume_exact = V_exacte,
Erreur_absolue = abs(as.numeric(estimations) - V_exacte)
)

#7. Afficher les resultats
print(tableau_hydraulique, row.names = FALSE)

#8. Visualiser le debit au cours du temps
temps <- seq(0, 30, length.out = 100)
debit <- Q(temps)

plot(
temps, debit,
type = "l",
lwd = 2,
col = "darkgreen",
main = "Evolution du debit de la pompe",
xlab = "Temps (minutes)",
ylab = "Debit (m3/min)"
)

#9. Afficher le volume calcule
cat("Volume exact :", V_exacte, "m3\n")
