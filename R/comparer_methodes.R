#' Comparer les methodes d'integration
#'
#' Applique toutes les methodes du package a une meme integrale et renvoie un
#' tableau comparatif : valeur obtenue, nombre **reel** d'evaluations de `f`
#' (compte automatiquement), erreur absolue (si la valeur exacte est connue) et
#' temps de calcul.
#'
#' @inheritParams newton_cotes
#' @param exact Valeur exacte de l'integrale (optionnelle). Si elle est
#'   fournie, la colonne `Erreur_abs` est renseignee.
#' @param n Nombre d'intervalles pour les rectangles, les trapezes et Simpson.
#' @param n_gauss Nombre de points de Gauss-Legendre.
#' @param k_romberg Nombre de niveaux de Romberg.
#' @param N_mc Nombre de points de Monte-Carlo.
#' @param seed Graine pour Monte-Carlo.
#'
#' @return Un `data.frame` avec les colonnes `Methode`, `Evaluations`,
#'   `Valeur`, `Erreur_abs` et `Temps_ms`.
#'
#' @examples
#' f <- function(x) exp(-x^2)
#' exact <- sqrt(pi) / 2 * (2 * pnorm(2 * sqrt(2)) - 1)
#' comparer_methodes(f, 0, 2, exact = exact)
#'
#' @seealso [ordre_observe()]
#' @export
comparer_methodes <- function(f, a, b, exact = NULL, n = 16, n_gauss = 5,
                              k_romberg = 5, N_mc = 1e5, seed = 42) {
  .verifier_fonction(f)
  .verifier_bornes(a, b)
  specs <- list(
    "Rectangles (gauche)" = function(g) rectangle_gauche(g, a, b, n),
    "Rectangles (milieu)" = function(g) rectangle_milieu(g, a, b, n),
    "Trapezes"            = function(g) trapeze(g, a, b, n),
    "Simpson"             = function(g) simpson(g, a, b, n),
    "Romberg"             = function(g) romberg(g, a, b, k_romberg),
    "Gauss-Legendre"      = function(g) gauss_legendre(g, a, b, n_gauss),
    "Monte-Carlo"         = function(g) monte_carlo(g, a, b, N_mc, seed)$valeur
  )
  lignes <- lapply(names(specs), function(nom) {
    cpt <- .compteur(f)
    t0 <- proc.time()[["elapsed"]]
    val <- specs[[nom]](cpt$f)
    temps <- (proc.time()[["elapsed"]] - t0) * 1000
    data.frame(
      Methode = nom,
      Evaluations = cpt$n(),
      Valeur = val,
      Erreur_abs = if (is.null(exact)) NA_real_ else abs(val - exact),
      Temps_ms = temps,
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, lignes)
  rownames(res) <- NULL
  res
}

#' Ordre de convergence observe
#'
#' Calcule l'erreur absolue d'une methode de Newton-Cotes pour une suite de
#' valeurs de `n`, puis l'ordre de convergence observe
#' \eqn{p = \log(E_{k-1}/E_k) / \log(n_k/n_{k-1})}. La theorie predit
#' \eqn{p = 1} (rectangles a gauche), \eqn{p = 2} (rectangles au milieu et
#' trapezes) et \eqn{p = 4} (Simpson).
#'
#' @inheritParams newton_cotes
#' @param exact Valeur exacte de l'integrale.
#' @param methode Methode etudiee : `"simpson"`, `"trapeze"`,
#'   `"rectangle_milieu"` ou `"rectangle_gauche"`.
#' @param n Vecteur croissant de nombres d'intervalles (defaut 2, 4, ..., 64).
#'
#' @return Un `data.frame` avec les colonnes `n`, `erreur` et `ordre`.
#'
#' @examples
#' f <- function(x) exp(-x^2)
#' exact <- sqrt(pi) / 2 * (2 * pnorm(2 * sqrt(2)) - 1)
#' ordre_observe(f, 0, 2, exact, methode = "simpson")
#' ordre_observe(f, 0, 2, exact, methode = "trapeze")
#'
#' @seealso [comparer_methodes()]
#' @export
ordre_observe <- function(f, a, b, exact,
                          methode = c("simpson", "trapeze",
                                      "rectangle_milieu", "rectangle_gauche"),
                          n = 2^(1:6)) {
  .verifier_fonction(f)
  .verifier_bornes(a, b)
  methode <- match.arg(methode)
  if (!is.numeric(exact) || length(exact) != 1L || !is.finite(exact)) {
    stop("`exact` doit etre un nombre reel fini.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) < 2L || is.unsorted(n, strictly = TRUE)) {
    stop("`n` doit etre un vecteur numerique strictement croissant.",
         call. = FALSE)
  }
  fun <- switch(methode,
    simpson = simpson,
    trapeze = trapeze,
    rectangle_milieu = rectangle_milieu,
    rectangle_gauche = rectangle_gauche
  )
  erreur <- vapply(n, function(k) abs(fun(f, a, b, k) - exact), numeric(1))
  k <- length(n)
  ordre <- c(NA_real_, log(erreur[-k] / erreur[-1L]) / log(n[-1L] / n[-k]))
  data.frame(n = n, erreur = erreur, ordre = ordre)
}
