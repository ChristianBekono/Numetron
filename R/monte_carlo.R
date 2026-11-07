#' Integration par la methode de Monte-Carlo
#'
#' Estime \eqn{\int_a^b f(x)\,dx} par
#' \eqn{\frac{b-a}{N} \sum_{k=1}^{N} f(U_k)} ou les \eqn{U_k} sont tires
#' uniformement dans \eqn{[a, b]}. L'erreur statistique est
#' \eqn{\sigma / \sqrt{N}} : elle decroit lentement (100 fois plus de points
#' pour 10 fois plus de precision) mais **ne depend pas de la dimension**, ce
#' qui rend la methode adaptee aux integrales multiples.
#'
#' @inheritParams newton_cotes
#' @param N Nombre de points aleatoires (entier >= 2, defaut `1e5`).
#' @param seed Graine du generateur aleatoire pour la reproductibilite (defaut
#'   42). Utiliser `NULL` pour ne pas fixer la graine.
#'
#' @return Une liste avec :
#' \describe{
#'   \item{`valeur`}{estimation de l'integrale ;}
#'   \item{`erreur_std`}{erreur standard estimee \eqn{\hat\sigma/\sqrt{N}} ;}
#'   \item{`ic95`}{intervalle de confiance a 95 pour cent (vecteur de longueur 2) ;}
#'   \item{`N`}{nombre de points utilises.}
#' }
#'
#' @examples
#' f <- function(x) exp(-x^2)
#' res <- monte_carlo(f, 0, 2, N = 1e5, seed = 42)
#' res$valeur
#' res$erreur_std
#' res$ic95
#'
#' @seealso [gauss_legendre()], [comparer_methodes()]
#' @export
monte_carlo <- function(f, a, b, N = 1e5, seed = 42) {
  .verifier_fonction(f)
  .verifier_bornes(a, b)
  N <- .verifier_entier(N, "N", min = 2L)
  if (!is.null(seed)) set.seed(seed)
  y <- (b - a) * .evaluer(f, runif(N, a, b))
  valeur <- mean(y)
  erreur <- sd(y) / sqrt(N)
  list(
    valeur = valeur,
    erreur_std = erreur,
    ic95 = valeur + c(-1, 1) * qnorm(0.975) * erreur,
    N = N
  )
}
