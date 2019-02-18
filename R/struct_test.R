
#' Semiparametric test of structural dependence
#'
#' @param x Either a model, or an [ergmito::ergmito] fit.
#' @param y A numeric vector of length `length(x)`
#' @param stat Function that receives two arguments, a network and a scalar.
#' @param R Integer. Number of replicates.
#' @param ... (Ignored)
#'
#' @export
struct_test <- function(x, y, stat,  R, ...) UseMethod("struct_test")

#' @export
#' @rdname struct_test
struct_test.ergmito <- function(x, y, stat, R, ...) {

  # Generating the sampler
  sampler <- ergmito::new_rergmito(
    model = x$formulae$model,
    theta = stats::coef(x),
    sizes = range(ergmito::nnets(x$network))
    )
}

#' @export
#' @param ergmito.args A list with arguments passed to [ergmito::ergmito].
#' @rdname  struct_test
struct_test.formula <- function(x, y, stat, R, ergmito.args = list(), ...) {

  # Estimating the ergmito
  ans <- do.call(ergmito::ergmito, c(list(model = x), ergmito.args))

  # Calling the function
  struct_test.ergmito(ans, y, stat, R, ...)

}

struct_test. <- function(g, y, sampler, stat, sizes, R) {

  # Step 0: Computing observed statistic
  s0 <- stat(g, y)

  # Step 1: Create the output vector
  ans <- vector("numeric", R)

  # Step 2: Iterative steps
  for (i in 1L:R) {

    # Generating new random sample.

  }

}
