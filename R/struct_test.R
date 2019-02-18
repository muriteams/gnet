
#' Semiparametric test of structural dependence
#'
#' @param x Either a model, or an [ergmito::ergmito] fit.
#' @param y A numeric vector of length `length(x)`
#' @param stat Function that receives two arguments, a list of networks and
#' a vector of the same length.
#' @param R Integer. Number of replicates.
#' @param mc.cores Passed to [parallel::mclapply].
#' @param verbose Logical. When `TRUE`, shows information during the execution.
#' @param ... (Ignored)
#'
#' @export
struct_test <- function(x, y, stat,  R, mc.cores = 2L, verbose = TRUE, ...) UseMethod("struct_test")

#' @export
#' @rdname struct_test
struct_test.ergmito <- function(x, y, stat, R, mc.cores = 2L, verbose = TRUE, ...) {

  n     <- ergmito::nnets(x)
  sizes <- ergmito::nvertex(x)

  # Updating the model
  netmodel <- x$formulae$model
  if (n == 1)
    netmodel <- stats::update.formula(netmodel, x$network ~ .)
  else
    netmodel <- stats::update.formula(netmodel, x$network[[i]] ~ .)

  environment(netmodel) <- environment()

  # Generating the sampler
  samplers <- parallel::mclapply(seq_len(n), function(i) {
    ans <- ergmito::new_rergmito(
      model = netmodel,
      theta = stats::coef(x)
    )

    ans$size <- sizes[i]

    ans
  }, mc.cores = mc.cores)


  # samplers
  struct_test.(x$network, y = y, samplers = samplers, stat = stat, R = R,
               mc.cores = mc.cores, ...)

}

#' @export
#' @rdname struct_test
struct_test.list <- function(x, y, stat, R, mc.cores = 2L, verbose = TRUE, ergmito.args = list(), ...) {

  test <- which(!sapply(x, inherits, "ergmito_sampler"))
  if (length(test))
    stop("If `x` is a list, all its elements should be of class `ergmito_sampler`.",
         call. = FALSE)

  struct_test.(
    g        = lapply(x, "[[", "network0"),
    y        = y,
    samplers = x,
    stat     = stat,
    R        = R,
    mc.cores    = mc.cores,
    ...
    )

}

#' @export
#' @param ergmito.args A list with arguments passed to [ergmito::ergmito].
#' @rdname  struct_test
struct_test.formula <- function(x, y, stat, R, mc.cores = 2L, verbose = TRUE, ergmito.args = list(), ...) {

  # Estimating the ergmito
  ans <- do.call(ergmito::ergmito, c(list(model = x), ergmito.args))

  # Calling the function
  struct_test.ergmito(ans, y, stat, R, mc.cores = mc.cores, ...)

}

struct_test. <- function(g, y, samplers, stat, R, mc.cores, ...) {

  # Step 0: Computing observed statistic
  s0 <- stat(g, y)

  # Step 1: Create the output vector
  ans <- vector("numeric", R)

  # debug(stat)

  # Step 2: Iterative steps
  for (i in 1L:R) {

    # Generating new random sample.
    ans[i] <- stat(
      parallel::mclapply(
        samplers, function(s) s$sample(1L, s$size)[[1L]],
        mc.cores = mc.cores
        ),
      y
    )

  }

  list(
    stat     = ans,
    stat0    = s0,
    samplers = samplers
  )

}
