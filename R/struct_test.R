
#' Semiparametric test of structural dependence
#'
#' @param x Either a model, or an [ergmito::ergmito] fit.
#' @param y A numeric vector of length `length(x)`
#' @param stat Function that receives two arguments, a list of networks and
#' a vector of the same length.
#' @param R Integer. Number of replicates.
#' @param mc.cores Passed to [parallel::mclapply].
#' @param alternative String character, either `"two.sided"`, `"smaller"`, or
#' `"greater"`.
#' @param verbose Logical. When `TRUE`, shows information during the execution.
#' @param ... (Ignored)
#'
#' @export
struct_test <- function(
  x, y, stat,  R, mc.cores = 2L, verbose = TRUE,
  alternative = "two.sided",
  ...
  ) {
  UseMethod("struct_test")
}

#' @export
#' @rdname struct_test
struct_test.ergmito <- function(
  x, y, stat, R, mc.cores = 2L, verbose = TRUE,
  alternative = "two.sided",
  ...
  ) {

  n     <- ergmito::nnets(x)
  sizes <- ergmito::nvertex(x)



  # Generating the sampler
  samplers <- parallel::mclapply(seq_len(n), function(i) {

    # Updating the model
    netmodel <- x$formulae$model
    if (n == 1) {
      netmodel <- stats::update.formula(netmodel, x$network ~ .)
    } else {
      netmodel <- stats::update.formula(netmodel, x$network[[i]] ~ .)
    }

    # Setting the environment as the current
    environment(netmodel) <- environment()

    ergmito::new_rergmito(
      model = netmodel,
      theta = stats::coef(x)
    )
  }, mc.cores = mc.cores)


  # samplers
  struct_test.(x$network, y = y, samplers = samplers, stat = stat, R = R,
               mc.cores = mc.cores, verbose = verbose, alternative = alternative, ...)

}

#' @export
#' @rdname struct_test
struct_test.list <- function(
  x, y, stat, R, mc.cores = 2L, verbose = TRUE,
  alternative = "two.sided", ergmito.args = list(), ...
  ) {

  test <- which(!sapply(x, inherits, "ergmito_sampler"))
  if (length(test))
    stop("If `x` is a list, all its elements should be of class `ergmito_sampler`.",
         call. = FALSE)

  struct_test.(
    g           = lapply(x, "[[", "network0"),
    y           = y,
    samplers    = x,
    stat        = stat,
    R           = R,
    mc.cores    = mc.cores,
    verbose     = verbose,
    alternative = alternative,
    ...
    )

}

#' @export
#' @param ergmito.args A list with arguments passed to [ergmito::ergmito].
#' @rdname  struct_test
struct_test.formula <- function(
  x, y, stat, R, mc.cores = 2L, verbose = TRUE,
  alternative = "two.sided", ergmito.args = list(), ...
  ) {

  # Estimating the ergmito
  ans <- do.call(ergmito::ergmito, c(list(model = x), ergmito.args))

  # Calling the function
  struct_test.ergmito(ans, y, stat, R, mc.cores = mc.cores, verbose = verbose,
                      alternative = alternative, ...)

}

struct_test. <- function(g, y, samplers, stat, R, mc.cores, alternative, ...) {

  # Checking alternatives
  if (!is.character(alternative))
    stop("The `alternative` parameter must be a character scalar.", call. = FALSE)

  if (length(alternative) != 1)
    stop("The `alternative` parameter should be a character scalar.",
         " The passed value has a length different from 1.", call. = FALSE)

  if (!(alternative %in% c("less", "greater", "two.sided")))
    stop("Invalid `alternative` value. It must be either \"",
         paste(c("less", "greater", "two.sided"), collapse="\", \""), "\"",

         call. = FALSE
    )

  # Step 0: Computing observed statistic
  t0 <- stat(g, y)

  # Step 1: Create the output vector
  t <- matrix(NA, nrow = R, ncol = length(t0), dimnames = list(NULL, names(t0)))

  # Collecting seeds
  seed <- .Random.seed

  # Obtaining a random sample for each sampler (indexes)
  Idx <- lapply(samplers, function(s) {
    s$sample(R, s$sizes[1L], as_indexes = TRUE)
    })

  Idx <- do.call(cbind, Idx)

  # Step 2: Iterative steps
  for (i in 1L:R) {

    # Retrieving the current nets
    nets_r <- Map(function(s, idx) s[idx, s$sizes[1L]][[1L]], s = samplers, idx = Idx[i,])
    nets_r <- unlist(nets_r, recursive = FALSE)

    # Generating new random sample.
    t[i, ] <- stat(nets_r, y)

  }

  # Identifying complete obs
  cobs <- which(stats::complete.cases(t))

  # Calculating p-value
  if (alternative == "two.sided") {

    pvalue <- colMeans(t0 < t[cobs,,drop=FALSE])
    pvalue <- ifelse(pvalue > .5, 1 - pvalue, pvalue)*2

  } else if (alternative == "less") {

    pvalue <- colMeans(t0 < t[cobs,,drop=FALSE])

  } else if (alternative == "greater") {

    pvalue <- colMeans(t0 > t[cobs,,drop=FALSE])

  }

  # pval <- colMeans(t0 <= t)
  # pval <- ifelse(pval > .1, 1-pval, pval)*2

  structure(
    list(
      t           = t,
      t0          = t0,
      pvalue      = pvalue,
      alternative = alternative,
      R           = R,
      samplers    = samplers,
      call        = sys.call(),
      seed        = seed,
      n           = length(samplers),
      stat        = stat,
      obs.used    = cobs
    ),
    class = "gnet_struct_test"
  )

}

#' @export
#' @rdname struct_test
print.gnet_struct_test <- function(x, ...) {

  cat("Test of structural association between a network and a graph level outcome\n")
  cat(sprintf("# of obs: %i\n# of replicates: %i (%i used)\n", x$n, x$R, length(x$obs.used)))
  cat(sprintf("Alternative: %s\n", x$alternative))
  cat(sprintf(
    "S[%i] s(obs): %6.4f s(sim): %6.4f p-val: %6.4f",
    seq_along(x$t0), x$t0, colMeans(x$t[x$obs.used,,drop=FALSE]), x$pvalue
    ), "", sep = "\n")

  invisible(x)

}
