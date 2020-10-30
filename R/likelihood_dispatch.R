

#'
#' @param distribution
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
likelihood <- function(...) {

  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("distribution"), names(mf), 0L)
  mf <- mf[c(1L, m)]

  parameters <- as.list(match.call(expand.dots = FALSE))

  distribution <- paste0(parameters$distribution %||%
    "normal", ".likelihood")



  lik_fun <- purrr::partial(.f = rlang::as_function(distribution),...)

  return(lik_fun())
}



#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
student_t.likelihood <- function(d,n) { # nolint




  # TODO:
  # Need a way to deal with
  # scaled and shifted t distributions
  #parameters <- as.list(match.call(expand.dots = TRUE))
  #center <- parameters$center
  #df <- parameters$df
  #scale <- parameters$scale %||% 1
    scale = 1

    lik_type <- "non-central t"
    lik_func <- function(theta) {
      dt(x = sqrt(n) * d, df = n - 1, ncp = sqrt(n) * theta)
    }
    params <- list(d = d, n = n)
    desc <-""# paste0("Parameters\nCenter: ", params$center, "\ndf: ", params$df)

  new(
    Class = "likelihood",
    data = list(likelihood_type = "student_t",
                parameters = params),
    func = lik_func,
    marginal = lik_func,
    desc = desc,
    dist.type = "continuous",
    plot = list(
      fun = "dt",
      params = list(d = d,
                    n = n)
    )
  )

}


normal.likelihood <- function(center, scale) { # nolint
  lik_func <- function(theta) {
    dnorm(x = center, mean = theta, sd = scale)
  }
  marginal <- function(theta, center) {
    dnorm(x = center, mean = theta, sd = scale)
  }
  params <- list(center = center, scale = scale)
  desc <- paste0(
    "Parameters\nCenter: ", params$center,
    "\nScale: ", params$scale
  )
  new(
    Class = "likelihood",
    data = list(likelihood_type = "normal", parameters = params),
    func = lik_func,
    marginal = marginal,
    desc = desc,
    dist.type = "continuous",
    plot = list(
      fun = "dnorm",
      params = list(mean = center,
                    sd = scale)
    )
  )
}

#' Title
#'
#' @param successes
#' @param trials
#'
#' @return
#' @export
#'
#' @examples
binomial.likelihood <- function(successes, trials){

  lik_func <- function(theta){
    dbinom(x = successes, size = trials, prob = theta)
  }

  marginal <- function(theta){
    dbinom(x = successes, size = trials, prob = theta)
  }

  params <- list(successes = successes, trials = trials)

  desc <- paste0(""
  )

  new(
    Class = "likelihood",
    data = list(likelihood_type = "binomial", parameters = params),
    func = lik_func,
    marginal = marginal,
    desc = desc,
    dist.type = "continuous",
    plot = list(
      fun = "dbinom",
      params = list(x = successes,
                    size =  trials)
    )
  )


}



# b <- likelihood(center = 0, scale = 1)

# bayesplay::plot.likelihood(b, theta = seq(-20, 20, .01))


# write a 'compare' function that takes two marginals and a
# range of observations


# write a predict function that takes a new parameter and generates a bf?!

# TODO: Include function for calculating posteriors....
# Compare posteriors generated from function
# Together will posteriors generated
# Using conjugate priors
# And posteriors using estimate... using Greta
# Or someting similar in Julia

