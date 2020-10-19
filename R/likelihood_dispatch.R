
`%||%` <- function(x, y) { # nolint
  if (purrr::is_empty(x)) y else x
}

likelihood <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))

  distribution <- paste0(parameters$distribution %||%
    "normal", ".likelihood")

  lik_fun <- purrr::partial(.f = rlang::as_function(distribution), ...)

  return(lik_fun())
}



student_t.likelihood <- function(...) { # nolint


  parameters <- as.list(match.call(expand.dots = TRUE))
  center <- parameters$center
  df <- parameters$df
  scale <- parameters$scale %||% 1

  if (scale == 1) {
    lik_type <- "non-central t"
    lik_func <- function(theta) {
      dt(sqrt(df + 1) * center, df, sqrt(df + 1) * theta)
    }
    params <- list(center = center, df = df)
    desc <- paste0("Parameters\nCenter: ", params$center, "\ndf: ", params$df)
  } else {
    lik_type <- "Scaled and shifted t"
    lik_func <- function(theta) {
      dt((center - theta) / scale, df = df)
    }
    params <- list(center = center, scale = scale, df = df)
    desc <- paste0(
      "Parameters\nCenter: ", params$center, "\nScale: ",
      params$scale, "\ndf: ", params$df
    )
  }

  new(
    Class = "likelihood",
    data = list(
      lik.type = lik_type,
      parameters = params
    ),
    func = lik.func,
    desc = desc,
    dist.type = "continuous"
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
    dist.type = "continuous"
  )
}



# b <- likelihood(center = 0, scale = 1)

# bayesplay::plot.likelihood(b, theta = seq(-20, 20, .01))


# prior dispatch function
prior <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))

  range <- parameters$range %||% c(-Inf, Inf) # nolint

  # prior function needs parameters for
  # distribution - normal, student_t, beta, cauchy, uniform, point
  # parameters - parameters for the distributions
  # range_of_support :: for one tailed etc

  distribution <- paste0(parameters$distribution %||%
    "uniform", ".prior")

  lik_fun <- purrr::partial(
    .f = rlang::as_function(distribution),
    range = range, ...
  )

  return(lik_fun())
}


# functions for different prior distributions
normal.prior <- function() {} # nolint

student_t.prior <- function() {} # nolint

beta.prior <- function() {} # nolint

cauchy.prior <- function() {} # nolint

uniform.prior <- function(min, max, range) { # nolint

  new(
    Class = "prior",
    theta_range = range,
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dunif(x = theta, min = ",
          min, ", max = ",
          max, ")"
        ))
    )),
    type = "uniform",
    parameters = list(min = min, max = max)
  )
}

point.prior <- function(range, point = 0) { # nolint

  new(
    Class = "prior",
    theta_range = c(point,point),
    func = eval(parse(
      text =
        (paste0(
          "function(theta) ifelse(theta == ",point,", 1, 0)"
        ))
    )),
    type = "point",
    parameters = list(point = point)

  )

}


# write a 'compare' function that takes two marginals and a
# range of observations


# write a predict function that takes a new parameter and generates a bf?!
