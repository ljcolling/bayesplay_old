#' Title
#'
#' @param distribution
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
prior <- function(distribution, ...) {
  parameters <- as.list(match.call(expand.dots = TRUE))

  range <- parameters$range %||% c(-Inf, Inf) # nolint

  if (parameters$distribution == "beta") {
    range <- c(0, 1)
  }


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
normal.prior <- function(mean,sd,range) { # nolint

  new(
    Class = "prior",
    theta_range = range,
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dnorm(x = theta, mean = ",
          mean, ", sd = ",
          sd, ")"
        ))
    )),
    type = "normal",
    plot = list(fun = "dnorm", params = list(mean = mean, sd = sd)),
    parameters = list(mean = mean, sd = sd)
  )

}

student_t.prior <- function() {} # nolint

#' Title
#'
#' @return
#' @export
#'
#' @examples
beta.prior <- function(shape1,shape2,range) { # nolint

  new(
    Class = "prior",
    theta_range = range,
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dbeta(x = theta, shape1 = ",
          shape1, ", shape2 = ",
          shape2, ")"
        ))
    )),
    type = "beta",
    plot = list(fun = "dbeta", params = list(shape1 = shape1, shape2 = shape2)),
    parameters = list(shape1 = shape1, shape2 = shape2)
  )


}





#' Title
#'
#' @param scale
#' @param location
#' @param range
#'
#' @return
#' @export
#'
#' @examples
cauchy.prior <- function(scale, location = 0, range) { #nolint
  new(
    Class = "prior",
    theta_range = range,
    func = eval(parse(
      text =
        (paste0(
          "function(theta) dcauchy(x = theta, location = ",
          location, ", scale = ",
          scale, ")"
        ))
    )),
    type = "cauchy",
    plot = list(fun = "dcauchy", params = list(location = location, scale = scale)),
    parameters = list(location = location, scale = scale)
  )

}

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

