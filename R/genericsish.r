# plot generic for likelihood

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot.bayesplay <- function(x) {
  ggplot2::ggplot() +  ggplot2::geom_function(fun = x@plot$fun,
                            args = x@plot$params, colour = "black")
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_bayesplay <- function(x,...){
  ggplot2::geom_function(fun = x@plot$fun,
                         args = x@plot$params, ...)

}

# as_tibble for likelihood
as_tibble.likelihood <- function(x, theta, ...){
  if (missing(theta)){stop("You must enter a theta range!")}
  theta <- theta
  likelihood <- suppressWarnings(expr =  x@func(theta = theta)) # suppress warning from t dist accuracy
  return(tibble(theta = theta,
                likelihood = likelihood))
}


# as_tibble for marginal
as_tibble.marginal <- function(x, theta, ...){
  if (missing(theta)){stop("You must enter a theta range!")}
  theta <- theta
  likelihood <- suppressWarnings(expr =  alt@data$marginal(theta = theta)) # suppress warning from t dist accuracy
  return(tibble(theta = theta,
                likelihood = likelihood))
}


# area under point null
point.null <- function(data.model,null.value){
  if(class(data.model) != "likelihood"){stop("data.model must be a likelihood")}
  data.model@func(theta = null.value)
}

get.prior <- function(alt,theta.range){
  prior.func <- function(theta){alt@K * alt@prior(theta)}
  tibble::tibble(theta = theta.range,
                 likelihood = prior.func(theta.range))
}



#' Title
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
#'
#' @examples
`*.bayesplay` <-function(e1,e2){
  theta.range = e2@theta_range

  likelihood.func <- e1@func
  prior.func <- e2@func

  # normalise the pior
  if(theta.range[1] != theta.range[2]){
    K = suppressWarnings(1 / stats::integrate(f = prior.func, lower = theta.range[1], upper = theta.range[2])$value)
  } else{
    K = 1
  }
  marginal <- function(theta){suppressWarnings(likelihood.func(theta = theta) * (K * prior.func(theta = theta)))}

  if(theta.range[1] != theta.range[2]){
    alt.val <- suppressWarnings(stats::integrate(marginal,theta.range[1],theta.range[2])$value)
  } else {
    alt.val <- marginal(theta.range[[1]])
  }

  alt.func <- marginal
  data = list(
    integral = alt.val,
    marginal = marginal,
    prior.normalising.constant = K)

  new(Class = 'predictive',
      data = data,
      K = K,
      lik = likelihood.func,
      prior = prior.func,
      theta.range = theta.range)

}


#' Title
#'
#' @param e1
#' @param e2
#'
#' @return
#' @export
#'
#' @examples
`/.predictive` <- function(e1, e2){
  e1@data$integral / e2@data$integral
}


# non-central t likelihood
t.lik <- function(center,df){
  lik.func <- function(theta){dt(sqrt(df + 1)*center,df,sqrt(df + 1)*theta)}
  params = list(center = center, df = df)
  desc = paste0("Parameters\nCenter: ",params$center,"\nDF: ",params$df)
  new(Class = 'likelihood',
      data = list(lik.type = "non-central t", parameters = params),
      func = lik.func,
      desc = desc,
      dist.type = "continuous")


}

# Shifted scaled t likelihood
scaled.shifted.t.lik <- function(center, scale, df){
  lik.func <- function(theta){dt((center - theta) / scale, df = df)}
  params = list(center = center, scale = scale, df = df)
  desc = paste0("Parameters\nCenter: ",params$center,"\nScale: ",params$scale,"\nDF: ",params$df)
  new(Class = 'likelihood',
      data = list(lik.type = "Shifted and Scaled t", parameters = params),
      func = lik.func,
      desc = desc,
      dist.type = "continuous")

}

# gaussian likelihood
norm.lik <- function(center, scale){
  lik.func <- function(theta){dnorm(x = center, mean = theta, sd = scale)}
  params = list(center = center, scale = scale, df = df)
  desc = paste0("Parameters\nCenter: ",params$center,"\nScale: ",params$scale)
  new(Class = 'likelihood',
      data = list(lik.type = "normal", parameters = params),
      func = lik.func,
      desc = desc,
      dist.type = "continuous")
}


GenSynthSample = function(mean, sd, se, n){
  inputs = as.list(sys.call())
  if(sum(names(inputs) %in% c("se","sd")) == 2){
    stop("input se OR sd")
  }

  if("se" %in% names(inputs)){
    sd = se * sqrt(n)
  }

  x = mean + sd * scale(rnorm(n, 0, 0))
  return(as.numeric(x))
}


tidy.BFBayesFactor <- function(x, ...){
  nullInterval = x@numerator[[1]]@prior$nullInterval

  if(class(x@numerator[[1]])[1] == "BFoneSample"){

    mean = mean(x@data$y)
    return(tibble::tibble(
      estimate = mean,
      BF10 = exp(x@bayesFactor$bf),
      BF01 = 1/exp(x@bayesFactor$bf),
      rscale = x@numerator[[1]]@prior$rscale,
      priortype = x@denominator@type,
      null = x@denominator@longName,
      nullInterval = ifelse(is.null(nullInterval),NA,nullInterval),
      method = class(x@numerator[[1]])[1]))



  } else {


    group.means = by(x@data$y,x@data$group, FUN = base::mean)

    return(tibble::tibble(estimate1 =
                            group.means[[1]],
                          estimate2 =
                            group.means[[2]],
                          BF10 = exp(x@bayesFactor$bf),
                          BF01 = 1/exp(x@bayesFactor$bf),
                          rscale = x@numerator[[1]]@prior$rscale,
                          priortype = x@denominator@type,
                          null = x@denominator@longName,
                          nullInterval = ifelse(is.null(nullInterval),NA,nullInterval),
                          method = class(x@numerator[[1]])[1]))
  }

}

loadpackages = function(){
  suppressMessages(require(tidyverse))
  suppressMessages(require(brms))
  suppressMessages(require(patchwork))
  suppressMessages(require(bayesplay))
  suppressMessages(require(BayesFactor))
  suppressMessages(require(logspline))
  suppressMessages(require(furrr))
  suppressMessages(require(tidyverse))
  suppressMessages(require(magrittr))
  suppressMessages(require(logspline))
  suppressMessages(require(patchwork))
  suppressMessages(require(IRdisplay))
  suppressMessages(require(broom))
  suppressMessages(require(glue))
  suppressMessages(require(invgamma))
  suppressMessages(require(metRology))

}


bfsay = function(BF, numerator = "H1", denominator = "H0"){
  if(BF < 1){
    model = denominator
    BF_base = BF
    BF = 1 / BF
  } else {
    model = numerator
    BF_base = BF
  }

  ev_level = dplyr::case_when(BF == 1 ~ "No evidence",
                              BF > 1 & BF <= 3 ~ "Anecdotal evidence",
                              BF > 3 & BF <= 10 ~ "Moderate evidence",
                              BF > 10 & BF <= 30 ~ "Strong evidence",
                              BF > 30 & BF <= 100 ~ "Very strong evidence",
                              BF > 100 ~ "Extreme evidence")


 print(glue::glue("Using the levels from  Wagenmakers et al (2017; https://doi.org/10.3758/s13423-017-1323-7)"))
 print(glue::glue("A BF of {BF_base} indicates:"))
 print(glue::glue("{ev_level} for {model}"))



}


plot_prior = function(prior,theta.range){
  # then make a tibble with the data for the plot
  tibble(theta = theta.range, p = prior(theta.range)$func(theta.range)) %>%
    ggplot(aes(x = theta, y = p)) + geom_line() + theme_minimal(18)
}





scenario1 = function(men,women){

  mean_men = mean(men)
  mean_women = mean(women)
  mean_diff = mean(women) - mean(men)
  se_diff = t.test(women,men,var.equal = T) %>%
    broom::tidy() %>% mutate(se = abs(estimate1 - estimate2)/statistic) %>% pull(se)
  max.diff = 45 - mean_men
  df = t.test(women,men,var.equal = T) %>%
    broom::tidy() %>% pull(parameter)


  point_null = function(theta.range)
  {
    list(func = function(theta) ifelse(theta == 0, 1, 0),
         theta.range = theta.range)
  }

  uniform_null = function(theta.range)
  {
    list(func = function(theta) dunif(x = theta, min = 0, max = 5),
         theta.range = theta.range)
  }

  h1_1 = function(theta.range)
  {
    list(func = function(theta) dunif(x = theta, min = 0, max = max.diff),
         theta.range = theta.range)
  }

  h1_2 = function(theta.range)
  {
    list(func = function(theta) dnorm(x = theta, mean = 0, sd = (max.diff)/2),
         theta.range = theta.range)
  }

  data_model = scaled.shifted.t.lik(center = mean_diff, scale = se_diff, df = df)


  B1 = (data_model *  h1_1(c(0,max.diff)))$intergral / (data_model * point_null(c(0,0)))$intergral



  point_null.plot = plot_prior(point_null, seq(0,max.diff, .01))
  uniform_null.plot = plot_prior(uniform_null, seq(0,max.diff, .01))
  h1_1.plot = plot_prior(h1_1, seq(0,max.diff, .01))
  h2_1.plot = plot_prior(h1_2, seq(0,max.diff, .01))

  plots1 = (point_null.plot + uniform_null.plot) / (h1_1.plot + h2_1.plot)


  plots2 = plot(data_model, seq(-5,25,0.1))

  print(plots1 + plots2)


  M1_1 = data_model *  h1_1(c(0,max.diff))
  M1_2 = data_model *  h1_2(c(0,max.diff))
  M0_point = data_model *  point_null(c(0,0))
  M0_uniform = data_model *  uniform_null(c(0,5))

  B1 = M1_1$integral / M0_point$integral
  B2 = M1_2$integral / M0_point$integral
  B3 = M1_1$integral / M0_uniform$integral
  B4 = M1_2$integral / M0_uniform$integral
  B5 = M1_1$integral / M1_2$integral

  glue::glue("Comparing H1 (uniform) to H0 (point) gives {round(B1,2)}\n\n") %>% cat()
  glue::glue("Comparing H1 (normal) to H0 (point) gives {round(B2,2)}\n\n") %>% cat()
  glue::glue("Comparing H1 (uniform) to H0 (interval) gives {round(B3,2)}\n\n") %>% cat()
  glue::glue("Comparing H1 (normal) to H0 (interval) gives {round(B4,2)}\n\n") %>% cat()
  glue::glue("Comparing H1 (uniform) to H1 (normal) gives {round(B5,2)}\n\n") %>% cat()

}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
integrate <- function (x, ...) {
  UseMethod("integrate", x)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
integrate.predictive <- function(x){
  x$integral
}

