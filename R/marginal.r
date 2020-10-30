

#' Title
#'
#' @param likelihood
#' @param prior
#' @param new_obs
#'
#' @return
#' @export
#'
#' @examples
marginal<- function(likelihood,prior, new_obs){

  if(likelihood$likelihood_type == "binomial"){
    data_model = paste0("likelihood('binomial', successes = new_obs, trials = ",
                        likelihood$parameters$trials,")")

    data_models = pmap(list(new_obs = new_obs),
                       function(new_obs) eval(parse(text = data_model)))
    return(purrr::map_df(data_models, function(x)
      tibble::tibble(successes = x$parameters$successes,
                     y = integrate(x * prior))))
  }


}

