likelihood <- setClass(
  Class = "likelihood",
  slots = list(
    data = "list",
    func = "function",
    marginal = "function",
    desc = "character",
    dist.type = "character",
    plot = "list"
  )
)

# marginal
marginal <- setClass(
  Class = "predictive",
  slots = list(
    data = "list",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta.range = "numeric"
  )
)


prior <- setClass(
  Class = "prior",
  slots = list(
    theta_range = "numeric",
    func = "function",
    type = "character",
    plot = 'list',
    parameters = "list")
)

setClassUnion("bayesplay",c("likelihood","prior"))
