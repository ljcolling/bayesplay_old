likelihood <- setClass(
  Class = "likelihood",
  slots = list(
    data = "list",
    func = "function",
    marginal = "function",
    desc = "character",
    dist.type = "character"
  )
)

# marginal
marginal <- setClass(
  Class = "marginal",
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
    parameters = "list")
)
