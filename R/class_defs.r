likelihood <- setClass(
  "likelihood",
  slots = list(
    data = "list",
    func = "function",
    desc = "character",
    dist.type = "character"
  )
)

# marginal
marginal <- setClass(
  "marginal",
  slots = list(
    data = "list",
    K = "numeric",
    lik = "function",
    prior = "function",
    theta.range = "numeric"
  )
)
