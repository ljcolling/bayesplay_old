setMethod("show",
          "likelihood",
          function(object) {
            cat("Object of class", class(object), "\n")
            cat("Likelihood type:", object@data$likelihood_type, "\n")
            cat(object@desc)
          })

setMethod("show",
          "marginal",
          function(object) {
            cat("Object of class", class(object), "\n")
            cat("Parameter range: from ",
                object@theta.range[1],
                " to ",
                object@theta.range[2],
                "\n")
            cat("Area under the curve (integral):", object@data$integral, "\n")
            cat("Prior function: ", toString(attributes(object@prior)$srcref), "\n")
            cat("Likelihood function: ", toString(attributes(object@lik)$srcref), "\n")
          })


setMethod("show",
          "prior",
          function(object) {
            cat("Object of class", class(object),"\n")
            cat("Range of support: from ",
                   object@theta_range[1],
                   " to ",
                   object@theta_range[2],
                   "\n")
            cat("Prior type: ", object@type, "\n")
            cat("Prior function: ", toString(attributes(object@func)$srcref), "\n")

          })


# $ methods
setMethod("$", signature = "likelihood",
          function(x, name) {
            returnval = x@data[[name]]
            return(returnval)
          })

setMethod("names", signature = "likelihood",
          function(x) {
            return(names(x@data))
          })


setMethod("$", signature = "marginal",
          function(x, name) {
            returnval = x@data[[name]]
            return(returnval)
          })

setMethod("names", signature = "marginal",
          function(x) {
            return(names(x@data))
          })
