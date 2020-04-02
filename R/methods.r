setMethod("show",
          "likelihood",
          function(object) {
            cat("Object of class", class(object), "\n")
            cat("Likelihood type:", object@data$lik.type, "\n")
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
