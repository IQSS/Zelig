# Proof of concept
zeligw <- function(formula, data, model,..., by=NULL, cite=FALSE) {
  if (model == "ls") {
    zlsloc <- zls
    z5 <- zlsloc$new()
    z5$zelig(formula=formula, data=data, ...)
  }
#   else if (model == "normal") { [...]
#     
#   }
  return(z5)
}

setxw <- function(z5, ...) {
  z5$setx(...)
  return(z5)
}

simw <- function(z5, ...) {
  z5$sim(...)
  return(z5)
}

summaryw <- function(z5) {
  z5$summarize()
}

