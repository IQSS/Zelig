data(coalition)

z5 <- zweibull$new()
z5
z5$zelig(Surv(duration, ciep12) ~ fract + numst2, data = coalition)
z5
z5$setx() # works
z5$setx(numst2 = 0)

z.out <- z5$zelig.out$z.out[[1]]
z.out

# z5$setx(numst2 = 0) # fails
# Error in terms(lm(formula, data), "predvars") : 
#   error in evaluating the argument 'x' in selecting a method for function 'terms':
#   Error in Ops.Surv(y, z$residuals) : Invalid operation on a survival time
z5$sim()
z5
z5$graph()
