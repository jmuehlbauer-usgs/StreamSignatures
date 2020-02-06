<<<<<<< HEAD
loglik <- function(parms){
	llik <- function(parms){
		pred <- parms[1] * exp(parms[2] * bolz$Dist)
		-1 * sum(dnorm(bolz$AbBiom, pred, exp(parms[3]), log = TRUE))
	}
	optim(parms, llik, method = 'BFGS')
}

predvals <- loglik(parms = c(120,-.06,5))
	## parms (parameters) are, in order, the intercept, distance coefficient, and standard deviation

plot(bolz$AbBiom ~ bolz$Dist)
curve(predvals$par[1] * exp(predvals$par[2] * x), add=TRUE)

=======
loglik <- function(parms){
	llik <- function(parms){
		pred <- parms[1] * exp(parms[2] * bolz$Dist)
		-1 * sum(dnorm(bolz$AbBiom, pred, exp(parms[3]), log = TRUE))
	}
	optim(parms, llik, method = 'BFGS')
}

predvals <- loglik(parms = c(120,-.06,5))
	## parms (parameters) are, in order, the intercept, distance coefficient, and standard deviation

plot(bolz$AbBiom ~ bolz$Dist)
curve(predvals$par[1] * exp(predvals$par[2] * x), add=TRUE)

>>>>>>> f0b3faa2aca91082a0e97ded9e797b2766ecc202
