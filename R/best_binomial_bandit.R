# estimate the Bayesian posterior probability of each alternative being the best binomial bandit
best_binomial_bandit <-
function(x, n) {
	k <- length(x)
	ans <- numeric(k)
	for (i in (1:k)) {
		indx <- (1:k)[-i]
		f <- function(z) {
			r <- dbeta(z, x[i]+1, n[i]-x[i]+1)
			for (j in indx) {
				r <- r * pbeta(z, x[j]+1, n[j]-x[j]+1)
			}
			return(r)
		}
		ans[i] = integrate(f,0,1)$value
	}
	return(ans)
}

bbb <-
function(x, n) {
	best_binomial_bandit(x,n)
}
# functions for computing resulting optimal probabilities via simulation
# sim.post <-
# function(y, n, ndraws) {
# 	k <- length(y)
# 	ans <- matrix(nrow=ndraws, ncol=k)
# 	no <- n - y
# 	for (i in 1:k) {
# 		ans[,i] <- rbeta(ndraws, y[i]+1, no[i]+1)
# 	}
# 	return(ans)
# }
# prob.winner <-
# function(post) {
# 	k <- ncol(post)
# 	w <- table(factor(max.col(post), levels=1:k))
# 	return(w/sum(w))
# }
# compute.win.prob <-
# function(y, n, ndraws) {
# 	return(prob.winner(sim.post(y, n, ndraws)))
# }

