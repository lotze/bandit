\name{value_remaining}
\title{value_remaining}
\alias{value_remaining}
\description{Compute the "value_remaining" in the binomial bandits}

\usage{value_remaining(x, n, alpha = 1, beta = 1, ndraws = 10000)}\arguments{
  \item{x}{as in prop.test, a vector of the number of successes}
  \item{n}{as in prop.test, a vector of the number of trials}
  \item{alpha}{shape parameter alpha for the prior beta distribution.}
  \item{beta}{shape parameter beta for the prior beta distribution.}
  \item{ndraws}{number of random draws from the posterior}
}

\value{value_remaining distribution; the distribution of improvement amounts that another arm might
have over the current best arm}

\author{Thomas Lotze and Markus Loecher}

\references{\url{https://support.google.com/analytics/answer/2846882?hl=en&topic=2844866&rd=1}}

\examples{

x=c(10,20,30,80)
n=c(100,102,120,240)
vr = value_remaining(x, n)
hist(vr)
best_arm = which.max(best_binomial_bandit(x, n))
# "potential value" remaining in the experiment
potential_value = quantile(vr, 0.95)
paste("Were still unsure about the CvR for the best arm (arm ", best_arm,
	"), but whatever it is, one of the other arms might beat it by as much as ",
	round(potential_value*100, 4), " percent.", sep="")
}
