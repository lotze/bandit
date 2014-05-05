\name{sim_post}
\alias{sim_post}
\title{sim_post}
\description{Simulate the posterior distribution  the Bayesian probabilities for each arm being the best binomial bandit}

\usage{sim_post(x, n, alpha = 1, beta = 1, ndraws = 5000)}\arguments{
  \item{x}{as in prop.test, a vector of the number of successes}
  \item{n}{as in prop.test, a vector of the number of trials}
  \item{alpha}{shape parameter alpha for the prior beta distribution.}
  \item{beta}{shape parameter beta for the prior beta distribution.}
  \item{ndraws}{number of random draws from the posterior}
}

\author{Thomas Lotze and Markus Loecher}

\examples{

x=c(10,20,30,50)
n=c(100,102,120,130)
sim_post(x,n)

}
