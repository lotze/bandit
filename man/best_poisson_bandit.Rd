\name{best_poisson_bandit}
\alias{best_poisson_bandit}
\alias{bpb}
\title{
best_poisson_bandit
}
\description{
Compute the Bayesian probabilities for each arm being the best poisson bandit.
}
\usage{
best_poisson_bandit(x, n = NULL)
}
\arguments{
  \item{x}{
as in prop.test, a vector of the number of successes; it may alternatively be a list of vectors of the results of each trial, if n is not provided
}
  \item{n}{
as in prop.test, a vector of the number of trials; if it is not provided, x must be a list of vectors of the results of each trial
}
}
\value{
a vector of probabilities for each arm being the best poisson bandit; this can be used for future randomized allocation
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}

\seealso{
\code{\link{prop.test}}
}
\references{
Steven L. Scott, A modern Bayesian look at the multi-armed bandit, Appl. Stochastic Models Bus. Ind. 2010; 26:639-658. (\url{http://www.economics.uci.edu/~ivan/asmb.874.pdf})
}
\examples{
p1 = rpois(100, lambda=10)
p2 = rpois(100, lambda=9)
x = sapply(list(p1, p2), sum)
n = sapply(list(p1, p2), length)
best_poisson_bandit(x,n)
}
\keyword{ htest }
\keyword{ design }
