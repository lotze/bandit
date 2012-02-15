\name{best_binomial_bandit}
\alias{best_binomial_bandit}
\alias{bbb}
\title{
best_binomial_bandit
}
\description{
Compute the Bayesian probabilities for each arm being the best binomial bandit.
}
\usage{
best_binomial_bandit(x, n)
}
\arguments{
  \item{x}{
as in prop.test, a vector of the number of successes
}
  \item{n}{
as in prop.test, a vector of the number of trials
}
}
\value{
a vector of probabilities for each arm being the best binomial bandit; this can be used for future randomized allocation
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
x=c(10,20,30,50)
n=c(100,102,120,130)
best_binomial_bandit(x,n)
}
\keyword{ htest }
\keyword{ design }
