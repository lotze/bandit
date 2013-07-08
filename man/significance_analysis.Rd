\name{significance_analysis}
\alias{significance_analysis}
\title{
significance_analysis
}
\description{
A convenience function to perform overall proportion comparison using prop.test, before doing pairwise comparisons, to see what outcomes seem to be better than others.
}
\usage{
significance_analysis(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
as in prop.test, a vector of the number of successes
}
  \item{n}{
as in prop.test, a vector of the number of trials
}
}
\value{
a data frame with the following columns:
\item{successes}{x}
\item{totals}{n}
\item{estimated_proportion}{x/n}
\item{lower}{0.95 confidence interval on the estimated amount by which this alternative outperforms the next-lower alternative}
\item{upper}{0.95 confidence interval on the estimated amount by which this alternative outperforms the next-lower alternative}
\item{significance}{p-value for the test that this alternative outperforms the next-lower alternative}
\item{order}{order, by highest success proportion}
\item{best}{1 if it is part of the 'highest performing group' -- those groups which were not significantly different from the best group}
\item{p_best}{Bayesian posterior probability that this alternative is the best binomial bandit}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\note{
This is intended for use in A/B split testing -- so sizes of n should be roughly equal.
Also, note that alternatives which have the same rank are grouped together for analysis with the 'next-lower' alternative, so you may want to check to see if ranks are equal.
}

\seealso{
\code{\link{prop.test}}
}
\examples{
x = c(10,20,30,50)
n = c(100,102,120,130)
sa = significance_analysis(x,n)
sa[rev(order(sa$estimated_proportion)), ]

x = c(37,41,30,43,39,30,31,35,50,30)
n = rep(50, length(x))
sa = significance_analysis(x,n)
sa[rev(order(sa$estimated_proportion)), ]

x = c(37,41,30,43,39,30,31,37,50,30)
n = rep(50, length(x))
sa = significance_analysis(x,n)
sa[rev(order(sa$estimated_proportion)), ]
}
\keyword{ htest }
\keyword{ design }
