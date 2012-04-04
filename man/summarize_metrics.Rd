\name{summarize_metrics}
\alias{summarize_metrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summarize_metrics
}
\description{
A convenience function to perform overall metric analysis: mean, median, CI.
}
\usage{
summarize_metrics(v, successes=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
a vector of values to be analyzed (for nonbinary data), or number of trials (for binary data)
}
  \item{successes}{
number of successes (for binary data)
}
}
\value{
a list with the following items:
\item{mean}{mean}
\item{median}{median}
\item{lower}{0.95 confidence interval on the mean}
\item{upper}{0.95 confidence interval on the mean}
\item{num_obs}{number of observations of this metric}
\item{total}{the sum of all values of this metric (mean*num_obs)}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
metric_list = list(rbinom(n=100,size=1,prob=0.5), rbinom(n=100,size=1,prob=0.7), rpois(n=100, lambda=5))
summarize_metrics(length(metric_list[[1]]), sum(metric_list[[1]]))
summarize_metrics(length(metric_list[[2]]), sum(metric_list[[2]]))
summarize_metrics(metric_list[[3]])
}
\keyword{ htest }
