\name{distribution_estimate}
\alias{distribution_estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summarize_metrics
}
\description{
A convenience function to perform overall metric analysis: mean, median, CI.
}
\usage{
distribution_estimate(metric_list, data_is_binary=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metric_list}{
a list of metric values, one for each assignment to be analyzed
}
  \item{data_is_binary}{
whether or not this is binary data
}
}
\value{
a matrix with a row for each metric in the metric list, giving the estimated cumulative distribution for the parameter of interest (a Beta-binomial estimate of p for binomial data, an empirical quantile distribution for real-valued numbers)
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
metric_list = list(rbinom(n=100,size=1,prob=0.5), rbinom(n=100,size=1,prob=0.7))
distribution_estimate(metric_list)
}
\keyword{ htest }
