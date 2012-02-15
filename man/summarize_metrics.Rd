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
summarize_metrics(metric_list, data_is_binary)
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
a data frame with the following columns:
\item{mean}{mean}
\item{median}{median}
\item{lower}{0.95 confidence interval on this metric}
\item{upper}{0.95 confidence interval on this metric}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
metric_list = list(rbinom(n=100,size=1,prob=0.5), rbinom(n=100,size=1,prob=0.7))
summarize_metrics(metric_list)
}
\keyword{ htest }
