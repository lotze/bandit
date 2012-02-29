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
distribution_estimate(v, data_is_binary=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
a vector of values to be analyzed
}
  \item{data_is_binary}{
whether or not this is binary data
}
}
\value{
a data frame with the following columns:
\item{quantiles}{the estimated quantiles for 0,0.01,0.02,...,1, using a Beta-binomial estimate of p for binomial data, an empirical quantile distribution for real-valued numbers}
\item{x}{x values for plotting a lineplot of the estimated distribution}
\item{y}{y values for plotting a lineplot of the estimated distribution}
\item{mids}{mid values for plotting a barplot of the estimated distribution}
\item{widths}{width values for plotting a barplot of the estimated distribution}
\item{heights}{height values for plotting a barplot of the estimated distribution}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
metric_list = list(rbinom(n=100,size=1,prob=0.5), rbinom(n=100,size=1,prob=0.7))
sapply(metric_list,distribution_estimate)
}
\keyword{ htest }
