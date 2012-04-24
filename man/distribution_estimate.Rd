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
distribution_estimate(v, successes=NULL, num_quantiles=101, observed=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
a vector of values to be analyzed (for nonbinary data), or number of trials (for binary data)
}
  \item{successes}{
number of successes (for binary data)
}
  \item{num_quantiles}{
number of quantiles to split into
}
  \item{observed}{
whether to generate the observed distribution (rather than the estimated distribution of the mean); default FALSE
}
}
\value{
a data frame with the following columns:
\item{quantiles}{the estimated quantiles (0,0.01,0.02,...,1) for the mean, using a Beta-binomial estimate of p for binomial data, a bootstrapped quantile distribution for real-valued numbers}
\item{x}{x values for plotting a lineplot of the estimated distribution}
\item{y}{y values for plotting a lineplot of the estimated distribution}
\item{mids}{mid values for plotting a barplot of the estimated distribution}
\item{lefts}{left values for plotting a barplot of the estimated distribution}
\item{rights}{right values for plotting a barplot of the estimated distribution}
\item{widths}{width values for plotting a barplot of the estimated distribution}
\item{heights}{height values for plotting a barplot of the estimated distribution}
\item{probabilities}{probabilities indicating how much probability is contained in each barplot}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
metric_list = list(rbinom(n=100,size=1,prob=0.5), rbinom(n=100,size=1,prob=0.7), rpois(n=100, lambda=5))
distribution_estimate(length(metric_list[[1]]), sum(metric_list[[1]]))
distribution_estimate(length(metric_list[[2]]), sum(metric_list[[2]]))
distribution_estimate(metric_list[[3]])
distribution_estimate(metric_list[[3]], observed=TRUE)
}
\keyword{ htest }
