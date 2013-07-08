\name{deseasonalized_trend}
\alias{deseasonalized_trend}
\title{
deseasonalized_trend
}
\description{
A convenience function to analyze a timeseries and return an estimate (via gam, using day of week factors and smoothed timestamp) of whether, after accounting for day-of-week, there is a significant time-based influence and what that influence is.
}
\usage{
deseasonalized_trend(df, w=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame containing timestamp and value entries
}
  \item{w}{
number of attempts (n for binomial data)
}
}
\value{
a list with the following items:
\item{pval}{pval given by anova on gam, to indicate whether s(timestamp) is significant}
\item{smoothed_prediction}{a smoothed prediction over time (on Wednesdays), to give a human-understandable idea of what the change over time has been}
}
\author{
Thomas Lotze <thomaslotze@thomaslotze.com>
}
\examples{
timestamps = as.numeric(as.POSIXct(seq(as.Date("2012-01-01"),as.Date("2012-05-03"),by=1)))
df=data.frame(timestamp = timestamps, value = rnorm(length(timestamps)))
dt = deseasonalized_trend(df)
if (dt$pval < 0.01) {
  print("Significant time-based factor")
  plot(df$timestamp, dt$smoothed_prediction)
} else {
  print("No significant time-based factor")
}

df=data.frame(timestamp = timestamps,
              value = sapply(timestamps, function(t) {rpois(1, lambda=t-min(timestamps))}))
dt = deseasonalized_trend(df)
if (dt$pval < 0.01) {
  print("Significant time-based factor")
  plot(df$timestamp, dt$smoothed_prediction)
} else {
  print("No significant time-based factor")
}

}
\keyword{ htest }
