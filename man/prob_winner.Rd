\name{prob_winner}
\alias{prob_winner}
\title{prob_winner}
\description{Function to compute probability that each arm is the winner, given simulated posterior results}

\usage{prob_winner(post)}\arguments{
  \item{post}{
    the simulated results from the posterior, provided by sim_post
  }
}

\author{Thomas Lotze and Markus Loecher}

\examples{

x=c(10,20,30,50)
n=c(100,102,120,130)
betaPost = sim_post(x,n)
prob_winner(betaPost)

}
