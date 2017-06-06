\name{homerun}
\alias{homerun}
\docType{data}

\title{Sample Data on Home Runs Hit By Mark McGwire and Sammy Sosa in 1998.}
\description{
 Game-by-game information for the 1998 season for Mark McGwire and Sammy Sosa. Data are a subset of the dataset provided in Simonoff (1998).
}
\usage{data(homerun)}
\format{
  A data frame containing 5 variables ("gameno", "month", "homeruns", "playerstatus", "player") and 326 observations.  
  \describe{
    \item{\code{gameno}}{an integer variable denoting the game number}
    \item{\code{month}}{a factor variable taking with levels "March" through "September" denoting the month of the game}
    \item{\code{homeruns}}{an integer vector denoting the number of homeruns hit in that game for that player}
    \item{\code{playerstatus}}{an integer vector equal to "0" if the player played in the game, and "1" if they did not.}
    \item{\code{player}}{an  integer vector equal to "0" (McGwire) or "1" (Sosa)}
  }
}

\source{\url{https://ww2.amstat.org/publications/jse/v6n3/datasets.simonoff.html}}

\references{Simonoff, Jeffrey S. 1998. ``Move Over, Roger Maris: Breaking Baseball's Most Famous Record.'' \emph{Journal of Statistics Education} 6(3). Data used are a subset of the data in the article.}

\keyword{datasets}
