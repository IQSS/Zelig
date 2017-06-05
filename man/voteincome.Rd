\name{voteincome}
\alias{voteincome}
\docType{data}

\title{Sample Turnout and Demographic Data from the 2000 Current Population Survey}
\description{
 This data set contains turnout and demographic data from a sample of respondents to the 2000 Current Population Survey (CPS). The states represented are South Carolina and Arkansas. The data represent only a sample and results from this example should not be used in publication. 
}
\usage{data(voteincome)}
\format{
  A data frame containing 7 variables ("state", "year", "vote", "income", "education", "age", "female") and 1500 observations.  
  \describe{
    \item{\code{state}}{a factor variable with levels equal to "AR" (Arkansas) and "SC" (South Carolina)}
    \item{\code{year}}{an integer vector}
    \item{\code{vote}}{an integer vector taking on values "1" (Voted) and "0" (Did Not Vote)}
    \item{\code{income}}{an integer vector ranging from "4" (Less than \$5000) to "17" (Greater than \$75000) denoting family income. See the CPS codebook for more information on variable coding}
    \item{\code{education}}{an  integer vector ranging from "1" (Less than High School Education) to "4" (More than a College Education). See the CPS codebook for more information on variable coding}
    \item{\code{age}}{an integer vector ranging from "18" to "85"}
    \item{\code{female}}{an integer vector taking on values "1" (Female) and "0" (Male)}
}
}

\source{Census Bureau Current Population Survey}

\references{\url{http://www.census.gov/cps}}

\keyword{datasets}