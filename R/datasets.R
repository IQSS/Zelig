#' Cigarette Consumption Panel Data
#'
#' @docType data
#' @source From Christian Kleiber and Achim Zeileis (2008). Applied
#' Econometrics with R. New York: Springer-Verlag. ISBN 978-0-387-77316-2. URL
#' <https://CRAN.R-project.org/package=AER>
#' @keywords datasets
#' @md
#' @format A data set with 96 observations and 9 variables
#' @name CigarettesSW
NULL


#' U.S. Supreme Court Vote Matrix
#'
#' This dataframe contains a matrix votes cast by U.S. Supreme Court
#' justices in all cases in the 2000 term.
#' 
#' @docType data
#' @source Harold J. Spaeth (2005). ``Original United States Supreme
#' Court Database:  1953-2004 Terms.'' <http://scdb.wustl.edu/data.php>
#' @keywords datasets
#' @md
#' @format The dataframe has contains data for justices Rehnquist,
#' Stevens, O'Connor, Scalia, Kennedy, Souter, Thomas, Ginsburg, and Breyer
#' for the 2000 term of the U.S. Supreme Court.  It contains data
#' from 43 non-unanimous cases. The votes are coded liberal (1) and
#' conservative (0) using the protocol of Spaeth (2003).   The unit
#' of analysis is the case citation (ANALU=0).  We are concerned with
#' formally decided cases issued with written opinions, after full
#' oral argument and cases decided by an equally divided vote
#' (DECTYPE=1,5,6,7).
#' @name SupremeCourt
NULL


#' Political Economic Risk Data from 62 Countries in 1987.
#'
#' @name PErisk
#'
#' @docType data
#'
#' @format A data frame with 62 observations on the following 9 variables. All
#' data points are from 1987. See Quinn (2004) for more details.
#' \describe{
#'   \item{country}{a factor with levels \code{Argentina} through
#'     \code{Zimbabwe}} \item{courts}{an ordered factor with levels \code{0} <
#'     \code{1}.\code{courts} is an indicator of whether the country in question is
#'     judged to have an independent judiciary. From Henisz (2002).}
#'   \item{barb2}{a numeric vector giving the natural log of the black market
#'     premium in each country. The black market premium is coded as the black market
#'     exchange rate (local currency per dollar) divided by the official exchange rate
#'     minus 1. From Marshall, Gurr, and Harff (2002). }
#'   \item{prsexp2}{an ordered factor
#'     with levels \code{0} < \code{1} < \code{2} < \code{3} < \code{4} < \code{5},
#'     giving the lack of expropriation risk. From Marshall, Gurr, and Harff
#'     (2002).}
#'   \item{prscorr2}{an ordered factor with levels \code{0} < \code{1} <
#'     \code{2} < \code{3} < \code{4} < \code{5}, measuring the lack of corruption.
#'     From Marshall, Gurr, and Harff (2002).}
#'   \item{gdpw2}{a numeric vector giving the natural log of real GDP per worker in
#'    1985 international prices. From Alvarez et al. (1999).}
#' }
#'
#' @references Kevin M. Quinn. 2004. ``Bayesian Factor Analysis for Mixed
#' Ordinal and Continuous Response.'' \emph{Political Analyis}. 12: 338-353.
#'
#' @source Mike Alvarez, Jose Antonio Cheibub, Fernando Limongi, and Adam
#' Przeworski. 1999. ``ACLP Political and Economic Database.''
#' \url{https://sites.google.com/site/joseantoniocheibub/datasets/aclp}.
#'
#' Witold J. Henisz. 2002. ``The Political Constraint Index (POLCON) Dataset.''
#'
#' Monty G. Marshall, Ted Robert Gurr, and Barbara Harff. 2002. ``State Failure
#' Task Force Problem Set.''
#'
#' @keywords datasets
NULL

#' 1932 Weimar election data
#'
#' @docType data
#' @source ICPSR
#' @keywords datasets
#' @md
#' @format A table containing 11 variables and 10 observations. The variables are
#' \describe{
#' \item{Nazi}{Number of votes for the Nazi party}
#' \item{Government}{Number of votes for the Government}
#' \item{Communists}{Number of votes for the Communist party}
#' \item{FarRight}{Number of votes for far right parties}
#' \item{Other}{Number of votes for other parties, and non-voters}
#' \item{shareunemployed}{Proportion unemployed}
#' \item{shareblue}{Proportion working class}
#' \item{sharewhite}{Proportion white-collar workers}
#' \item{sharedomestic}{Proportion domestic servants}
#' \item{shareprotestants}{Proportion Protestant}
#' }
#' @name Weimar
NULL

#' U.S. Presidential Approval Data
#'
#' @docType data
#' @source ICPSR
#' @keywords datasets
#' @md
#' @format A table containing 8 variables ("month", "year", "approve",
#' "disapprove", "unsure", "sept.oct.2001", "iraq.war", and
#' "avg.price") and 65 observations.
#' @name approval
NULL

#' Sample data for bivariate probit regression
#'
#' Sample data for bivariate probit regression
#'
#' @docType data
#' @source This is a cleaned and relabelled version of the sanction
#' data set, available in Zelig.
#' @keywords datasets
#' @format A table containing 6 variables ("y1", "y2", "x1", "x2", "x3", and "x4") and 78 observations.
#' @name bivariate
#' @references Martin, Lisa (1992).  \emph{Coercive Cooperation:
#' Explaining Multilateral Economic Sanctions}, Princeton: Princeton University Press.
NULL

#' Coalition Dissolution in Parliamentary Democracies
#'
#' @description This data set contains survival data on government
#' coalitions in parliamentary democracies (Belgium, Canada, Denmark, 
#' Finland, France, Iceland, Ireland, Israel, Italy, Netherlands,
#' Norway, Portugal, Spain, Sweden, and the United Kingdom) for the
#' period 1945-1987. For parsimony, country indicator variables are
#' omitted in the sample data.
#' @docType data
#' @source ICPSR
#' @keywords datasets
#' @format A table containing 7 variables ("duration", "ciep12",
#' "invest", "fract", "polar", "numst2", "crisis") and 314
#' observations.  For variable descriptions, please refer to King,
#' Alt, Burns and Laver (1990).  
#' @name coalition
NULL

#' Coalition Dissolution in Parliamentary Democracies, Modified Version
#'
#' @description This data set contains survival data on government
#' coalitions in parliamentary democracies (Belgium, Canada, Denmark, 
#' Finland, France, Iceland, Ireland, Israel, Italy, Netherlands,
#' Norway, Portugal, Spain, Sweden, and the United Kingdom) for the
#' period 1945-1987. Country indicator variables are included in the
#' sample data. 
#' @docType data
#' @source ICPSR
#' @keywords datasets
#' @format A table containing 8 variables ("duration", "ciep12",
#' "invest", "fract", "polar", "numst2", "crisis", "country") and 314 
#' observations.  For variable descriptions, please refer to King,
#' Alt, Burns and Laver (1990).  
#' @name coalition2
NULL


