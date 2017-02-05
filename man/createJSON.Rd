\name{createJSON}
\alias{createJSON}
\title{Utility function for constructing JSON file that encodes the hierarchy of available statistical models in Zelig}
\usage{
createJSON(movefile=TRUE)
}
\arguments{
\item{movefile}{Logical of whether to (TRUE) move the JSON file into path \code{./inst/JSON} or (FALSE) leave in working directory.}
}
\value{
Returns TRUE on successful completion of json file
}
\description{
Utility function for construction a JSON file that encodes the hierarchy of available statistical models.  
}
\author{
Christine Choirat, Vito D'Orazio, James Honaker
}

