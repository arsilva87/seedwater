\name{waterloss}
\alias{waterloss}
\docType{data}

\title{
Seed Water Loss Data
}

\description{
Hypothetical data from an experiment containing observations of water content (g/g) of seeds in 
a drying process for 2 hours.
}

\usage{data("onionseeds")}

\format{
  A data frame with 11 observations on the following 2 variables:
  \describe{
    \item{\code{Time}}{a numeric vector containing values of drying time, in hours.}
    \item{\code{WaterContent}}{a numeric vector containing values of water content (g/g) of a seed sample.}
  }
}

\examples{
data(waterloss)
str(waterloss)
summary(waterloss)
plot(WaterContent ~ Time, data = waterloss)
}

\keyword{datasets}
