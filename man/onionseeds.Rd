\name{onionseeds}
\alias{onionseeds}
\docType{data}

\title{
Onion Seed Soaking Kinetics Data
}

\description{
Data from a lab experiment containing observations of onion seed sample weight (g) subjected to
a soaking process for 96 hours.
}

\usage{data("onionseeds")}

\format{
  A data frame with 23 observations on the following 2 variables:
  \describe{
    \item{\code{Time}}{a numeric vector containing values of soaking time, in hours.}
    \item{\code{Weight}}{a numeric vector containing values of weight (g) of a onion seed sample.}
  }
}

\examples{
data(onionseeds)
str(onionseeds)
summary(onionseeds)
plot(Weight ~ Time, data = onionseeds)
}

\keyword{datasets}
