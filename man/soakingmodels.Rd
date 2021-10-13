\name{soakingmodels}
\alias{soakingmodels}

\title{ Modeling Seed Soaking Kinetics }

\description{
An interactive function to fit nonlinear models for seed soaking kinetics.
}

\usage{
soakingmodels(moisture, time)
}

\arguments{
  \item{moisture}{ a numeric vector containing values of seed water content or weight of seed sample. See Details.}
  \item{time}{ a numeric vector containing values of soaking time.}
}

\details{
\code{soakingmodels} is set to fit curves of \emph{water absorption} (WA\%) rather than \code{moisture}. It is
automatically calculated through the equation: 

\deqn{ WA = \frac{\code{moisture} - Mi}{Mi} }

where \eqn{Mi} is the initial value of seed \code{moisture}.

These are the currently available models:

(Peleg)  \eqn{ WA = x/(k1 + k2 * x) }

(Logistic)  \eqn{ WA = a/(1 + exp(-b1 * (x - c1))) }

(Logistic adapted)  \eqn{ WA = a/(1 + exp(-b1 * (x - c1))) + exp(b2 * (x - c2)) }

(Peleg adapted)  \eqn{ WA = x/(k1 + k2 * x) + exp(b2 * (x - c2)) }

where \eqn{x} represents \code{time} and \eqn{a}, \eqn{k1}, \eqn{k2}, \eqn{b1}, \eqn{b2}, \eqn{c1} and \eqn{c2} are the model parameters. 
Check da Silva et al. (2018) for more details and description of parameters.
}

\value{
An object of class \code{nls} containing the parameter estimates.
}

\section{Side Effects }{
An interactive graphic is displayed for selecting the model and the initial values for the parameters.
}

\references{
da Silva, A. R. et al. (2018) Modeling the Three Phases of the Soaking Kinetics of Seeds. 
	\emph{Agronomy Journal}, 110:164-170. doi:10.2134/agronj2017.07.0373
}

\author{
Anderson Rodrigo da Silva <anderson.silva@ifgoiano.edu.br>
}

\seealso{
\code{\link{nls}}, \code{\link{dryingmodels}}
}

\examples{
data(onionseeds)
if (interactive()) {
   res <- with(onionseeds, soakingmodels(moisture = Weight, time = Time))
   summary(res)
}

# End (not run)
}
