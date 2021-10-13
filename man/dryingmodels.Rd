\name{dryingmodels}
\alias{dryingmodels}

\title{ Modeling Seed Drying Kinetics }

\description{
An interactive function to fit nonlinear models for seed drying kinetics.
}

\usage{
dryingmodels(moisture, time)
}

\arguments{
  \item{moisture}{ a numeric vector containing values of seed water content of weight of seed sample. See Details.}
  \item{time}{ a numeric vector containing values of drying time.}
}

\details{
\code{dryingmodels} is set to fit curves of \emph{moisture ratio} (MR) rather than \code{moisture}. It is
automatically calculated through the equation: 

\deqn{ MR = \frac{\code{moisture} - Mf}{Mi - Mf} }

where \eqn{Mi} and \eqn{Mf} are the initial and final values of seed \code{moisture}.

These are the currently available models:

(Page)  \eqn{ MR = exp(-K * x^n) }

(Henderson and Pabis)  \eqn{ MR = A * exp(-K * x) }

(Henderson and Pabis modified)  \eqn{ MR = A * exp(-K * x) + b * exp(-K0 * x) + exp(-K1 * x) }

(Midilli)  \eqn{ MR = A * exp(-K * x^n) + b*x }

(Diffusion approximation)  \eqn{ MR = A * exp(-K * x) + (1 - A) * exp(-K * b * x) }

(Two terms exponential 1)  \eqn{ MR = A * exp(-K0 * x) + b * exp(-K1 * x) }

(Two terms exponential 2)  \eqn{ MR = A * exp(-K * x) + (1 - A) * exp(-K * A * x) }

(Logarithmic)  \eqn{ MR = A * exp(-K * x) + b }

(Thompson)  \eqn{ MR = exp(-A - sqrt(A^2 + 4 * b * x))/(2 * b) }

(Newton)  \eqn{ MR = exp(-K * x)}

(Verma)  \eqn{ MR = A * exp(-K * x) + (1 - A) * exp(-K1 * x)}

(Wang and Sing)  \eqn{ MR = 1 + A * x + b * x^2}

where \eqn{x} represents \code{time} and \eqn{A}, \eqn{K}, \eqn{n}, \eqn{b}, \eqn{K0} and \eqn{K1} are the model parameters. 
}

\value{
An object of class \code{nls} containing the parameter estimates.
}

\section{Side Effects }{
An interactive graphic is displayed for selecting the model and the initial values for the parameters.
}

\author{
Anderson Rodrigo da Silva <anderson.silva@ifgoiano.edu.br>
}

\seealso{
\code{\link{nls}}, \code{\link{soakingmodels}}
}

\examples{
data(waterloss)
if (interactive()) {
   res <- with(waterloss, dryingmodels(moisture = WaterContent, time = Time))
   summary(res)
}

# End (not run)
}
