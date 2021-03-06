% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xFormula.R
\name{xFormula}
\alias{xFormula}
\title{Quickly Create Formula Objects}
\usage{
xFormula(y = NULL, x = NULL)
}
\arguments{
\item{y}{Left-hand side of desired formula provided as a character vector or left null}

\item{x}{Right-hand side of desired formula provided as a character vector or left null}
}
\value{
An object of class formula
}
\description{
A function that takes left-hand vars and right-hand vars as character strings
and returns and R formula object.
}
\examples{
Y <- c("A", "B", "C")
X <- c("D", "E", "F")
xFormula(Y, X)
}
