% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CatMisc.R
\name{is.something}
\alias{is.something}
\title{Is Something}
\usage{
is.something(x, unclass = FALSE)
}
\arguments{
\item{x}{The object to be tested}

\item{unclass}{Default \code{FALSE}. If true, then the query will
be \code{unclass}ed before inspection}
}
\value{
TRUE if x is "defined" (\link{is.def}) and is neither
    (numeric) zero nor an empty string.
}
\description{
Check if something is defined, and not zero or an empty string
}
\details{
Primarily designed to deal with parameter checking for
user-supplied arguments
}
\examples{

is.something("")
is.something(0)
is.something(c(0,0)) # -> TRUE

}
