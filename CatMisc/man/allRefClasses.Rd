% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RefClassHelper.R
\name{allRefClasses}
\alias{allRefClasses}
\title{All Reference Classes}
\usage{
allRefClasses(obj, standardClasses = FALSE, struct = NA)
}
\arguments{
\item{obj}{Required, the object to be tested. This can be either a
class name, a class generator structure, or a RefClass object
itself.}

\item{standardClasses}{Default FALSE. If TRUE then the R-internal
utility classes (eg \code{envRefClass}) will be included in the
output.}

\item{struct}{Default NA. Used internally for recursion}
}
\description{
Return class representations for object and all inherited classes
}
\details{
Recursively follows 'contains' (inherited) reference classes to
provide an exhaustive list of all reference classes associated
with the provided object
}
