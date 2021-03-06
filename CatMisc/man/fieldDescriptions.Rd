% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MethodsDocumentation.R
\name{fieldDescriptions}
\alias{fieldDescriptions}
\title{Field Descriptions}
\arguments{
\item{help}{Default FALSE. If TRUE, show this help and perform no
other actions.}
}
\value{
A list of character vectors, each a single entry long, each
(ideally) being a brief description of the field.
}
\description{
Get brief descriptions for all RefClass fields used by the object
}
\details{
\preformatted{
## Method Usage:
myObject$fieldDescriptions( help=TRUE )

myObject$fieldDescriptions( )
}

This method simply returns a list of descriptive text for each
Reference Class field defined for the object. It is intended to
provide self-documenting information to the \link{help} function.

If the object inherits other RefClass classes, they will be
recursively queried for descriptive text associated in their own
\code{fieldDescriptions} structures. This recursion is managed by
\link{getFieldDescriptions}.
}
\examples{

el <- RefClassHelper()
el$fieldDescriptions()
el$help() # Descriptive text will be at bottom
}
\seealso{
\link{getFieldDescriptions}, \link{help},
\link{helpSections}
}
