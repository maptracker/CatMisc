% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MethodsDocumentation.R
\name{help}
\alias{help}
\title{Object Help}
\arguments{
\item{genericName}{Default 'myObject'. Used if R can not determine
the actual variable name of the object (via \link{.selfVarName})}

\item{color}{Default \code{NULL}, which will use the value defined
by \link{useColor}. If TRUE, then output will be colorized.}

\item{generic}{Default \code{FALSE}. If TRUE, then generic
Reference Class methods common to all RefClass objects will be
included.}

\item{help}{Default FALSE. If TRUE, show this help and perform no
other actions.}
}
\value{
NULL, invisibly
}
\description{
Summarize object methods, fields and commands to get additional help
}
\details{
\preformatted{
## Method Usage:
myObject$help( help=TRUE )

myObject$help( genericName='myObject', color=NULL, generic=FALSE )
}

Shows help for the object. This method attempts to both be compact
and exhaustive. Each object method will be shown, as well as a
one-line description of its function (if it has been defined in the
code). Additionally, the method call using the help flag will be
shown explicitly with the actual object variable. That is, if
\code{$help()} is called on an object named \code{foo} that has a
method named \code{soak}, then the reported text will include:

\code{foo$soak( help=TRUE )}

This is to aid rapid copy-paste exploration of the object.

Object fields will also be reported, along with their descriptive
text.
}
\examples{

rch <- RefClassHelper()
## Report help for the object
rch$help()
rch$help( generic=TRUE ) # Include generic methods
}
\seealso{
\link{getFieldDescriptions}, \link{getHelpSections},
\link{show}
}
