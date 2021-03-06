% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MethodsDocumentation.R
\name{varCheck}
\alias{varCheck}
\title{RefClassHelper Variable Name Last Checked}
\description{
Internal RefClass field tracking the last time the object name was checked
}
\details{
The \link{varName} field caches the variable name of the
object. Finding this value sometimes fails (not sure why). It is
generally possible to eventually recover the name. The
\code{varCheck} field tracks the last attempt to get the name, and
restricts retries to once every 10 seconds.  function.
}
\examples{

rch <- RefClassHelper( )
rch$varCheck # Not attempted yet
rch # Pretty-print the object, which will attempt to discover its name
Sys.time()
rch$varCheck # Should be close to the time above
}
\seealso{
\link{varName}, \link{.selfVarName}, \link{help},
\link{getFieldDescriptions}
}
