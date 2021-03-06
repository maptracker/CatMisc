% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MethodsDocumentation.R
\name{useColor}
\alias{useColor}
\title{Use Color}
\arguments{
\item{newval}{Default NULL. If provided and can be made logical,
will set the flag. Inability to cast as logical will emit a
non-fatal error.}

\item{help}{Default FALSE. If TRUE, show this help and perform no
other actions.}
}
\value{
A single logical value, invisibly
}
\description{
RefClassHelper object method to get/set colorization flag
}
\details{
\preformatted{
## Method Usage:
myObject$useColor( help=TRUE )

myObject$useColor( newval=NULL )
}

Gets or sets the flag determining if messages should be colorized
}
\examples{

rch <- RefClassHelper()
## Show the object:
rch
## Turn off colorization, show the object again:
rch$useColor(FALSE)
rch
}
\seealso{
\link{message}, \link{colorize}, \link{useCol}
}
