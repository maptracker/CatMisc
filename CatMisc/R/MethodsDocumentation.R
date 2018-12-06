## ROxygen provides only rudimentary support for documenting Reference
## Class object methods (as of 2017). It will recognized an unassigned
## character string at the beginning of the method function as a
## description, and will parse the parameters. This class is complex
## enough that I wanted more formalized documentation. The blocks
## below define help topics for both RefClass fields and methods:

## * The method name is being put in @name and/or @aliases. This will
##   likely be interpreted by the help system as a simple function in the
##   namespace, but I don't think that will cause problems. It allows
##   \link{}s to be built between topics.
## * I am also specifying the methods under the @method section. This
##   will likely be perceived as an S3/S4 function in the class. I don't
##   _think_ that's an issue, but it might be.
## * It's effectively impossible to use the @usage section, since R_CMD
##   check becomes very unhappy with attempts to formalize usage in
##   'true' object fasion. Instead, usage is manually smuggled into the
##   @details section in a \preformatted{} block.

#' RefClassHelper Use Color Flag
#'
#' Internal RefClass field indicating if output should be colorized
#'
#' @name useCol
#'
#' @details
#'
#' A logical flag, if TRUE it indicates that messages (from compliant
#' methods) should be colorized with the \link[crayon]{crayon}
#' package.
#'
#' \preformatted{
#' ## NORMALLY YOU WILL NOT WANT TO ACCESS THIS FIELD DIRECTLY
#' ## Instead, use the \link{useColor} method to check and alter the value
#' }
#'
#' @return A single logical value
#'
#' @seealso \link{useColor}, \link{message}
#'
#' @examples
#' 
#' rch <- RefClassHelper( )
#' message(rch$colorize("I'm feeling blue", "blue"))
#' rch$useColor(FALSE)
#' message(rch$colorize("I'm still down, I'm just not showing it", "blue"))
NULL

#' Map Color
#'
#' RefClassHelper object method to turn a string into a crayon color function
#'
#' @name colorMap
#' @method colorMap RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$colorMap( help=TRUE )
#' 
#' myObject$colorMap( color, bg=FALSE )
#' }
#'
#' Takes a color name as input, returns a \link[crayon]{crayon}
#' function. This is primarily a utility function.
#' 
#' @param color Required, a string describing a color, eg
#'     "magenta". Can also be a function reference
#' @param bg Default FALSE. If TRUE, will pick the background color
#'     corresponding to the name.
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A function from the \link[crayon]{crayon}
#'
#' @seealso \link{colorize}, \link[crayon]{crayon},
#'     \link{colorNameToFunc}
#'
#' @examples
#'
#' el <- RefClassHelper()
#' f <- el$colorMap("red")
#' base::message(f("Quick Brown Fox"))
#' f <- el$colorMap("cyan", TRUE)
#' base::message(f("Lazy Dog"))
NULL

#' Colorize
#'
#' RefClassHelper object method to colorize a string
#'
#' @name colorize
#' @method colorize RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$colorize( help=TRUE )
#' 
#' myObject$colorize( msg="", color=NULL, bgcolor=NULL )
#' }
#'
#' Takes a character vector and applies foregrand and/or background
#' color to it.
#' 
#' @param msg Default "". A character vector of text to colorize
#' @param color Default NULL. Optional text (eg "yellow")
#'     corresponding to the foreground color of the text
#' @param bgcolor Default NULL. Optional text (eg "silver")
#'     corresponding to the background color of the text
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A character vector with ANSI color codes injected by
#'     \link[crayon]{crayon}
#'
#' @seealso \link[crayon]{crayon}, \link{colorNameToFunc},
#'     \url{https://en.wikipedia.org/wiki/ANSI_escape_code#Colors}
#'
#' @examples
#'
#' el <- RefClassHelper()
#' x <- el$colorize(c("This", "That"), "green")
#' x
#' message(paste(x, collapse=" ... and ... "))
NULL

#' Use Color
#'
#' RefClassHelper object method to get/set colorization flag
#'
#' @name useColor
#' @method useColor RefClassHelper
#'
#' @details
#'
#' \preformatted{#' ## Method Usage:
#' myObject$useColor( help=TRUE )
#' 
#' myObject$useColor( newval=NULL )
#' }
#'
#' Gets or sets the flag determining if messages should be colorized
#' 
#' @param newval Default NULL. If provided and can be made logical,
#'     will set the flag. Inability to cast as logical will emit a
#'     non-fatal error.
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A single logical value, invisibly
#'
#' @seealso \link{message}, \link{colorize}, \link{useCol}
#'
#' @examples
#'
#' el <- RefClassHelper()
#' el$actionMessage("This is exciting!")
#' el$useColor(FALSE)
#' el$actionMessage("Still exciting, but it does not show as much")
NULL

