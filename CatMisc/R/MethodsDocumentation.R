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

#' RefClassHelper Variable Name
#'
#' Internal RefClass field holding its own variable name
#'
#' @name varName
#'
#' @details
#'
#' A character string containing the name of the object. That is, an
#' object invoked with the R variable \code{x} should have "x" as the
#' value for this field. It is set using the \link{.selfVarName}
#' function.
#'
#' @seealso \link{.selfVarName}, \link{help}, \link{varCheck},
#' \link{getFieldDescriptions}
#'
#' @examples
#' 
#' rch <- RefClassHelper( )
#' rch$varName # Should initially be unset
#' rch # Pretty-print the object, which will attempt to discover its name
#' rch$varName # Should now be "rch"
NULL

#' RefClassHelper Variable Name Last Checked
#'
#' Internal RefClass field tracking the last time the object name was checked
#'
#' @name varCheck
#'
#' @details
#'
#' The \link{varName} field caches the variable name of the
#' object. Finding this value sometimes fails (not sure why). It is
#' generally possible to eventually recover the name. The
#' \code{varCheck} field tracks the last attempt to get the name, and
#' restricts retries to once every 10 seconds.  function.
#'
#' @seealso \link{varName}, \link{.selfVarName}, \link{help},
#' \link{getFieldDescriptions}
#'
#' @examples
#' 
#' rch <- RefClassHelper( )
#' rch$varCheck # Not attempted yet
#' rch # Pretty-print the object, which will attempt to discover its name
#' Sys.time()
#' rch$varCheck # Should be close to the time above
NULL

#' Field Descriptions
#'
#' Get brief descriptions for all RefClass fields used by the object
#'
#' @name fieldDescriptions
#' @method fieldDescriptions RefClassHelper
#' 
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$fieldDescriptions( help=TRUE )
#' 
#' myObject$fieldDescriptions( )
#' }
#'
#' This method simply returns a list of descriptive text for each
#' Reference Class field defined for the object. It is intended to
#' provide self-documenting information to the \link{help} function.
#'
#' If the object inherits other RefClass classes, they will be
#' recursively queried for descriptive text associated in their own
#' \code{fieldDescriptions} structures. This recursion is managed by
#' \link{getFieldDescriptions}.
#' 
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A list of character vectors, each a single entry long, each
#' (ideally) being a brief description of the field.
#'
#' @seealso \link{getFieldDescriptions}, \link{help},
#' \link{helpSections}
#'
#' @examples
#'
#' el <- RefClassHelper()
#' el$fieldDescriptions()
#' el$help() # Descriptive text will be at bottom
NULL

#' Help Sections
#'
#' Get a list that organizes methods into conceptually-related sections
#'
#' @name helpSections
#' @method helpSections RefClassHelper
#' 
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$helpSections( help=TRUE )
#' 
#' myObject$helpSections( )
#' }
#'
#' This method simply returns a list that organizes the methods used
#' by the Reference Class object into related sections. It is intended
#' to provide self-documenting information to the \link{help}
#' function.
#'
#' If the object inherits other RefClass classes, they will be
#' recursively queried for their own sections. This recursion is managed by
#' \link{getHelpSections}.
#' 
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A list of character vectors, each containing one or more
#' methods within the section defined by that entry's name.
#'
#' @seealso \link{getHelpSections}, \link{help}, \link{fieldDescriptions}
#'
#' @examples
#'
#' el <- RefClassHelper()
#' el$helpSections()
#' el$help() # Methods will be broken out into the above sections
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
#' \preformatted{
#' ## Method Usage:
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
#' rch <- RefClassHelper()
#' ## Show the object:
#' rch
#' ## Turn off colorization, show the object again:
#' rch$useColor(FALSE)
#' rch
NULL

#' Object Help
#'
#' Summarize object methods and fields, and commands to get additional help
#'
#' @name help
#' @method help RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$help( help=TRUE )
#' 
#' myObject$help( genericName='myObject', color=NULL, generic=FALSE )
#' }
#'
#' Shows help for the object. This method attempts to both be compact
#' and exhaustive. Each object method will be shown, as well as a
#' one-line description of its function (if it has been defined in the
#' code). Additionally, the method call using the help flag will be
#' shown explicitly with the actual object variable. That is, if
#' \code{$help()} is called on an object named \code{foo} that has a
#' method named \code{soak}, then the reported text will include:
#'
#' \code{foo$soak( help=TRUE )}
#'
#' This is to aid rapid copy-paste exploration of the object.
#'
#' Object fields will also be reported, along with their descriptive
#' text.
#' 
#' @param genericName Default 'myObject'. Used if R can not determine
#' the actual variable name of the object (via \link{.selfVarName})
#' @param color Default \code{NULL}, which will use the value defined
#' by \link{useColor}. If TRUE, then output will be colorized.
#' @param generic Default \code{FALSE}. If TRUE, then generic
#' Reference Class methods common to all RefClass objects will be
#' included.
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return NULL, invisibly
#'
#' @seealso \link{getFieldDescriptions}, \link{getHelpSections},
#' \link{show}
#'
#' @examples
#'
#' rch <- RefClassHelper()
#' ## Report help for the object
#' rch$help()
#' rch$help( generic=TRUE ) # Include generic methods
NULL

#' Get Field Descriptions
#'
#' Get brief descriptions for all object fields
#'
#' @name getFieldDescriptions
#' @method getFieldDescriptions RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$getFieldDescriptions( help=TRUE )
#' 
#' myObject$getFieldDescriptions( )
#' }
#'
#' Returns a list, with names representing object fields and values
#' being descriptions of each one. The descriptions need to be defined
#' in the object's \link{fieldDescriptions} method. If inheritance is
#' being used, they will be recovered recursively for each parent
#' class. The function is used by the \link{help} method.
#' 
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A list of character strings
#'
#' @seealso \link{fieldDescriptions}, \link{getHelpSections},
#' \link{help}
#'
#' @examples
#'
#' rch <- RefClassHelper()
#' rch$getFieldDescriptions()
#' rch$help() # Will be shown at the end of the help
NULL

#' Get Help Sections
#'
#' Get a list of all notable object methods, organized into sections
#'
#' @name getHelpSections
#' @method getHelpSections RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$getHelpSections( help=TRUE )
#' 
#' myObject$getHelpSections( )
#' }
#'
#' Returns a list, with names representing conceptual sections and
#' values being object methods assigned to those sections. This
#' structure needs to be defined in the object's \link{helpSections}
#' method. If inheritance is being used, sections will be recovered
#' recursively for each parent class. The function is used by the
#' \link{help} method.
#' 
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A list of character strings
#'
#' @seealso \link{helpSections}, \link{getFieldDescriptions},
#' \link{help}
#'
#' @examples
#'
#' rch <- RefClassHelper()
#' rch$getHelpSections()
#' rch$help() # Will be used near the top to organize methods
NULL

#' Get Name of Current Variable
#'
#' For an object, get the variable name being used by R to 'hold' it
#'
#' @name dotSelfVarName
#' @aliases .selfVarName
#' @method .selfVarName RefClassHelper
#'
#' @details
#'
#' \preformatted{
#' ## Method Usage:
#' myObject$.selfVarName( help=TRUE )
#' 
#' myObject$.selfVarName( def="myObj", fallbackVar="" )
#' }
#'
#' Returns the variable name of an object. That is, if you have an
#' object \code{foo}, calling \code{foo$.selfVarName} should return
#' \code{"foo"}.
#'
#' This functionality is used to generate self-referential help text
#' that can be copied-and-pasted to allow for easy exploration of the
#' object.
#'
#' @param def Default \code{"myObj"}, a default name that can be used
#' if the actual name can not be found.
#' @param fallbackVar Default \code{""}. Another default name, will be
#' used in preference to \code{def} if the real name can not be
#' found. There are ... reasons ... for this. I can't remember what
#' they were and am too timid to simplify the function at this point.
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A single character string
#'
#' @seealso \link{varName}, \link{help}, \link{varCheck},
#' \link{getFieldDescriptions}
#'
#' @examples
#'
#' rch <- RefClassHelper()
#' rch$.selfVarName()
#' identical("rch", rch$.selfVarName())
NULL

#' Initialize RefClass Object
#'
#' R internal method to create a new Reference Class instance
#'
#' @name initialize
#' @method initialize RefClassHelper
#'
#' @details
#'
#' The \code{$initialize()} object method is one of the reserved
#' Reference Class functions. It executes whenever you generate a new
#' object (that is, if a class `foo` is defined, when you run \code{x
#' <- foo()}). You should not call \code{$initialize()} directly.
#'
#' In many cases you may not need to define such a method. I have
#' found, however, that if you are attempting RefClass inheritance
#' that the method can be useful. Some suggestions when creating a
#' method:
#'
#' \itemize{
#'    \item Make use of \dots in the parameters.
#'    \item Be sure to call \code{callSuper(...)} to invoke the
#'          \code{initialize} for parent classes
#'    \item This is a reasonable place to define default field values, expose
#'          them for parameterized customization, and put in sanity-checking
#'    \item Using the same variable name for parameters and fields works, but
#'          can be confusing.
#'    \item Do not forget to set fields with \code{<<-}
#' }
#'
#' @return The newly created RefClass object
#' 
#' @examples
#'
#' ## Using one of the built-in toy objects:
#' rct <- myRefClassThing() # There. That was it. It ran transparently.
NULL

