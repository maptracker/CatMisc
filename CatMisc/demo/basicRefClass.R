## Demonstrate building a Reference Class object with inheritance, in
## particular inheriting the RefClassHelper class with methods
## assisting in documentation.

library("CatMisc")

## Simple class with three fields
demoClass <- setRefClass(
    "demoClass",
    fields = list(
    cVal    = "character", # Color
    wVal    = "numeric",   # Weight
    mVal    = "POSIXct"),  # Modified Time
    contains = c("RefClassHelper")
)

## You could define the methods in the above setRefClass() call, but I
## find it somewhat cleaner to define them as a separate $methods()
## call (implemented below).

## If a "naked" string is placed at the top of any method function, it
## will be parsed as a brief description for that method when $help()
## is called. It is advised that this be done for each method, both to
## make $help() more informative, and as a simple layer of
## documentation for your code.

## If you wrap this code up as a formal package, you can provide
## easy-to-recover help for each method. Just include a `help=FALSE`
## parameter, and the following call at the top of each function:
##
##   if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
##
## This demo can not benefit from the Roxygen documentation
## processing that you would get if you built this as a formal
## package. If this were a package, you could write Roxygen blocks
## like the one below. Those topics would then be available in `?`
## help calls.

#' Set Values
#'
#' demoClass object method to set color, weight and modified date
#'
#' @name setValues
#' @method setValues demoClass
#'
#' @details
#'
#' \preformatted{#' ## Method Usage:
#' myObject$setValues( color=NULL, weight=NULL, modified=NULL)
#' 
#' myObject$setValues$useColor( help=TRUE )
#' }
#'
#' Sets the values held by the demoClass object. Each value is
#' optional, so you need only set the value(s) of interest
#' 
#' @param color Default \code{NULL}. If non-NULL, will set the
#'   \code{cVal} field of the object (color)
#' @param weight Default \code{NULL}. If non-NULL, will set the
#'   \code{wVal} field of the object (weight in kilograms)
#' @param modified Default \code{NULL}. If non-NULL, will set the
#'   \code{mVal} field of the object (modified time stamp)
#' @param help Default FALSE. If TRUE, show this help and perform no
#'     other actions.
#'
#' @return A list of the three values currently stored.
#'
#' @seealso \link{printValues}
#'
#' @examples
#'
#' dc <- demoClass()
#' ## Show the current values:
#' dc$printValues()
#' ## Change two fields:
#' dc$setValues(color='sepia', weight='42')
#' ## Check that the change took place:
#' dc$printValues()
NULL

## Again, within this demo the above block of Roxygen will be
## completely ignored.


demoClass$methods(
    
    initialize = function (color="blue", weight=16.5, modified=Sys.time(),
    ... ) {
        "Initialize a new demoClass object"
        cVal <<- color
        wVal <<- weight
        mVal <<- modified
        callSuper(...)
    },
    
    setValues = function(color=NULL, weight=NULL, modified=NULL, help=FALSE) {
        "Allows any or all of the three fields to be set"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!is.null(color))    cVal <<- color
        if (!is.null(weight))   wVal <<- weight
        if (!is.null(modified)) mVal <<- modified
        list(color=cVal, weight=wVal, modified=mVal)
    },
    
    printValues = function(sep="\t", help=FALSE) {
        "Returns the current demo values as a single string"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        paste0(paste(cVal, wVal, mVal, sep=sep), "\n")
    },
    
    orderSixtySix = function() {
        ## Somebody find out if Grievous is even *checking* his tickets!
        "Not yet implemented"
        stop("Unfinished - Subcontracted to Kamino Enterprises")
    },

    ## The functions show(), fieldDescriptions() and help() are
    ## specially handled by RefClassHelper:
    
    show = function (help=FALSE) {
        ## Kind of like a print.class function that allows nice
        ## customization of the object rendering
        "Pretty-prints the demoClass object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        cat(colorize("## demoClass Object ##", "magenta"), "\n",
            colorize("Color:", "blue"), cVal, "\n",
            colorize("Weight:", "blue"), wVal, "kg\n",
            colorize("Modified:", "blue"), format(mVal), "\n")
    },
    
    fieldDescriptions = function(help=FALSE) {
        ## If you define a fieldDescriptions method, it will be
        ## utilized by $.showHelp() to describe the fields being used
        "A static list of brief descriptions for each field in this object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        ## Note: mVal apparently can't take attributes aas a POSIXct vector
        list("cVal" = "The color of the entry, as specified in SOP9-B",
             "wVal" = "The weight of the entry, in kilograms",
             "mVal" = "Last time the entry was modified (POSIXct)")
    },

    help = function (color=NULL, help=FALSE) {
        ## If you define a $help() method, it allows you to organize
        ## your methods into sections. The special SKIP section
        ## defines methods that should not be shown
        "Display high-level help about all object methods"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        sections <- list(
            "Data Retrieval" = c("printValues"),
            "Updates" = c("setValues"),
            "SKIP" = c("orderSixtySix") # Don't want to expose this to user
            )
        showHelp(sections, 'obj', color=color)
    }

    
)

## Make an instance:
dc <- demoClass()

## evaluate the instance, which will transparently invoke $show() :
dc

## Get help for the object
dc$help()

## Confirm that we've inherited colorization management from RefClassHelper:
dc$useColor( FALSE )
dc  # No longer colorized

## Get help for a method
##    dc$printValues( help=TRUE)
## ... because this demo is not a formal package, there won't be `man`
## pages defined for it, and the above request will report that it was
## unable to find relevant help pages.

## Get help for a method within the inherited RefClassHelper
## class. This *should* work, since those methods are documented
## within the CatMisc package:
dc$useColor( help=TRUE )
