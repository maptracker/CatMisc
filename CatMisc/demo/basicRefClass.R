## Demonstrate building a Reference Class object with inheritance, in
## particular inheriting the RefClassHelper class with methods
## assisting in documentation.

library("CatMisc")

## Simple class with three fields
demoClass <- setRefClass(
    "demoClass",
    fields = list(
    color   = "character",
    weight  = "numeric",
    modified = "POSIXct"),
    contains = c("RefClassHelper")
)

## You could define the methods in the setRefClass() call, but I find
## it somewhat cleaner to define them as a separate call.

## If a "naked" string is placed at the top of a method function, it
## will be parsed as a brief description for that method when $help()
## is called. It is advised that this be done for each method, both to
## make $help() more informative, and as a simple layer of
## documentation

## If you wrap this code up as a formal package, you can provide
## easy-to-recover help for each method. Just include a `help=FALSE`
## parameter, and the following call at the top of each function:

##   if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )

demoClass$methods(
    initialize = function (color="blue", weight=16.5, modified=Sys.time()) {
        "Initialize a new demoClass object"
        color    <<- color
        weight   <<- weight
        modified <<- modified
    },
    setValues = function(color=NULL, weight=NULL, modified=NULL, help=FALSE) {
        "Allows any or all of the three fields to be set"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!is.null(color))       color <<- color
        if (!is.null(weight))     weight <<- weight
        if (!is.null(modified)) modified <<- modified
    },
    printValues = function(sep="\t", help=FALSE) {
        "Returns the current demo values as a single string"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        paste0(paste(color, weight, modified, sep=sep), "\n")
    },
    orderSixtySix = function() {
        "Not yet implemented"
        stop("Subcontracted to Kamino Enterprises")
    },

    show = function (help=FALSE) {
        "Pretty-prints the demoClass object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        cat(colorize("## demoClass Object ##", "magenta"), "\n",
            colorize("Color:", "blue"), color, "\n",
            colorize("Weight:", "blue"), weight, "kg\n",
            colorize("Modified:", "blue"), modified)
    },
    
    fieldDescriptions = function(help=FALSE) {
        ## If you define a fieldDescriptions method, it will be
        ## utilized by $.showHelp() to describe the fields being used
        "A static list of brief descriptions for each field in this object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        list("color"    = "The color of the entry, as specified in SOP9-B",
             "weight"   = "The weight of the entry, in kilograms",
             "modified" = "Last time the entry was modified (POSIXct)")
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
            "SKIP" = c("orderSixtySix")
            )
        .showHelp(sections, 'obj', color=color)
    }

    
)

## Make an instance:
dc <- demoClass()

## evaluate the instance, which will invoke $show() :
dc

## Get help for the object
dc$help()

## Get help for a method
dc$printValues( help=TRUE)
## ... because this demo is not a formal package, there won't be `man`
## pages defined for it, and the request will report that it was
## unable to find relevant help pages.
