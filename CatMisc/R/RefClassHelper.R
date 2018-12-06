#' Reference Class Helper Class
#'
#' A utility class that provides help functions when inherited
#' 
#' @details
#'
#' This class is not designed to be used on its own, but rather to be
#' inherited by another Reference Class.
#'
#' @field useCol Default TRUE. Logical flag to indicate if color
#'   should be used in messaging
#' @field varName Extracted variable name of the object
#' @field varCheck Last time an attempt was made to extract varName
#' 
#' @import crayon
#' 
#' @export RefClassHelper
#' @exportClass RefClassHelper

RefClassHelper <- setRefClass(
    "RefClassHelper",
    fields = list(
    useCol   = "logical",
    varName  = "character",
    varCheck = "POSIXct" )
)

RefClassHelper$methods(
    
    initialize = function(useColor=TRUE, ...) {
        "Create a new RefClassHelper Reference Class object"
        ## Manage useCol flag through method. Default is TRUE but can be reset
        .self$useColor( useColor )
    },
    
    colorMap = function(color, bg=FALSE, help=FALSE) {
        "Convert a color name into a crayon colorizing function"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!useColor() || ! CatMisc::is.def(color)) return(NA)
        if (is.function(color[1])) return( color[1] )
        key   <- "FG"
        color <- tolower(color[1])
        if (bg || grepl('^bg', color)) {
            ## Use the background methods
            key   <- "BG"
            ## Lookup keys are standardized to discard the leading bg:
            color <-  gsub('^bg','', color)
        }
        CatMisc:::colorNameToFunc()[[ key ]][[ color ]]
    },

    colorize = function(msg="", color=NULL, bgcolor=NULL, help=FALSE) {
        "Use crayon to add ANSI color codes to text"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!is.character(msg)) msg <- as.character(msg)
        fgFn <- colorMap(color, FALSE)
        if (is.function(fgFn)) msg <- fgFn(msg)
        bgFn <- colorMap(bgcolor, TRUE)
        if (is.function(bgFn)) msg <- bgFn(msg)
        msg
    },
    
    useColor = function(newval=NULL, help=FALSE) {
        "Get or set the flag determining if messages are colorized"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!is.null(newval)) {
            ## Request to change the value
            nv <- as.logical(newval)[1]
            if (is.na(nv)) {
                warning("useColor() should be provided with a boolean argument")
            } else {
                useCol <<- nv
            }
        }
        invisible(useCol)
    },

    annotateFields = function(help=FALSE) {
        "Update object fields to include attributes with brief descriptions"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        ## If the descriptions are not defined then nothing to be done:
        if (!is.function(.self[["fieldDescriptions"]])) return(FALSE)
        myClassName <- class(.self)
        fields <- fieldDescriptions()
        hfmt <- paste0(" help('%s', '", myClassName,
                       "') # More information on field ")
        for (fld in names(fields)) {
            if (is.null(.self[[fld]])) next # Can't attribute NULL
            ## The [[ accessor seems to work for fields?
            attr(.self[[fld]], "Description") <- fields[[fld]]
            attr(.self[[fld]], "Help") <- sprintf(hfmt,fld)
        }
        TRUE
    },

    ## Any objects that inherit this class should write their own
    ## methods for:
    ##   $fieldDescriptions()
    ##   $help()

    fieldDescriptions = function(help=FALSE) {
        "A static list of brief descriptions for each field in this object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        list("useCol"   = "Flag indicating if messages should be colorized",
             "varName"  = "Extracted variable name associated with this object",
             "varCheck" = "Datestamp, last attempt to extract varName")

    },

    help = function (color=NULL, help=FALSE) {
        "Display high-level help about all object methods"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        sections <- list(
            "Color Management" = c("useColor", "colorize", "colorMap",
            "colorNameToFunc"),
            "Help Management" = c("methodHelp", "help", ".showHelp"),
            "Utility Methods" = c("allRefClasses")
            )
        .showHelp(sections, 'RefClassHelper', color=color)
    },

    .showHelp = function (sections=list(), genericName='myObject', color=NULL,
    help=FALSE) {
        "Construct text for the help() method"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (is.null(color)) color <- useColor() # Use color setting
        doCol   <- if (color) { .self$colorize } else { function(x, ...) x }
        objName <- .self$.selfVarName(genericName)
        whtName <- doCol(objName, "white")
        comCol  <- "yellow"

        ## Figure out all available methods:
        myClassName <- class(.self)
        myClass     <- methods::getRefClass(myClassName)
        allMeth     <- myClass$methods()
        ## Subtract out the generic ones:
        allMeth <- setdiff(allMeth, c("callSuper", "copy", "export", "field", "getClass", "getRefClass", "import", "initFields", ".objectPackage", ".objectParent", "show", "trace", "untrace", "usingMethods"))
        ## Subtract out some superclasses
        allMeth <- allMeth[ !grepl('#', allMeth) ]

        ## Junk drawer section for methods not defined in `sections`:
        sections[["Other Methods"]] <- setdiff(allMeth, unname(unlist(sections)))

        ## Basic header:
        txt <- sprintf("
%s
?%s %s
%s                 %s
str(%s, max.lev=3) %s
",
doCol(sprintf("###
### %s Help - call the below commands for more details
###", myClassName),"magenta"),
myClassName,
doCol("# Built-in documentation on the class", comCol),
whtName, doCol("# Summary report of the object", comCol),
whtName, doCol("# Inspect the object structure", comCol))

        noHelp <- c()
        ## Add snippets for each method, broken down by section
        for (sec in names(sections)) {
            if (sec == "SKIP") next
            txt <- c(txt, doCol(paste("\n############\n###", sec, "\n"),comCol))
            meths <- sections[[ sec ]]
            for (meth in meths) {
                ## Going to see if we can extract the ROxygen
                ## description string from the method
                suppressWarnings(try(code <-
                    utils::capture.output(myClass$methods(meth))))
                ## Should not happen, but be safe:
                if (is.null(code)) {
                    next
                    
                }
                isHelped <- FALSE
                com <- NA
                for (line in code) {
                    ## See if 'help=FALSE' is set - indicates I have
                    ## tied it into my internalized help framework:
                    if (grepl('help\\s*=\\s*FALSE', line))  isHelped <- TRUE
                    cm <- CatMisc::parenRegExp('^\\s+"(.+)"\\s*$', line)
                    if (!is.na(cm[1])) {
                        ## Found a single line quoted string, presume
                        ## it is description and stop scanning code:
                        com <- cm[1]
                        break
                    }
                }
                if (!isHelped) {
                    noHelp <- c(noHelp, meth)
                    next
                }
                txt <- c(txt, sprintf("%s$%s( help=TRUE )", whtName, meth))
                if (!is.na(com)) txt <-
                     c(txt, doCol(paste("\n    #", com), comCol))
                txt <- c(txt, "\n")
            }
        }
        noHelp <- setdiff(noHelp, 'help')
        if (length(noHelp) > 0) txt <- c(txt,
              doCol("\n### Methods lacking help\n", comCol),
              sprintf("# %s\n", strwrap(paste(noHelp, collapse=' '))))

        txt <- c(txt, doCol("\n### Object fields\n", comCol))
        ## Some fields are expected to be small structures, don't need str():
        strFmt <- "str(%s$%s)" # Fields with complex format
        simFmt <- "%s$%s"      # Simple fields
        fCls   <- .self$.refClassDef@fieldClasses # field structure
        fDesc  <- if (is.function(.self[["fieldDescriptions"]])) {
            fieldDescriptions( ) } else { list() }
        ## 'simple' vector types
        simpVec <- c("logical", "numeric", "double", "character",
                     "factor", "complex")
        for (field in names(fCls)) {
            cls <- fCls[[ field ]]
            ## Is the field "simple"? We will consider it as such if:
            ##  1. It is one of the vector types listed above
            ##  2. It is 10 elements or less
            
            isSimple <- is.element(cls, simpVec) &&
                length( .self[[ field ]]) <= 10
            fmt <- ifelse(isSimple, simFmt, strFmt)
            txt <- c(txt, sprintf(fmt, whtName, doCol(field, "blue")))
            com <- sprintf(" # [%s]", cls)
            if (CatMisc::is.something(fDesc[[ field ]]))
                com <- paste0(com, " ", fDesc[[ field ]])
            txt <- c(txt, doCol(com, comCol), "\n")
        }
        annotateFields()
        if (length(fCls) == 0) {
            txt <- c(txt, doCol("# This object does not have fields\n", "yellow"))
        }
        base::message(paste(txt, collapse='', sep=''))
        invisible(NULL)
    },

    .selfVarName = function( def="myObj", fallbackVar="", help=FALSE ) {
        "Determine the variable name of this object"
        if (help) return( CatMisc::methodHelp(match.call(), class(.self) ) )
        if (!CatMisc::is.something(varName)    # Have not found the name yet
            && (is.null(varCheck)              # and we have not tried yet
                || Sys.time() - varCheck > 10) # .. or it's been 10 sec
            ) {
            for (vn in ls(pos=1)) {
                ## I really hope there's a better way to do this. I
                ## have not found it. So instead, look at all variable
                ## names in the global environment and see if the
                ## object they recover is the same as this one.
                if (identical(get(vn, pos=1), .self)) {
                    varName <<- vn
                    break
                }
            }
            ## Don't want to keep trying a fruitless task over and
            ## over. So make note of when we last did this so we don't
            ## spam ls(), get() and identical()
            varCheck <<- Sys.time()
        }
        if (CatMisc::is.something(varName)) {
            ## We think we identified the name of the variable!
            varName
        } else if (CatMisc::is.something(fallbackVar)) {
            ## Well, at least we have a meaningful fallback name
            fallbackVar
        } else {
            ## Crumb. Could not find the name, no fallback given
            def
        }
    }
    
)


#' Example Inherited RefClass Object
#'
#' A tiny object designed to be inherited by another tiny object
#'
#' @details
#'
#' This is a toy ReferenceClass (aka 'R5') object that is used by
#' another toy class (\link{myRefClassThing}), and is used for tests
#' of \link{allRefClasses}.
#'
#' @field txt An interesting text value
#'
#' @seealso \link{myRefClassThing}, \link{allRefClasses}

myRefClassStuff <- setRefClass(
    "myRefClassStuff",
    fields=list(txt="character"),
    contains = c("RefClassHelper"))

myRefClassStuff$methods(
    initialize = function (txt="Initial text") {
        "Initialize a new myRefClassStuff object"
        txt <<- txt
    },
    thingText = function(prefix="[Thing Text]", help=FALSE) {
        "Reports the text held by the thing"
        if (help) return( methodHelp(match.call(), class(.self) ) )
        ## Actual function code follows:
        paste(prefix, txt)
    }
)
        
#' Example Reference Class Object
#'
#' A tiny object that multiplies things
#'
#' @details
#'
#' This is a toy ReferenceClass (aka 'R5') object used to illustrate
#' the \link{methodHelp} function's use in documenting object
#' methods. It is also utilized by tests for \code{methodHelp}.
#'
#' @field x A numeric value
#'
#' @examples
#'
#' x <- myRefClassThing( x=17 )
#' # Help at the method level
#' x$thingProduct( help=TRUE )
#'
#' @seealso \link{methodHelp}
#' 
#' @importFrom methods new setRefClass
#' @export myRefClassThing
#' @exportClass myRefClassThing

myRefClassThing <- setRefClass(
    "myRefClassThing",
    fields=list(x="numeric"),
    contains = c("myRefClassStuff"))

myRefClassThing$methods(

    ## In most cases, a generic methodHelp() can be made as:

    ## methodHelp(match.call(),
    ##            class(.self),
    ##            names(.refClassDef@contains))

    ## However, to make this toy example work without working up a
    ## full package for 'myRefClassThing', we are manually including
    ## 'CatMisc' instead, to allow the help topic to be found. The
    ## method call is also being manually set for initialize
    ## (otherwise it would be 'initialize', which would be recognized
    ## as a constructor and converted to 'CatMisc')
    
    initialize = function( x=3, ...) {
        "Initialize a new myRefClassThing object"
        x <<- x
        callSuper(...)
    },
    thingProduct = function( y=7, help=FALSE ) {
        "Multiplies the x field by parameter y"
        if (help) return( methodHelp(match.call(), class(.self) ) )
        ## Actual function code follows:
        message("Multiplying ",x," by ",y," ...")
        x * y
    }
)

#' Thing Product
#'
#' A toy object method used to illustrate \link{methodHelp}
#'
#' @name thingProduct
#' @method thingProduct myRefClassThing
#'
#' @details Multiplies the internally-stored value of x by a supplied
#'     second number.
#' 
#' @param y The second number
#'
#' @return A numeric product of x * y
#'
#' @examples
#'
#' mrct <- myRefClassThing(5)
#' mrct$thingProduct(11)
#' 
NULL


#' Method Help
#'
#' Mechanism to identify relevant help topics from calling context
#'
#' @details
#'
#' This function is part of an attempt to better document
#' ReferenceClass objects, here focusing on method documentation. The
#' function is designed to be called generically from within a method;
#' See Examples for a fleshed-out illustration. This allows richer
#' documentation to be maintained in Roxygen, and allows objects to be
#' self-documenting if the user passes a help=TRUE flag, eg:
#'
#' \preformatted{
#'  myRefClass$methods(
#'    cube = function( x, help=FALSE ) {
#'        if (help) return( methodHelp(match.call(), class(.self),
#'                                     names(.refClassDef@contains)) )
#'        x ^ 3
#'    })
#' }
#'
#' @param mc Required, the result of match.call, called just before
#'     entering this function. This should allow automatic
#'     determination of the method name, as well as the variable name
#'     holding the object
#' @param cl Required, the class() of the object
#' @param inh Ignored. Included for compatibility with old calls.
#'
#' @return The mysterious 'help_files_with_topic' object R uses for
#'     managing internal help (which will be rendered if not
#'     captured), or NA if no topic could be found
#'
#' @examples
#' 
#' mrct <- myRefClassThing(5)
#' 
#' # General information on the class as a whole:
#' ?myRefClassThing
#'
#' # Specific information on the $thingProduct() function
#' mrct$thingProduct( help=TRUE )
#' 
#' @importFrom utils help
#' @export

methodHelp <- function( mc, cl, inh ) {
    mc <- as.character(mc)
    if (!is.something(mc)) {
        warning("methodHelp(): No calling code was provided, can not determine method name")
        return( invisible(NA) )
    }
    vm       <- parenRegExp('^(\\S+)\\$([a-z0-9_.]+)', mc[1])
    if (is.na(vm[1])) {
        if (grepl('^[a-z0-9_.]+$', mc[1], ignore.case=TRUE)) {
            ## Looks like just a method name
            vm <- c("myObject", mc[1])
        } else {
            warning("Failed to parse method name from calling code: ", mc[1])
            return( invisible(NA) )
        }
    }
    varName  <- vm[1]
    methName <- vm[2]
    
    ## Note - useful function for extracting actual help data:
    ## https://stackoverflow.com/a/9195691 (Richie Cotton)

    ## Get all classes associated with this object
    arc      <- allRefClasses( cl )
    clNames  <- names(arc)
    allPacks <- list()  # Gather observed packages
    
    for (clName in clNames) {
        ## See if we can find a help topic named after the method in
        ## the package it appears to come from
        cl   <- arc[[ clName ]] # Class structure
        pack <- cl@package      # Package name (string)
        ## Check if package is known:
        packOk <- find.package(pack, quiet=TRUE)
        if (length(packOk) == 0) next
        x <- utils::help(methName, (pack) )
        ## See if documentation exists:
        if (is.something(x[1])) return( x )
        ## If not, it appears to indicate that no topic was found - keep looking
        allPacks[[ pack ]] <- TRUE
    }

    ## If we got here, we failed to find a help topic for the method
    if (length(allPacks) == 0) allPacks[["--No packages identified--"]] <- TRUE
    warning("
Sorry!
I failed to find documentation for ", methName,"() in any of these packages:
    ", paste(names(allPacks), collapse=', '), "
You can also try:
    ?'",clNames[1],"::",methName,"'  or   ??'",methName,"'
")
    invisible(NA)
}


##' All Reference Classes
##'
##' Return class representations for object and all inherited classes
##'
##' @details
##'
##' @param obj Required, the object to be tested. This can be either a
##' class name, a class generator structure, or a RefClass object
##' itself.
##' @param standardClasses Default FALSE. If TRUE then the R-internal
##' utility classes will be included in the output.
##' @param struct Default NA. Used internally for recursion
##' 
##' @importFrom methods getRefClass
##' @export

allRefClasses <- function( obj, standardClasses=FALSE, struct=NA ) {
    ## If this is the first call, initialize struct as an empty list
    if (is.na(struct)) struct <- list()

    myClass     <- NULL
    myClassName <- NULL
    if (inherits(obj, 'refObjectGenerator')) {
        ## obj is already a RefClass definition, presumably passed
        ## recursively
        myClass     <- obj
        myClassName <- myClass@className[1]
    } else {
        ## Since I am allowing polymorphic input for `obj`, certain
        ## inputs will make some of the tests below fatally angry
        suppressWarnings(try(
            if (inherits(obj[[".refClassDef"]], "refClassRepresentation")) {
                ## Object appears to be an instatiated RefClass object
                myClassName <- class(obj)[1]
                myClass     <- methods::getRefClass(myClassName)
            }
            ))
        if (is.null(myClass)) {
            suppressWarnings(try(
                if (is.character(obj[1])) {
                    ## This is a string name for a class?
                    myClass <- methods::getRefClass(obj[1])
                    if (inherits(myClass, 'refObjectGenerator')) {
                        ## We seem to have gotten it
                        myClassName <- myClass@className[1]
                    }
                }
                ))
        }
    }
    
    ## If none of the above worked, return the structure so far:
    if (is.null(myClass) || is.null(myClassName)) return(struct)

    ## If we have already dealt with the class then do nothing further:
    if (inherits(struct[[ myClassName ]], 'refObjectGenerator'))
        return(struct)

    ## Ok, newly seen class! Note it:
    struct[[ myClassName ]] <- myClass

    stndCls <- c("envRefClass", ".environment", "refClass",
                 "environment", "refObject")
    ## Does it inherit any other classes?
    cont    <- myClass@generator$def@contains
    for (cName in names(cont)) {
        ## Do not include the "standard" classes, unless requested
        if (!standardClasses && is.element(cName, stndCls)) next
        ## If we have not seen this class before, recursively call
        ## this function to add it to the structure
        if (!inherits(struct[[ cName ]], 'refObjectGenerator')) {
            struct <- allRefClasses(methods::getRefClass( cName ),
                                    struct=struct )
        }
    }
    struct # Return the list structure
}

#' Color Name to Crayon Function
#'
#' Function to generate a map of color names to crayon functions
#'
#' @details
#'
#' The list generated by this method is simply a lookup list-of-lists
#' used to turn names ("cyan", "black") into \link[crayon]{crayon}
#' colorizing functions. It is utilized by \link{colorize} to convert
#' the user's color parameters into the appropriate colorizing
#' functions.
#'
#' The function is run once, the first time the map is
#' requested. Afterwards the resulting list is stored in a local
#' variable for cached retrieval.
#' 
#' @return A list of lists
#'
#' @seealso \link{colorize}

colorNameToFunc <- function( ) {
    "Internal utility, generates list-of-lists that maps color names to crayon functions"
    ## If the color map has already been generated, return it. It
    ## "lives" as an internal variable inside the package
    ## environment (shared by all instances of EventLogger)
    cmVar <- "colorMapList"
    if (exists(cmVar, envir=packEnv)) return(get(cmVar, envir=packEnv))
    ## We need to make the map

    ## Was difficult to juggle referencing colors by function name
    ## when you can't be sure the user has installed
    ## crayon. Instead, make a named lookup of crayon functions,
    ## which will then be used by $colorize() to get() the correct
    ## function, provided it exists().
    myNames <- c("black", "red", "green", "yellow", "blue", "magenta",
                 "cyan", "white", "silver", "gray", "purple", "lightblue")
    fgNames <- myNames
    names(fgNames) <- myNames
    ## I have included some aliases, remap to the R/ANSI names:
    fgNames[ "gray" ]      <- "silver"
    fgNames[ "purple" ]    <- "magenta"
    fgNames[ "lightblue" ] <- "cyan"
    ## Background color is the same, but with capitalized first
    ## letter and a "bg" prefix, eg "bgYellow":
    bgNames <- vapply(fgNames, function (x) {
                          paste("bg", toupper(substr(x,1,1)),
                                substr(x,2,nchar(x)), sep="") }, "")
    bgNames[ "silver" ] <- "bgWhite"
    bgNames[ "gray" ]   <- "bgWhite"
    ## Initially I tried to manage this as a simple string->string
    ## lookup, and then converted each string to a function
    ## on-the-fly with get(). However, get() will need package
    ## crayon to be in the search space, and when the object is
    ## used via inheritance that seems to not be the case. So I am
    ## going to instead use this as a string->function lookup, and
    ## evaluate each function here.
    cm <- list(FG=fgNames, BG=bgNames)
    cf <- list()
    for (typ in names(cm)) {
        cf[[typ]] <- list()
        for (nm in names(cm[[typ]])) {
            cc <- cm[[typ]][ nm ]
            ## eval() in R: https://stackoverflow.com/a/1743796
            cf[[typ]][[tolower(nm)]] <-
                eval(parse(text=paste("crayon::", cc, sep="")))
        }
    }
    ## Store the constructed map in our environment and return it
    assign(cmVar, cf, envir=packEnv)
    cf
}
