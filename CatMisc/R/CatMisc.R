#' Is Empty Field
#'
#' Tests for ReferenceClass fields that have not been set.
#'
#' @details
#'
#' Some RefClass fields will be automatically set, even if you try not
#' to. For example, simple fields (numeric, character, logical, etc)
#' will be unavaoidably (?) set to a zero-length vector of the
#' appropriate type.
#'
#' A field defined as 'ANY', however, can not auto-populate. If left
#' unset, it will be represented as a little Slot object of class
#' "uninitializedField". This function simply tests if the provided
#' object inherits that class
#'
#' @param x The object to be tested
#'
#' @param zero.length.empty Default FALSE, if true then count a
#'     zero-length vector as an empty field (returns TRUE)
#'
#' @return TRUE if x inherits class "uninitializedField", otherwise FALSE
#'
#' @examples
#'
#' ## Simple object with two fields, one numeric, the other ANY
#' foo <- setRefClass("foo", fields = list( x = 'numeric', y = 'ANY' ))
#' foo$methods( initialize = function(...) {
#'     ## Don't do anything. This should override (?) the default
#'     ## $initFields() call.
#' });
#' 
#' fooObj <- foo()
#' 
#' ## $x will be an zero-element numeric vector. 
#' str(fooObj$x)
#' ## It is not considered an empty field:
#' is.empty.field(fooObj$x)
#' ## ... unless you ask for such cases to test positive:
#' is.empty.field(fooObj$x, zero.length.empty=TRUE)
#' 
#' ## $y will be a small object:
#' str(fooObj$y)
#' ## And will report as being an empty field:
#' is.empty.field(fooObj$y)
#' 
#' @export

is.empty.field <- function(x, zero.length.empty=FALSE) {
    if (inherits(x, "uninitializedField")) {
        TRUE
    } else if (zero.length.empty && is.vector(x) && length(x) == 0) {
        TRUE
    } else {
        FALSE
    }
}


#' Is Defined
#'
#' Broad test returning false for a variety of "empty" values
#'
#' @details
#'
#' Born out of frustration with diverse possibilities for
#' "not present". The function is NOT vectorized, instead expecting
#' the passed object to be "a single thing".
#'
#' @param x The object to be tested
#' 
#' @return TRUE if "defined", otherwise FALSE. The following objects
#'     are considered "not defined":
#'
#' \itemize{
#'
#'   \item \code{NULL}
#'
#'   \item Objects that only contain \code{NA}
#'
#'   \item Matrices with zero columns and zero rows
#' 
#'   \item Empty fields (TRUE value from \link{is.empty.field})
#'
#' }
#'
#' @examples
#'
#' ## TRUE (defined) objects
#' is.def(FALSE)
#' is.def(c(""))
#' noData <- matrix(numeric(), 0, 2,
#'            dimnames=list(sample=character(),attr=c("width","weight")))
#' is.def(noData) # No information, but it has two columns
#' is.def(c(NA, NA, "b"))
#'
#' ## FALSE (not defined)
#' is.def(NULL)
#' is.def(numeric())
#' is.def(c(NA,NA,NA,NA))
#' is.def(matrix(numeric(),0,0))  # 0x0 matrix
#'
#' @export

## Return TRUE for defined objects:
is.def <- function(x) {
    if (is.null(x) || is.empty.field(x)) {
        FALSE
    } else if (is.object(x)) {
        TRUE
    } else if (is.matrix(x) && (nrow(x) != 0 || ncol(x) != 0)) {
        ## As long as a matrix has some rows or columns it is
        ## "defined", even if it has no content
        TRUE
        ## RefClass fields defined as "matrix" auto-instantiate with
        ## 0x0 matrices.
    } else {
        ## If any part of x is NOT NA, then it's defined
        any(!is.na(x))
    }
}


#' Is Something
#'
#' Check if something is defined, and not zero or an empty string
#'
#' @details
#'
#' Primarily designed to deal with parameter checking for
#' user-supplied arguments
#'
#' @param x The object to be tested
#'
#' @return TRUE if x is "defined" (\link{is.def}) and is neither
#'     (numeric) zero nor an empty string.
#'
#' @examples
#'
#' is.something("")
#' is.something(0)
#' is.something(c(0,0)) # -> TRUE
#'
#' @export

is.something <- function(x) {
    if (!is.def(x) || length(x) == 0) {
        ## Not defined or length zero? false
        FALSE
    } else if (length(x) > 1) {
        ## More than one thing? Always true
        TRUE
    } else if (is.numeric(x)) {
        ## Single number, 0 = false
        if (x == 0) { FALSE } else { TRUE }
    } else if (is.character(x)) {
        ## Single string, "" = false
        if (x == "") { FALSE } else { TRUE }
    } else if (is.logical(x)) {
        x
    } else {
        TRUE # I guess? Will expand conditions as weirdness occurs...
    }
}

#' Parenthetical Regular Expression
#'
#' Extract values from parenthetical capture blocks in regular expressions
#'
#' @details
#'
#' RegExp in R is not fully fleshed out. This implements a hack
#' suggested in the internal documentation to allow recovery of text
#' from multiple parenthetical captures
#'
#' @return
#'
#' A character vector representing matched values, or \code{NA} if no
#' match was found. If unlist is set to \code{FALSE}, then a list of
#' character vectors, one list element for each value in the submitted
#' text.
#'
#' @param RegExp The regular expression to use
#' @param text The string(s) to test
#' @param ignore.case Default TRUE, which will perform matches case
#'     insensitively.
#' @param unlist Default TRUE, which will unlist the results. A string
#'     that does not match will return a single \code{NA}. If more
#'     that one capture field is defined, and at least one string
#'     fails to match, then the returned, unlisted vector will be
#'     ragged. If you are matching multiple strings, set unlist=FALSE.
#'
#' @examples
#'
#' codes <- c("Launch code: 0000", "Bro code", "Locker code = 321203")
#' extractor <- '([A-Z]+).+?(\\d+)'
#' parenRegExp( extractor, codes, unlist = FALSE )
#'
#' header  <- ">PK139-beta   Alien infection mediator (San Antonio serotype) "
#' fastaRE <- '^>(\\S+)\\s*(.*?)\\s*$'
#' parenRegExp( fastaRE, header )
#'
#' @export

parenRegExp <- function(RegExp, text, ignore.case=TRUE, unlist=TRUE) {
    ## Taken from the Examples section of ?gregexpr
    m <- lapply(regmatches(text, gregexpr(RegExp, text, perl = TRUE,
                                          ignore.case = ignore.case)),
                function(e) {
                    regmatches(e, regexec(RegExp, e, ignore.case = ignore.case))
                })
    ## Select out just the parenthetical parts. Assign a value of NA
    ## if matching failed.
    rv <- lapply(m, function (x) if (length(x) == 0) { NA } else { x[[1]][-1] })
    if (unlist) {
        unlist(rv)
    } else {
        rv
    }
}

#' Text Block to Vector
#' 
#' Convert a multi-line string to a vector of lines
#'
#' @details
#'
#' When embedding static lists of text in code I find it sometimes
#' easier to read and maintain the list if it is encoded as a simple
#' block of text. This function will break such a block into a vector
#'
#' @param x The block of text to parse
#' @param split Default \code{"[\n\r]+"}, the regular expression used
#'     to split out lines
#' @param trim.white Default TRUE, which will cause leading and
#'     trailing whitespace to be removed from each line
#' @param skip.empty Default TRUE, which will remove elements that are
#'     the empty string (\code{''})
#'
#' @return A character vector, each element being a line
#'
#' @examples
#'
#' myBlock <- "
#' A quick brown fox
#'   Five golden rings  
#' Klaatu barada nikto
#' "
#' 
#' textBlockToVector(myBlock)
#'
#' @export

textBlockToVector <- function (x, split="[\n\r]", trim.white=TRUE,
                               skip.empty=TRUE) {
    rv <- unlist(strsplit(x, split))
    if (trim.white) rv <- gsub('(^\\s+|\\s+$)', '', rv)
    if (skip.empty) rv <- rv[ rv != "" ]
    rv
}

#' Flexible Filehandle
#'
#' Utility method to transparently handle 'normal' and compressed files
#'
#' @param file Required, the path to the file to read
#'
#' @return A list with filehandle, basename, suffix and gzip flag
#'
#' @export

.flexFilehandle <- function (file) {
    if (!file.exists(file)) return ( NA )
    name <- basename(file)
    isGz <- CatMisc::parenRegExp("(.+)\\.gz$", name)
    fh <- if (is.na(isGz[1])) {
        ## 'normal' file
        isGz <- FALSE
        file(file, open = "r")
    } else {
        ## gzipped file
        name <- isGz[1]
        isGz <- TRUE
        gzfile(file, open = "r")
    }
    hasSfx <- CatMisc::parenRegExp("(.+)\\.([^\\.]+)$", name)
    sfx <- if (is.na(hasSfx[1])) {
        ## No apparent suffix?
        ""
    } else {
        name <- hasSfx[1]
        hasSfx[2]
    }
    list(fh=fh, name=name, sfx=sfx, gz=isGz)
}

