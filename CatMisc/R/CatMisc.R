
# This package's environment, used to manage return values from inside
# file.rename2()'s tryCatch environment:
packEnv  <- as.environment(-1) 



#' CAT Miscellaneous Functions
#'
#' A collection of utility methods frequently used in packages by
#' Charles Tilford (CAT)
#'
#' @name CatMisc
#'
#' @details
#'
#' \itemize{
#' 
#'   \item is.empty.field - Test for empty RefClass fields
#'   \item is.def - Tests a scalar against a variety of "nothing" values
#'   \item is.something - Tests if object is defined, and not zero, "" or FALSE
#'   \item parenRegExp - Simplifies capture from regular expression parentheses
#'   \item textBlockToVector - Splits a block of text into lines
#'   \item .flexFilehandle - Automatically handles .gz files
#'   \item methodHelp - Used inside RefClass methods for "self-help"
#'   \item file.rename2 - File rename that can cross device boundaries
#' }
#' 
#' 
NULL

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
#'   \item Objects that ONLY contain \code{NA}
#'   \item Matrices with zero columns and zero rows
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
    } else if (is.object(x) || is.function(x)) {
        ## Objects and functions are always defined
        TRUE
    } else if (is.matrix(x) && (nrow(x) != 0 || ncol(x) != 0)) {
        ## As long as a matrix has some rows or columns it is
        ## "defined", even if it has no content
        TRUE
        ## RefClass fields defined as "matrix" auto-instantiate with
        ## 0x0 matrices.

        ## Eh. matrix() (with no arguments) returns a 1x1 logical
        ## matrix with a single NA cell. Not sure how to handle that
        ## (it will end up in this block == TRUE)
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
        ## If we are here, we have a single thing
        if (is.vector(x)) {
            x
        } else {
            ## We could have a matrix here, and it could be one or
            ## more NAs. To assure that this returns a logical value,
            ## vectorize and run through is.def:
            is.def(as.vector(x))
            ## That should deal with a matrix of just NA
        }
    } else {
        TRUE # I guess?
        
        ## Further conditions will be added if I find 'things' that
        ## are passing but shouldn't, or causing issues with the
        ## if/else tree above.
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
#' @name dotFlexFilehandle
#' 
#' @details
#'
#' If the file ends with 'gz', then \code{gzfile} will be used to open
#' a file handle. Otherwise, \code{file} will be used.
#'
#' @param file Required, the path to the file to read
#'
#' @return A list with filehandle ("fh"), basename ("name"), suffix
#'     ("sfx") and gzip flag ("gz")
#' 
#' @export

.flexFilehandle <- function (file) {
    if (!file.exists(file)) return ( NA )
    name <- basename(file)
    isGz <- parenRegExp("(.+)\\.gz$", name)
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
    hasSfx <- parenRegExp("(.+)\\.([^\\.]+)$", name)
    sfx <- if (is.na(hasSfx[1])) {
        ## No apparent suffix?
        ""
    } else {
        name <- hasSfx[1]
        hasSfx[2]
    }
    list(fh=fh, name=name, sfx=sfx, gz=isGz)
}

#' File Rename II
#'
#' Attempts to move/rename files over device boundaries
#'
#' @details
#'
#' At least some implementations of R apparently use a low-level
#' rename library that will refuse to move files across device
#' boundaries. That is, if the file resides at a path that is on a
#' device different from the destination path, the rename will fail
#' with an error message similar to:
#'
#' \code{
#' In file.rename(fromPath, toPath) :
#'   cannot rename file '/mnt/deviceX/foobar.txt' to '/mnt/deviceY/foobar.txt',
#'   reason 'Invalid cross-device link'
#' }
#'
#' Some (many? all?) systems disallow (or simply cannot?) make
#' \emph{hard links} between devices. However, a move/rename is not a
#' link (at least in outcome), so the error is a bit perplexing. The
#' behavior is not limited to R, problems are also seen in Python
#' where the \code{os.rename} method fails, but \code{shutil.move}
#' will work (\url{https://stackoverflow.com/a/15300474})
#'
#' Anyhoo. If renames are failing with \code{cross-device link}
#' messages, you're probably running into this issue. This method
#' detects the issue (based on a failure to copy and a grepl to
#' "Invalid cross.device link"), and attempts to solve the problem by
#' a copy-then-delete-source mechanism. Doing this task carefully was
#' more complex than initially anticipated. Source on GitHub has more
#' commentary (which get removed if you just evaluate
#' \code{file.rename2}) on why the code is the way it is.
#'
#' @param from Required, the current path to the file
#' @param to Required, the path you wish to rename/move the file to
#'
#' @return A single logical value, TRUE for success, FALSE for failure.
#'
#' @seealso \link[base]{file.rename}
#'
#' @examples
#'
#' \donttest{
#' from <- "/mnt/device1/foo.txt"
#' to   <- "/mnt/device2/foo.txt"
#' file.rename(from, to)  # Fails if /mnt/device1 and /mnt/device2 differ
#' file.rename2(from, to) # Yay! Works.
#' }
#' 
#' @export

file.rename2 <- function (from, to) {
    ## I haven't quite gotten a handle of the scope inside
    ## tryCatch. At a minimum, it's not the scope <here>. So we'll use
    ## get/assign to assure that we can manage the logical return
    ## value:
    rvName <- ".fr2rv" # Used to manage return value with assign/get
    assign(rvName, FALSE, envir=packEnv)
    tryCatch( {
        ## Wrap in tryCatch to 1) avoid halting execution and 2)
        ## capture and suppress message to not worry the user when we
        ## can succeed with fallback method
        assign(rvName, ok <- file.rename(from, to), envir=packEnv)
        
    }, warning = function(e) {
        ## Something went wrong. We expect this specific kind of error
        ## to come out as a warning, so snoop around here.
        ok <- get(rvName, envir=packEnv)
        if (ok) {
            ## Huh. It *reports* it was succesful, but is still
            ## emitting a warning. Trust it worked, but also pass on
            ## the warning to the user:
            warning(e)
        } else if (grepl("Invalid cross.device link", e, ignore.case=TRUE)) {
            ## This is the situation we wrote the method for. If that
            ## message ever changes we'll miss catching it here.

            ## INTERESTING FACT: file.rename() will fail with the
            ## "Invalid cross-device link" warning EVEN IF `from` DOES
            ## NOT EXIST. This library is really, REALLY worked up
            ## about even the THEORETICAL risk of moving files across
            ## state lines... So check that both the from file AND the
            ## target directory exist, and emit a more informative
            ## error if not:
            
            if (!file.exists(from)) {
                
                ## So. This is not really a cross-device issue, but
                ## instead a source-not-found issue that *THINKS* it's
                ## a cross-device issue. To be fair, it will *become*
                ## a cross-device issue as soon as the source can be
                ## found (on that device) *but* we'll be ready to
                ## handle that correctly if/when that happens.
                warning('In file.rename2("',from,'", "', to, '") :\n',
                        "  cannot rename file '",from,"' to '",to,
                        "', reason 'No such file or directory'")
                
            } else if (ok <- file.copy(from, to, overwrite=TRUE,
                                        copy.mode=TRUE, copy.date=TRUE )) {
                ## Since rename *will* clobber existing files, we are
                ## setting overwrite=TRUE to mirror that behavior

                ## Considered putting checksumming in here, decided I
                ## would trust file.copy to be competent when
                ## reporting success or failure. But I'll at least
                ## double-check that the 'to' exists:
                if (ok <- file.exists(to)) {
                    ## Yay. If we can also remove the source, call it
                    ## a succcess and return TRUE
                    ok <- file.remove(from)
                    if (!ok) { 
                        ## Booo. We couldn't get rid of the source
                        warning("file.rename2 - Destination successfully copied:\n    ", to,"\n  but source could not be removed:\n    ",from)
                    }
                } else {
                    warning("Attempt to rename file failed by both file.rename and file.copy\n   - BUT file.copy claims to have worked! (it didn't)")
                }
            } else {
                ## Copy warning *should* be emitted here. Also point
                ## out why we were copying the file when the user was
                ## just trying to rename it:
                warning("Attempt to rename file failed by both file.rename and file.copy")
            }
        } else {
            ## A problem, but not ours.
            warning(e)
        }
        assign(rvName, ok, envir=packEnv)
    })
    get(rvName, envir=packEnv)
}

#' Relative Path
#'
#' Reports the relative file path from a parent directory to a child object
#'
#' @details
#'
#' Given 'child' and 'parent' file paths, return the relative path
#' needed to reach the child from the parent, or \code{NA} if the
#' child is not a descendant of the parent.
#'
#' By default, neither child nor parent will be checked for existance,
#' or if they are an appropriate object. Both will have their paths
#' normalized via \code{normalizePath()}. If you wish to force
#' existance of both, set \code{mustWork=TRUE}.
#'
#' @param parent Required, the file path that presumably is an
#'     ancestor of the child in the directory structure. To return a
#'     non-NA value, this object presumably needs to resolve to a
#'     directory.
#' @param child Required, the file path of the "deeper" object (can be
#'     any component of the file system - file, directory, link, etc.
#' @param mustWork Default \code{FALSE}. Passed to normalizePath, set
#'     to TRUE if you wish to assure that both child and parent exist.
#' @param normChild Default \code{TRUE}, which will cause the child
#'     path to be normalized as well. This is not always desirable;
#'     For example, \code{normalizePath} will convert links to their
#'     ultimate target path. If you wish to leave links as-is, set
#'     normChild to FALSE.
#'
#' @return If either child or parent are any of \code{NULL}, \code{NA}
#'     or an empty string, then \code{NA}. If child is the same as
#'     parent (after normalization), an empty string. If child is not
#'     a descendant of the parent, \code{NA}. In all other cases, a
#'     single string representing the relative path.
#'
#' @seealso \code{\link[base]{normalizePath}}
#'
#' @examples
#'
#' relativePath("/tmp/RtmpaacRRB", "/tmp/RtmpaacRRB/output.txt")
#' relativePath(file.path(Sys.getenv('HOME'), "data"), "~/data/plots/x.png")
#' relativePath("/bin/bang/boom", "/bin/etc/etc/etc.txt")
#' relativePath("/usr/bin", "")
#' 
#' @export

relativePath <- function(parent, child, mustWork=FALSE, normChild=TRUE) {
    ## Reject NULL, NA and "" for either argument
    if (!is.something(child) || !is.something(parent))
        return(as.character(NA))
    parent <- normalizePath(parent, mustWork=mustWork)
    if (normChild) child  <- normalizePath(child, mustWork=mustWork)
    ## If these are the same, return an empty string
    if (parent == child) return("")
    ## We need a trailing slash. According to file.path the '/'
    ## separator will also be used on Windows systems, so we shouldn't
    ## need to determine if we should use '\' instead:
    if (!grepl('/$', parent)) parent <- paste0(parent, '/')
    ## Simply "subtract out" the parent from the 'front' of the
    ## child. We need to include the '^' front-of-string anchor, but
    ## want the rest of parent to be literal. Use \Q\E tokens instead
    ## of fixed=TRUE
    regExp   <- paste0('^\\Q',parent,'\\E')
    locChild <- gsub(regExp, "", child)
    if (locChild == child) {
        ## If child is unaltered, then it is not a descendant
        as.character(NA)
    } else {
        locChild
    }
}

