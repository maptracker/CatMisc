library("CatMisc")

test_that("Relative Path", {

    ## Build example by parts
    d <- "/foo/fam/fib"
    rp <- "bing/bang"
    c  <- file.path(d, rp)
    expect_identical(relativePath(d, c), rp, "Check basic path finding",
                     "Extracted relative path")

    expect_error(rp2 <- relativePath(d, c, mustWork=TRUE),
                 regexp="No such file or directory",
                 info="mustWork=TRUE should fail on fictional paths" )

    ## Check paths that are not related. We are using a
    ## character-classed version of NA:
    charNA <- as.character(NA)
    expect_identical(relativePath("/a/b/c", "/a/b/d/foo.txt"), charNA,
                     "Non descendants should yield NA",
                     "Unrelated paths")

    expect_identical(relativePath("/c/d/e", "/a/b/c/d/e/foo.txt"), charNA,
                     "Parent must match front of child path",
                     "Superficial path similarity")
    
    
    ## Not passing valid params
    expect_identical(relativePath(d, NA), charNA,
                     "NA child should yield NA")
    expect_identical(relativePath(d, ""), charNA,
                     "empty child should yield NA")
    expect_identical(relativePath(NA, d), charNA,
                     "NA parent should yield NA")
    expect_identical(relativePath("", d), charNA,
                     "empty parent should yield NA")

})
