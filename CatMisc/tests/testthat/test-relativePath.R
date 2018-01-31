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

    ## Check paths that are not related
    expect_identical(relativePath("/a/b/c", "/a/b/d/foo.txt"), NA,
                     "Non descendants should yield NA",
                     "Unrelated paths")

    expect_identical(relativePath("/c/d/e", "/a/b/c/d/e/foo.txt"), NA,
                     "Parent must match front of child path",
                     "Superficial path similarity")
    
    
    ## Not passing valid params
    expect_identical(relativePath(d, NA), NA,
                     "NA child should yield NA")
    expect_identical(relativePath(d, ""), NA,
                     "empty child should yield NA")
    expect_identical(relativePath(NA, d), NA,
                     "NA parent should yield NA")
    expect_identical(relativePath("", d), NA,
                     "empty parent should yield NA")

})
