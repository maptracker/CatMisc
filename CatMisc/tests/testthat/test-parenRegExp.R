library("CatMisc")

context("parenthetical regular expression")

test_that("basic tests", {
    expect_identical(c("BK", "0139"),
                     parenRegExp("^([A-Z]+)([0-9]+)$", c("BK0139")),
                     "Multiple capture points, one string")
    expect_identical(c("bk", "0139"),
                     parenRegExp("^([A-Z]+)([0-9]+)$", c("bk0139")),
                     "Case-insensitive")
    expect_identical(NA,
                     parenRegExp("^([A-Z]+)([0-9]+)$", c("bk0139"),
                                 ignore.case=FALSE),
                     "Case-sensitive")
    
})


test_that("vectorization", {
    expect_identical(c("Dog", "cat", "rat", "dog"),
                     parenRegExp("(dog|cat|rat)",
                                 c("Dogstar","catty","prattle","good dog")),
                     "Unlisted results")
    expect_identical(list("Dog", "cat", "rat", "dog"),
                     parenRegExp("(dog|cat|rat)",
                                 c("Dogstar","catty","prattle","good dog"),
                                 unlist=FALSE),
                     "Listed results")
    
    expect_identical(list(c("big", "dog"), c("fat", "cat"), c("dirty", "rat")),
                     parenRegExp("(.+?)\\s*(dog|cat|rat)",
                                 c("big dog","fat cat","dirty rat"),
                                 unlist=FALSE),
                     "Listed results, multiple capture points")
    
})

