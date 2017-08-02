
library("CatMisc")

test_that("methodHelp applied to sample object", {
    mrct <- myRefClassThing(5)
    expect_identical(mrct$thingProduct(3), 15,
                     "Toy object is intact")

    x <- mrct$thingProduct( help=TRUE )
    expect_identical(class(x)[1], "help_files_with_topic",
                     "Help topic recovered")
    
    expect_message(x2 <- myRefClassThing( help=TRUE ), NULL,
                   "Class-level help is a bit weird")
    expect_identical(class(x2)[1], "myRefClassThing",
                     "Class-level help still returns a class object!")
    expect_identical(length(x2$x), 0L,
                     "Calling help on a class should result in unformed object")

    expect_warning(x3 <- methodHelp("parrot", "tiger", "banana"), NULL,
                   "Undocumented methods should create a warning")
    expect_identical(x3, NA,
                   "Undocumented methods should return NA")

    
})
