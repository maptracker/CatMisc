library("CatMisc")

context("is empty field")

test_that("empty RefClass ANY field", {
    Foo <- setRefClass("Foo", fields=list(x="ANY"))
    f   <- Foo()
    expect_true(is.empty.field(f$x), "Empty field slot")
    f$x <- 4
    expect_false(is.empty.field(f$x), "Assigned field slot")    
})

test_that("empty vectors", {
    expect_false(is.empty.field( integer(), zero.length.empty=FALSE),
                 "zero-length vector zero.length.empty=FALSE")
    expect_true(is.empty.field( integer(), zero.length.empty=TRUE),
                "zero-length vector zero.length.empty=TRUE")
    expect_false(is.empty.field( c(1L), zero.length.empty=FALSE),
                 "Populated vector zero.length.empty=FALSE")
    expect_false(is.empty.field( c(1L), zero.length.empty=TRUE),
                 "Populated vector zero.length.empty=TRUE")
})

test_that("Random things", {
    expect_false(is.empty.field( matrix(0,0,0)), "0x0 matrix")
    expect_false(is.empty.field( data.frame()), "data.frame")
})

