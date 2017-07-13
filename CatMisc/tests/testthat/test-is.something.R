library("CatMisc")

context("is something")

test_that("Non-unitary vectors", {
    expect_false(is.something(character()), "Empty vector")    
    expect_true(is.something(c(1,2)), "Length 2 vector")    
})

test_that("Single values", {
    expect_false(is.something(""), "Empty string")    
    expect_false(is.something(0.0), "0.0")    
    expect_false(is.something(0L), "Integer 0")    
    expect_true(is.something("0"), "String 0")    
    expect_true(is.something(TRUE), "TRUE")    
    expect_false(is.something(FALSE), "FALSE")    
    expect_false( is.something(matrix(0,0,0)),  "0x0 matrix")
    expect_false( is.something(matrix(0,2,0)),   "2x0 matrix")
    expect_false( is.something(matrix(0,0,2)),   "0x2 matrix")
    expect_true( is.something(matrix(1:4,2,2)),   "2x2 matrix")
})
