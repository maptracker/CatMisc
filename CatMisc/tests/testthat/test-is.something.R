library("CatMisc")

context("is something")

test_that("Non-unitary vectors", {
    expect_false(is.something(character()), info="Empty vector")    
    expect_true(is.something(c(1,2)), info="Length 2 vector")    
})

test_that("Single values", {
    expect_false(is.something(""), info="Empty string")
    expect_false(is.something(0.0), info="0.0")
    expect_false(is.something(0L), info="Integer 0")
    expect_true(is.something("0"), info="String 0")
    
    expect_true(is.something(max), info="function")
    expect_true(is.def(data.frame()), info="empty data frame")

    expect_true(is.something(TRUE), info="TRUE")
    expect_false(is.something(FALSE), info="FALSE")

    expect_false( is.something(matrix(0,0,0)),  info="0x0 matrix")
    expect_false( is.something(matrix(0,2,0)),   info="2x0 matrix")
    expect_false( is.something(matrix(0,0,2)),   info="0x2 matrix")
    expect_true( is.something(matrix(1:4,2,2)),  info="2x2 matrix")
})
