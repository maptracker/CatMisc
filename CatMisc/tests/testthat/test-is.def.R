library("CatMisc")

context("is defined")

test_that("NULL, NA and empty RefClass field", {
    expect_false(is.def(NULL), info="NULL")
    expect_false(is.def(NA), info="Single NA")
    expect_false(is.def(c(NA,NA)), info="Bunch of NAs")
    expect_true(is.def(c(NA,"puddle", NA)), info="Bunch of NAs plus non-na")
    Foo <- setRefClass("Foo", fields=list(x="ANY"))
    f   <- Foo()
    expect_false(is.def(f$x), info="RefClass object")
})

test_that("objects", {
    expect_true(is.def(data.frame()), info="empty data frame")
    expect_true(is.def(max), info="function (max)")
})

test_that("matrices", {
    expect_false( is.def(matrix(0,0,0)),  info="0x0 matrix")
    expect_true( is.def(matrix(0,2,0)),   info="2x0 matrix")
    expect_true( is.def(matrix(0,0,2)),   info="0x2 matrix")
    expect_true( is.def(matrix(1:4,2,2)), info="2x2 matrix")
})

test_that("things that are not NA", {
    expect_true( is.def(c(1L,NA,2L)),   info="integers")
    expect_true( is.def(c(1.1,NA,2.2)),   info="reals")
    expect_true( is.def(c("a","b",NA)),   info="characters")
    expect_true( is.def(c("")),   info="empty string")
})
