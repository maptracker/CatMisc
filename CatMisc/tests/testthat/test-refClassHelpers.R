library("CatMisc")

context("Reference Class Helpers")

rcObj <- NULL

test_that("Create toy RefClass Object", {
    rcObj <- myRefClassThing( x=17, txt="Hoo" )
              
    expect_identical(rcObj$x, 17,
                     info="Primary field is set")
    expect_identical(rcObj$txt, "Hoo",
                     info="Inherited field is set")

    expect_identical(rcObj$thingProduct(3), 51,
                     info="Primary method works")
    expect_identical(rcObj$thingText(prefix="Boo"), "Boo Hoo",
                     info="Inherited method works")

    rcObj$x   <- 5
    rcObj$txt <- "Yoo"
    
    expect_identical(rcObj$x, 5,
                     info="Primary field properly changed")
    expect_identical(rcObj$txt, "Yoo",
                     info="Inherited field properly changed")

})

test_that("objects", {
})

