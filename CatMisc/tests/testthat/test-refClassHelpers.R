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
                     info="Method from parent class works")
    expect_true(rcObj$useColor(TRUE),
                info="Method from grandparent class works")

    rcObj$x   <- 5
    rcObj$txt <- "Yoo"
    
    expect_identical(rcObj$x, 5,
                     info="Primary field properly changed")
    expect_identical(rcObj$txt, "Yoo",
                     info="Inherited field properly changed")

    ## Make sure that help sections are bubbled-up from parents
    hs <- rcObj$getHelpSections()
    hsExp <- list(
        "Math Functions" = c("thingProduct"),
        "Text Functions" = c("thingText"),
        "Color Management" = c("useColor","colorize","colorMap"),
        "Help Utilities" = c("help", "annotateFields",
                             "fieldDescriptions", "getFieldDescriptions",
                             "helpSections","getHelpSections"),
        "SKIP" = c("initialize", ".selfVarName"))
    expect_identical(hs, hsExp,
                     info="Help sections are inherited from parent classes")

    ## Assure that field descriptions are similarly inheritted
    fd <- rcObj$getFieldDescriptions()
    fdExp <- list(
        "x"        = "A value stored by the Thing",
        "txt"      = "A string stored by the Stuff",
        "useCol"   = "Controls message colorization, set with $useColor()",
        "varName"  = "Extracted variable name associated with this object",
        "varCheck" = "Datestamp, last attempt to extract varName")
    expect_identical(fd, fdExp,
                     info="List descriptions are inherited from parent classes")
    

})

