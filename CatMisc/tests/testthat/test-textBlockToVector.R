library("CatMisc")

context("text block to vector")

test_that("parameter permutations", {
    
    expect_identical(c("quick", "brown", "fox"),
                     textBlockToVector("
quick
brown
fox
"), "Basic block of text")
    
    expect_identical(c("quick", "brown", "fox"),
                     textBlockToVector("
   quick
 brown
   fox  
"), "whitespace trimming")
    
    expect_identical(c("   quick", " brown", "   fox  "),
                     textBlockToVector("
   quick
 brown
   fox  
", trim.white=FALSE), "whitespace trimming off")

    expect_identical(c("quick", "brown", "fox"),
                     textBlockToVector("
quick


brown

fox
"), "Empty lines")

       expect_identical(c("", "quick", "", "", "brown", "", "fox"),
                     textBlockToVector("
quick


brown

fox
", skip.empty=FALSE), "Empty lines kept")

       expect_identical(c("quick", "brown", "fox"),
                     textBlockToVector("quick&  brown &fox", '\\s*&\\s*'),
                     "Alternate split token")
    
})
