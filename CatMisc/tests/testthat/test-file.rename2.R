library("CatMisc")

test_that("File Rename 2", {

    ## The main value here is to rename files across device
    ## boundaries. Since that's fundamentally dependant on the
    ## system's hardware (I can't rely on the builder's system having
    ## two or more devices), I don't know a reliable way to check that
    ## part of the feature on arbitrary systems. So I'll check the
    ## basic features that just replicate file.rename()

    d <- tempdir()
    f1 <- file.path(d, "fileRenameTest1")
    f2 <- file.path(d, "fileRenameTest2")

    if (file.exists(f1)) file.remove(f1)
    if (file.exists(f2)) file.remove(f2)

    ## Sanity set up checks
    expect_false(file.exists(f1), "Preliminary files should be absent")
    expect_false(file.exists(f2), "Preliminary files should be absent")

    msg <- "This is a test file, feel free to delete it"
    cat(msg, file=f1)
    expect_true(file.exists(f1), "Test file should be created")
    
    ## Basic functionality
    expect_true(x <- file.rename2(f1, f2), "Basic file rename")
    expect_true(x, "Succesful rename should return TRUE")
    expect_false(file.exists(f1), "Original file name should be absent")
    expect_true(file.exists(f2), "New file name should be present")

    roundTrip <- readLines(f2, warn=FALSE)
    expect_identical(roundTrip, msg,
                     "Renamed file should have same content as original")

    ## Proper errors generated
    bogus <- file.path(d, "sirFileNotExistingInThisMovie.txt")
    expect_false(file.exists(bogus), "Test file name should not exist")
    
    expect_warning(x <- file.rename2(bogus, f1),
                   regexp="No such file or directory",
                   "Renaming missing file should fail")
    expect_false(x, "Failed rename should return FALSE")
    expect_false(file.exists(f1), "Original file should STILL be absent")
    
    expect_warning(x <- file.rename2(bogus, f2),
                   regexp="No such file or directory",
                   "Renaming missing file should fail")
    expect_true(file.exists(f2), "New file name should STILL be present")
    roundTrip <- readLines(f2, warn=FALSE)
    expect_identical(roundTrip, msg,
                     "Attempts to rename a non-existant file should not damage existing target")
   
})
