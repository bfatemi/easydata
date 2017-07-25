context("Function xFormula")

test_that("Test of LHS Argument: 0-character string", {
    
    y <- ""
    x <- c("A", "B", "C", "D")
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})

test_that("Test of LHS Argument: NULL", {
    
    y <- NULL
    x <- c("A", "B", "C", "D")
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})


test_that("Test of LHS Argument: single var", {
    
    y <- "Y"
    x <- c("A", "B", "C", "D")
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})



test_that("Test of LHS Argument: mult var", {
    
    y <- c("X", "Y", "Z")
    x <- c("A", "B", "C", "D")
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})




# TEST OF RIGHT HAND SIDE VARIABLES ---------------------------------------


test_that("Test of RHS Argument: 0-character string", {
    
    y <- ""
    x <- ""
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})

test_that("Test of RHS Argument: NULL", {
    
    y <- NULL
    x <- NULL
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})


test_that("Test of RHS Argument: single var", {
    
    y <- c("X", "Y", "Z")
    x <- "A"
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})



test_that("Test of RHS Argument: mult var", {
    
    y <- c("X", "Y", "Z")
    x <- c("A", "B", "C", "D")
    res <- xFormula(y, x)
    expect_equal(class(res), "formula")
    
    
})


