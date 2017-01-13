context("Test index functions")

test_that("Testing split_by_index", {
    vec <- c("a", "b", "c", " ", "d", " ", "e", "f", " ", "g", " ")
    index <- which(vec == " ")
    
    # expecting empty strings to be contained in the 'next set'
    r1 <- split_by_index(vec, index)
    lapply(r1[2:4], function(i)
        expect_equal(i[1], " "))
    
    # expecting the first index to define the initial element in the result
    r2 <- split_by_index(vec, index, include_first = FALSE)
    expect_equal(length(r2), 3)
    
    
    # expecting the first index to define the initial element in the result
    r3 <- split_by_index(vec, index, include_at_index = FALSE, include_first = FALSE)
    expect_equal(length(r3), 3)
    expect_equal(r3, list("d", c("e", "f"), "g"))
})

test_that("Testing make_index", {
    totalN <- 1000
    eachN <- 10
    
    # not simplified
    res <- make_index(totalN, eachN)
    expect_type(res, "list")
    expect_length(res, 10)
    expect_length(res[[10]], 100)
    expect_equal(res[[10]][100], 1000)
    
    # simplified
    res <- make_index(totalN, eachN, mat = TRUE)
    expect_equal(class(res), "matrix")
    expect_equal(ncol(res), 10)
    expect_equal(nrow(res), 100)
    expect_equal(res[100, 10], 1000)
})


test_that("Testing splitn", {
    n <- 22
    expect_length(splitn(1:1000, n), n)
})