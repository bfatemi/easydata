test_that("Test split_by_index", {
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