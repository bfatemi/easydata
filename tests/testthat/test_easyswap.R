context("Test easyswap")

test_that("Testing easyswap", {
    # Create sample data.table
    dt <- data.table(A = c("A", NA, "A", "A", NA),
                     B = c(NA, "B", "B", "B", "B"),
                     C = rep(NA, 5),
                     D = c("D", "D", "D", NA, "D"))
    # Simple use
    cdt <- copy(dt)
    res <- easyswap(cdt, find="A", swap = "XXX")
    expect_equal(unique(cdt$A), c("XXX", NA))
    
    expect_error(easyswap(dt, swap = 0))
    
    e <- substitute(unique(pcc(dt, bret = TRUE)$Class))
    expect_equal(eval(e), c("character", "logical"))
    easyswap(dt, swap = 0, force = TRUE)
    expect_equal(eval(e), "character")
    
    
    # swap in 1 or more specific columns
    easyswap(dt,
             find = "0",
             swap = "999",
             cols = c("A", "B"))
    expect_equal(sort(unique(c(dt$A, dt$B)))[1], "999")
})


