context("Test find_column")

test_that("Testing find_column", {
    ## Create fake data
    ##
    DT <- rbindlist(
        list(
            data.table(V1 = "Junk notes as first row", V2=NA, V3=NA, V4=NA),
            data.table("colA", "colB", "colC", "colD")
        ), fill=TRUE)
    expect_warning(expect_equal(find_column("does not exist", DT), list(nrow=0, ncol=0)))
    expect_equal(find_column("V3", DT), list(nrow=0, ncol=3))
    expect_equal(find_column("colA", DT), list(nrow=2, ncol=1))
})
