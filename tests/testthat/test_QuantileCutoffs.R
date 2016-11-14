context("Function QuantileCutoffs")

# ClassMorph()
# FilterRows()
# MorphStat()
# NumMorph()
# RemoveCols()
# cc()
# checkdt()
# describe()
# easysplit()
# easyswap()
# extract_date()
# fudate()
# pclass()
# plyStat()
# rFilter()
# rFilter_All()

test_that("Test function QuantileCutoffs", {
    # Use data provided in this package
    #  - change column class to numeric
    DT <- copy(fiberCountyDem)
    DT[, JobsCount := as.numeric(JobsCount)]
    
    values <- "JobsCount"
    grouping <- "State"
    quants <- c(.3, .7)
    
    # Set bMelt to false to receive a wide table for display purposes
    #
    res <- QuantileCutoffs(DT, values, grouping, quants, bMelt=FALSE)
    expect_equal(nrow(res), 51)
})