#-----------------------------------------------------------
# Testing efficiency for ClassMorph
#-----------------------------------------------------------

library(easydata)
library(data.table)

# ready in test data
DT <- readRDS("../usm/data/twotests_MeasureDT")
DT <- DT[1:1000000]

t <- Sys.time()
ClassMorph(DT, "factor", "character")
Sys.time() - t

