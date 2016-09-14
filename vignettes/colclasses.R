## ------------------------------------------------------------------------
library(data.table)
library(easydata)

# Create example data
dt <- data.table(A = as.factor(c(1:2, "-", 4:5)),
                 B = rep(2.0, 5),
                 C = as.factor(c(1, "garbage", 2, "", 3)),
                 D = as.factor(rep(999, 5)),
                 E = letters[1:5],
                 F = rep("2020-01-22", 5))
dt

# print column classes
cc(dt) 

## ---- error=TRUE---------------------------------------------------------
ClassMorph(dt, "factor", "integer") #results in ERROR

## ------------------------------------------------------------------------
ClassMorph(dt, "factor", "integer", force = TRUE) # no error
cc(dt)

## ---- results='hold'-----------------------------------------------------
newdt <- ClassMorph(dt, "numeric", "factor", copy = TRUE)

identical(cc(dt), cc(newdt))    # classes are not equal
cc(dt)                      # confirm correct conversion
cc(newdt)

## ---- results='hold'-----------------------------------------------------
cc(dt)
newdt <- ClassMorph(dt, "numeric", "factor")

identical(cc(dt), cc(newdt))
identical(dt, newdt)  

