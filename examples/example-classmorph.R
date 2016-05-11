#### Create example data
dt <- data.table(Int     = as.factor(c(1:2, "-", 4:5)),
                 Double  = rep(2.0, 5),
                 Factor  = as.factor(c(1, "", 2, "", 3)),
                 nFactor = as.factor(rep(999, 5)),
                 Char    = letters[1:5])
print(dt)
sapply(dt, class) # check col classes

#### Change all Factors to Integers
ClassMorph(dt, "factor", "integer")
sapply(dt, class) # check col classes

#### Other options include:
#
# not modifying dt in place, rather generate a new copy of dt
ClassMorph(dt, "numeric", "factor", copy = TRUE)

# forcing conversion despite generate of NAs
ClassMorph(dt, "factor", "numeric", force = TRUE)
