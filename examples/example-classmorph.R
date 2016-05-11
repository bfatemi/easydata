#' @examples
#' 
#-------------------------------------------------------------
# Create example data
#-------------------------------------------------------------
dt <- data.table(Int     = as.factor(c(1:2, "-", 4:5)),
                 Double  = rep(2.0, 5),
                 Factor  = as.factor(c(1, "", 2, "", 3)),
                 nFactor = as.factor(rep(999, 5)),
                 Char    = letters[1:5])

print(dt)
#    Int Double Factor nFactor Char
# 1:   1      2      1     999    a
# 2:   2      2            999    b
# 3:   -      2      2     999    c
# 4:   4      2            999    d
# 5:   5      2      3     999    e

sapply(dt, class) # check col classes

#       Int      Double      Factor     nFactor        Char 
# "integer"   "numeric"    "factor"    "factor" "character" 

#-------------------------------------------------------------
# CHANGE ALL FACTORS TO NUMERICS
#-------------------------------------------------------------
ClassMorph(dt, "factor", "integer")

#    Int Double Factor nFactor Char
# 1:   2      2      2       1    a
# 2:   3      2      1       1    b
# 3:   1      2      3       1    c
# 4:   4      2      1       1    d
# 5:   5      2      4       1    e

sapply(dt, class) # check col classes

#       Int      Double      Factor     nFactor        Char 
# "integer"   "numeric"   "integer"   "integer" "character" 

#-------------------------------------------------------------
# Other options include:
#-------------------------------------------------------------

# not modifying dt in place, rather generate a new copy of dt
ClassMorph(dt, "numeric", "factor", copy = TRUE)

# forcing conversion despite generate of NAs
ClassMorph(dt, "factor", "numeric", force = TRUE)