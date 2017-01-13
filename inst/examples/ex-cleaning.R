# Examples of: ClassMorph, pcc, CleanCols, CleanRows, easyswap, Booleanize

#-------------------------------------------------------------------------------------
# Example: Change column classes in batch
#-------------------------------------------------------------------------------------

library(data.table)

DT <- data.table(A = as.factor(c(1:2, "-", 4:5)),
                 B = rep(2.0, 5),
                 C = as.factor(c(1, "garbage", 2, "", 3)),
                 D = as.factor(rep(999, 5)),
                 E = letters[1:5],
                 F = rep("2020-01-22", 5),
                 G = c(1, rep(NA, 4)),
                 H = NA)
DT
# > DT
#    A B       C   D E          F  G  H
# 1: 1 2       1 999 a 2020-01-22  1 NA
# 2: 2 2 garbage 999 b 2020-01-22 NA NA
# 3: - 2       2 999 c 2020-01-22 NA NA
# 4: 4 2         999 d 2020-01-22 NA NA
# 5: 5 2       3 999 e 2020-01-22 NA NA

# no error, and by reference. To operate on a copy, run with copy=TRUE
ClassMorph(DT, "factor", "integer", force = TRUE)
pcc(DT)

#-------------------------------------------------------------------------------------
# Example: Clean columns or rows
#
# Sometimes a set operation will result in a bunch of columns being all NAs and if
# there are many columns, it's useful to drop all of them:
#-------------------------------------------------------------------------------------
cDT <- CleanCols(DT) # Drop all NA columns

# Drop rows based on NA value in ANY column, or a subset of columns
CleanRows(cDT)               # remove all rows that have an NA value across any of the columns
CleanRows(cDT, "A")          # just in one column
CleanRows(cDT, c("A", "C"))  # subset of columns


#-------------------------------------------------------------------------------------
# Use Booleanize if you want to identify a particular value
#-------------------------------------------------------------------------------------

bDT <- Booleanize(DT, value = 1)

# > bDT
#        A     B     C     D     E     F    G  H
# 1:  TRUE FALSE  TRUE FALSE FALSE FALSE TRUE NA
# 2: FALSE FALSE    NA FALSE FALSE FALSE   NA NA
# 3:    NA FALSE FALSE FALSE FALSE FALSE   NA NA
# 4: FALSE FALSE    NA FALSE FALSE FALSE   NA NA
# 5: FALSE FALSE FALSE FALSE FALSE FALSE   NA NA


