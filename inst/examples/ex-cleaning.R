# Examples of: ClassMorph, cc, CleanCols, CleanRows, easyswap, Booleanize

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


#-------------------------------------------------------------------------------------
# Example: (c)heck (c)lasses
#-------------------------------------------------------------------------------------

cc(DT) 
# > cc(DT)
#         A           B           C           D           E           F           G           H 
# "integer"   "numeric"   "integer"   "integer" "character" "character"   "numeric"   "logical" 


#-------------------------------------------------------------------------------------
# Example: Clean columns or rows
#-------------------------------------------------------------------------------------

# Sometimes a set operation will result in a bunch of columns being all NAs. 
# It's sometimes useful to clean out these columns in very large tables.
# Then move on to cleaning out NA rows

cDT <- CleanCols(DT)

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

# we can swap out the remaining NAs if desired, though it's less common as typically 
# I avoid associating NAs with a valid entry to test against
#
# swaps out NA for FALSE by reference Set copy = TRUE to operate on a copy of the data

#easyswap(bDT, swap = "f")               # error bc "f" is not the same type as the col
easyswap(bDT, swap = "f", force = TRUE) # forces the col class to be same class as swap arg

cc(bDT)
# > cc(bDT)
#           A           B           C           D           E           F           G           H 
# "character"   "logical" "character"   "logical"   "logical"   "logical" "character" "character" 


# let's put false back in, but NOT by reference, and only on a subset of columns:
easyswap(bDT, 
         cols = c("G", "H"),
         find = "f", 
         swap = FALSE, 
         force = TRUE, 
         copy = TRUE)

# > easyswap(bDT, 
#            cols = c("G", "H"),
#            find = "f", 
#            swap = FALSE, 
#            force = TRUE, 
#            copy = TRUE)
# A     B     C     D     E     F     G     H
# 1:  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
# 2: FALSE FALSE     f FALSE FALSE FALSE FALSE FALSE
# 3:     f FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# 4: FALSE FALSE     f FALSE FALSE FALSE FALSE FALSE
# 5: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
