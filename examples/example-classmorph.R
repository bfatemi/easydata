library(data.table)

#-------------------------------------------------------------------------------------
# Example: Change column classes in batch
#-------------------------------------------------------------------------------------

# Create example data
dt <- data.table(Int     = as.factor(c(1:2, "-", 4:5)),
                 Double  = rep(2.0, 5),
                 Factor  = as.factor(c(1, "garbage", 2, "", 3)),
                 nFactor = as.factor(rep(999, 5)),
                 Char    = letters[1:5],
                 Date    = rep("2020-01-22", 5))
dt
#-------------------------------------------------------------------------------------
# Errors when generating NA
#
# The following line will generate NAs because there is an element called 
# "garbage" in the column Factor. In fact, a missed non-numeric value in
# an otherwise mostly number column is the main reason the class automatically
# gets read as a factor. 
pclass(dt)
ClassMorph(dt, "factor", "integer")

#-------------------------------------------------------------------------------------
# The 'force' flag
#
# Naturally, when converting "garbage" to an integer, you will get an NA.
# To prevent accidental loss of data, the default behavior is to generate
# an error when conversion creates NAs. This should only be expected to 
# happen when going from factor to numeric/integer. In cases where NAs are expected,
# set the force flag = TRUE:
#
ClassMorph(dt, "factor", "integer", force = TRUE) # no error
pclass(dt)

#-------------------------------------------------------------------------------------
# The 'copy' flag
#
# Not modifying dt in place, rather generate a new copy of dt.
# Notice in the previous examples, ClassMorph had no return object. That is due 
# to the fact that ClassMorph was modifying the data in-place. Traditionally
# r has been a "copy-on-modify" language. That is typically not the case today,
# but in some cases it is desired. For example, if we want to be conservative 
# about accidental data loss, we would specify the flag 'copy=TRUE' and then
# manually delete the old table. 
#
newdt <- ClassMorph(dt, "numeric", "factor", copy = TRUE)

identical(cc(dt), cc(newdt))    # classes are not equal
pclass(dt)                      # confirm correct conversion
pclass(newdt)

#-------------------------------------------------------------------------------------
# IMPORTANT: Note that without specifying 'copy=TRUE', newdt is simply just 
# pointing to dt, as observed here:
#
pclass(dt)
newdt <- ClassMorph(dt, "numeric", "factor")

# dt and newdt are the same table in memory
identical(cc(dt), cc(newdt))
identical(dt, newdt)    




