# Now we want make our work reproducible and 
# apply it to the rest of the similarly structured tables

# let's functionalize the code we wrote and make minor tweaks for reusability

# give function a helpful name. Refer to a style guide or make your own, but be consistent

#cleanmelt for "clean" and "melt" which is what our function does
CleanMelt <- function(dt){
    print(names(dt))
    # make all integer and logical -> numeric
    ClassMorph(dt,"logical","numeric")
    ClassMorph(dt,"integer","numeric")
    
    # remove columns where ALL values are NA (last column)
    cRemoveNA(dt) 
    
    # make table long-form
    dtlong <- melt(dt, id.vars = "COUNTRY", 
                   variable.name = "Quarter")
    
    # because the new column "Quarter" was a mix of characters and numbers seperated by a space,
    # R converted them to factors. Convert all factors to characters
    ClassMorph(dtlong, "factor", "character")
    
    # Notice that column quarter also has year. lets split that up
    dtlong[, (c("Quarter", "Year")):=(as.data.table(do.call(rbind,strsplit(Quarter, " "))))]
    
    return(dtlong)
}

# place all of our functions in a list and use "lapply" to apply it
# good practice to name the list if we are holding similar structures with otherwise no
# distinguishing attributes
#
lldata <- list(AveSpeed_Mbps = dt_ave, 
               PeakSpeed_Mbps = dt_peak, 
               Access_Broadband = dt_bb, 
               Access_HighBroadband = dt_hbb, 
               Access_LowBroadband = dt_nbb)
llres <- lapply(lldata, CleanMelt)

# Notice how we have a column called "value" for each? Lets be more specific like our script,
# however we couldn't generalize it in our function because each table's "value" column should
# have a different name. Let's use the names we gave our data in the list as it describes the
# entries.

# write a simple loop to change a dt's column name from "value" to a given input
        # Note how I access elements of "llres" by name
for(i in names(llres))
    setnames(llres[[i]], "value", i)



