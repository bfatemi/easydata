# Now we want make our work reproducible and 
# apply it to the rest of the similarly structured tables

# let's functionalize the code we wrote and make minor tweaks for reusability

# give function a helpful name. Refer to a style guide or make your own, but be consistent

#cleanmelt for "clean" and "melt" which is what our function does
CleanMelt <- function(dt){
    # make all integer and logical -> numeric
    ClassMorph(dt,"logical","numeric")
    ClassMorph(dt,"integer","numeric")
    
    # Demonstrate cRemove
    
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
llraw <- list(AveSpeed_Mbps = dt_ave, 
               PeakSpeed_Mbps = dt_peak, 
               Access_Broadband = dt_bb, 
               Access_HighBroadband = dt_hbb, 
               Access_LowBroadband = dt_nbb)
ll <- lapply(llraw, CleanMelt)

# Notice how we have a column called "value" for each? Lets be more specific like our script,
# however we couldn't generalize it in our function because each table's "value" column should
# have a different name. Let's use the names we gave our data in the list as it describes the
# entries.

# write a simple loop to change a dt's column name from "value" to a given input
        # Note how I access elements of "llres" by name
for(i in names(ll))
    setnames(ll[[i]], "value", i)

#-----------------------------------------
# MORPH DATA TO DESIRED FORM
#-----------------------------------------

# Keep as list seperated by measure variable type
ll

# Merge them together into a wider table
wideDT <- Reduce(merge, ll)

# Melt the wider table and add a column identifying the type of measure
longDT <- melt(DT, id.vars = c("COUNTRY", "Quarter", "Year"))

#-----------------------------------------
# EXPLORE THE DATA (IN VARIOUS FORMS)
#-----------------------------------------

# Demonstrate rFilter

# Since the data is in "long" form (much better for analysis), we need to be
# aware of a key nuance: in our new "variable" column, we have two types of
# measures. We have "Access" (a percentage), and "Speed" (Mbps). 
#
# filter rows for "Access" measures (does a fast text search accross columns)
rFilter(longDT, val = "Access")

# Lets say we quickly want to quickly create a new column that identifies
# whether the row corresponds to a percentage measure or a speed measure:
a <- rFilter(longDT, val = "Access", set = list(IsAccess=1))
b <- rFilter(longDT, val = "Speed", set = list(IsAccess=0))

longDT <- rbindlist(list(a,b))

# Demonstrate xSplit

# Let's take a quick look all data points such that access was 
# above 50 in 09 
splice(DT=longDT, 
       Year = "09",
       Quarter = "Q1" & "Q4",
       Access = 1, 
       value > 50)

# Two new areas in Q4 and also Delaware access grew by 5% in 2009

# Let's try the same thing on the wide view. This time, we can apply
# a statistics function to a different column after the splice:

# For the same cut of data answer this question:

# What is the average mbps speed in places where broadband access is 75% at 
# the beginning and end of 2011
splice(DT = wideDT,
       Quarter = "Q1" & "Q4",
       Year = "11",
       Access_Broadband > 75,
       apply = mean(AveSpeed_Mbps))

# Could there be a relationship between access and speed? Makes sense:
# the wider availability could bring more competition among internet providers
# to offer higher speeds. I'm assuming a lot so entertain me by not pointing
# out the shoddy conclusions

# Let's investigate just a bit further and see if the same thing is happeing 
# as a general trend aggregating all years:
splice(DT = wideDT,
       Quarter = "Q1" & "Q4",
       Access_Broadband > 75,
       apply = mean(AveSpeed_Mbps))

# How about between years (aggregating the quarters)
splice(DT = wideDT,
       Year = "10" & "11" & "12" & "13",
       Access_Broadband > 75,
       apply = mean(AveSpeed_Mbps))



