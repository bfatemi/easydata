# # Now we are ready for the final act
# 
# # Let's take our list of finely cleaned and melted data, and let's merge them all
# # by COUNTRY, Quarter, and Year
# DT <- Reduce(merge, llres)
# 
# # Now let's melt one more time (long tables are much better suited for analysis)
# mDT <- melt(DT, id.vars = c("COUNTRY", "Quarter", "Year"))
# 
# # There is one important thing to remember. In our new "variable" column, we have two types of
# # measures. We have "Access" as a percentage, and "Speed" as Mbps. Let's identify that through
# # the use of rFilter:
# rFilter(mDT,val = "Access")
# a <- rFilter(mDT,val = "Access", set=list(Access = 1))
# b <- rFilter(mDT,val = "Speed",  set=list(Access = 0))
# 
# # put back together. Need to modify rFilter to "set" in place
# mDT <- rbindlist(list(a,b))
# 
# # Let's take a quick look all data points such that access was above 50 in Q1 09 and Q1 13
# splice(DT = mDT,
#        Quarter = "Q1",
#        Year = "09" & "13",
#        Access = 1,
#        value > 50)

