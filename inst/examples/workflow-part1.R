# library(data.table)
# 
# # Internet Data by Country and Quarter
# dt_ave  <- fread("exampledata/AkamaiData/avg_connection_speed_country_wise.csv")
# dt_peak <- fread("exampledata/AkamaiData/avg_peak_connection_speed_country_wise.csv")
# dt_bb   <- fread("exampledata/AkamaiData/broadband_adoption_country_wise.csv")
# dt_hbb  <- fread("exampledata/AkamaiData/high_broadband_adoption_country_wise.csv")
# dt_nbb  <- fread("exampledata/AkamaiData/narrowband_adoption_country_wise.csv")
# 
# # # Point in time data by country code for Q3 2013
# # fread("exampledata/AkamaiData/Q3 2013/avePeak.csv")
# # fread("exampledata/AkamaiData/Q3 2013/uniqueIP.csv")
# # fread("exampledata/AkamaiData/Q3 2013/bb.csv")
# # fread("exampledata/AkamaiData/Q3 2013/hibb.csv")
# # fread("exampledata/AkamaiData/Q3 2013/lowBandwidthAdoption.csv")
# 
# # check classes
# sapply(dt_ave,class)
# 
# # make all integer and logical -> numeric
# #
# ClassMorph(dt_ave,"logical","numeric")
# ClassMorph(dt_ave,"integer","numeric")
# 
# # remove columns where ALL values are NA (last column)
# cRemoveNA(dt_ave) 
# 
# # make table long-form
# dtlong <- melt(dt_ave, id.vars = "COUNTRY", 
#                variable.name = "Quarter", 
#                value.name = "AveSpeed_Mbps")
# 
# # because the new column "Quarter" was a mix of characters and numbers seperated by a space,
# # R converted them to factors. Convert all factors to characters
# ClassMorph(dtlong, "factor", "character")
# 
# # Notice that column quarter also has year. lets split that up
# dtlong[, (c("Quarter", "Year")):=(as.data.table(do.call(rbind,strsplit(Quarter, " "))))]




