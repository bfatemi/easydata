library(data.table)

#-----------------------------------------------------------------------------------------
# DATA #1 
# 
# 2013 Q3 Internet Speed and ADOPTION Data from Akamai
#-----------------------------------------------------------------------------------------
avePeak_Q313 <- fread("data-raw/13Q3-avePeak.csv", key = "Country")
bb_Q313      <- fread("data-raw/13Q3-bb.csv", key = "Country")
hibb_Q313    <- fread("data-raw/13Q3-hibb.csv", key = "Country")
lowbb_Q313   <- fread("data-raw/13Q3-lowbb.csv", key = "Country")
uniqIP_Q313  <- fread("data-raw/13Q3-uniqueIP.csv", key = "Country")

# Merge point in time data to one dataset
broadband <- avePeak_Q313[bb_Q313][lowbb_Q313][hibb_Q313][uniqIP_Q313]

# set new column names
cnames <- c("CountryCode", "PeakMbps", "Adoption_BB", "Adoption_LowBB", "Adoption_HighBB", "Count_UniqueIP")
setnames(broadband, cnames)

# Add date column as a reminder
broadband[, Year := 2013]
broadband[, Quarter := "Q3"]

setcolorder(broadband, c("CountryCode", "Year", "Quarter", cnames[-1]))


#-----------------------------------------------------------------------------------------
# DATA #2-3
# 
# GDP and Internet Speed Data Merged by County and MSA
#-----------------------------------------------------------------------------------------
fiberGDP_County <- fread("data-raw/BEA_NTIA_merged_COUNTY.csv")
fiberGDP_MSA    <- fread("data-raw/BEA_NTIA_merged_MSA.csv")

cnames_county <- c("Fips", "County", "State", "Year", "PersonalIncome_PerCap")
cnames_msa <- c("Fips", "Year", "Area", "GDP", "GDPGrowth", "MetroAreaSize", "PersonalIncome_PerCap")
cnames_both <- c("JobsCount", "JobsGrowth", "TYPE", "LandArea", "Population", 
                 "Households", "Income_Median", "Income_Less25", "Income_25to50", "Income_50to100",
                 "Income_100to200", "Income_greater200", "OpticalFiber", "advdl_gr1gig")
setnames(fiberGDP_MSA, c(cnames_msa, cnames_both))
setnames(fiberGDP_County, c(cnames_county, cnames_both))


#-----------------------------------------------------------------------------------------
# DATA #4
#
# Adoption by quarter country, for broadband, low broadband and high broadband
# Average speed and Peak speed by quarter country
#-----------------------------------------------------------------------------------------

# low/med/high adoption
adopt_BB     <- fread("data-raw/adopt_BB_QtrCountry.csv")
adopt_HighBB <- fread("data-raw/adopt_highBB_QtrCountry.csv")
adopt_LowBB  <- fread("data-raw/adopt_lowBB_QtrCountry.csv")

m.adopt_BB <- melt.data.table(adopt_BB, 
                              id.vars = "COUNTRY", 
                              variable.name = "Period", 
                              value.name = "Adoption_BB")
m.adopt_HighBB <- melt.data.table(adopt_HighBB, 
                                  id.vars = "COUNTRY", 
                                  variable.name = "Period", 
                                  value.name = "Adoption_HighBB")
m.adopt_LowBB <- melt.data.table(adopt_LowBB, 
                                 id.vars = "COUNTRY", 
                                 variable.name = "Period", 
                                 value.name = "Adoption_LowBB")
setkeyv(m.adopt_BB, c("COUNTRY", "Period"))
setkeyv(m.adopt_HighBB, c("COUNTRY", "Period"))
setkeyv(m.adopt_LowBB, c("COUNTRY", "Period"))


# AveSpeed/AvePeakSpeed
aveSpeed     <- fread("data-raw/aveSpeed_QtrCountry.csv")
avePeakSpeed <- fread("data-raw/avePeakSpeed_QtrCountry.csv")

aveSpeed[, V27:=NULL]
m.aveSpeed <- melt.data.table(aveSpeed, 
                              id.vars = "COUNTRY", 
                              variable.name = "Period", 
                              value.name = "AveSpeed")
m.avePeakSpeed <- melt.data.table(avePeakSpeed, 
                                  id.vars = "COUNTRY", 
                                  variable.name = "Period", 
                                  value.name = "AvePeakSpeed")
setkeyv(m.aveSpeed, c("COUNTRY", "Period"))
setkeyv(m.avePeakSpeed, c("COUNTRY", "Period"))


DT_adoption   <- m.adopt_HighBB[m.adopt_LowBB][m.adopt_BB]
broadband_Qtr <- DT_adoption[m.aveSpeed][m.avePeakSpeed]

#-----------------------------------------------------------------------------------------
# SAVE THESE DATASETS
#-----------------------------------------------------------------------------------------

devtools::use_data(broadband)
devtools::use_data(broadband_Qtr)
devtools::use_data(fiberGDP_MSA)
devtools::use_data(fiberGDP_County)
# 
# fwrite(broadband, "data/DT_Broadband_Q313")
# fwrite(broadband_Qtr, "data/DT_Broadband_Qtr")
# fwrite(fiberGDP_MSA, "data/GDP_FiberAccess_MSA")
# fwrite(fiberGDP_County, "data/GDP_FiberAccess_County")
# 
rm(list = ls())
