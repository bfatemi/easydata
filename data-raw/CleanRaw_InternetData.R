library(data.table)

#-----------------------------------------------------------------------------------------
# DATA #1 
# 
# 2013 Q3 Internet Speed and ADOPTION Data from Akamai
#-----------------------------------------------------------------------------------------
avePeak_Q313 <- fread("inst/extdata/13Q3-avePeak.csv", key = "Country")
bb_Q313      <- fread("inst/extdata/13Q3-bb.csv", key = "Country")
hibb_Q313    <- fread("inst/extdata/13Q3-hibb.csv", key = "Country")
lowbb_Q313   <- fread("inst/extdata/13Q3-lowbb.csv", key = "Country")
uniqIP_Q313  <- fread("inst/extdata/13Q3-uniqueIP.csv", key = "Country")

# Merge point in time data to one dataset
DT_Broadband_Q313 <- avePeak_Q313[bb_Q313][lowbb_Q313][hibb_Q313][uniqIP_Q313]

# set new column names
cnames <- c("CountryCode", "PeakMbps", "Adoption_BB", "Adoption_LowBB", "Adoption_HighBB", "Count_UniqueIP")
setnames(DT_Broadband_Q313, cnames)

# Add date column as a reminder
DT_Broadband_Q313[, Year := 2013]
DT_Broadband_Q313[, Quarter := "Q3"]

setcolorder(DT_Broadband_Q313, c("CountryCode", "Year", "Quarter", cnames[-1]))


#-----------------------------------------------------------------------------------------
# DATA #2-3
# 
# GDP and Internet Speed Data Merged by County and MSA
#-----------------------------------------------------------------------------------------
GDP_FiberAccess_County <- fread("inst/extdata/BEA_NTIA_merged_COUNTY.csv")
GDP_FiberAccess_MSA    <- fread("inst/extdata/BEA_NTIA_merged_MSA.csv")

cnames_county <- c("Fips", "County", "State", "Year", "PersonalIncome_PerCap")
cnames_msa <- c("Fips", "Year", "Area", "GDP", "GDPGrowth", "MetroAreaSize", "PersonalIncome_PerCap")
cnames_both <- c("JobsCount", "JobsGrowth", "TYPE", "LandArea", "Population", 
                 "Households", "Income_Median", "Income_Less25", "Income_25to50", "Income_50to100",
                 "Income_100to200", "Income_greater200", "OpticalFiber", "advdl_gr1gig")
setnames(GDP_FiberAccess_MSA, c(cnames_msa, cnames_both))
setnames(GDP_FiberAccess_County, c(cnames_county, cnames_both))


#-----------------------------------------------------------------------------------------
# DATA #4
#
# Adoption by quarter country, for broadband, low broadband and high broadband
# Average speed and Peak speed by quarter country
#-----------------------------------------------------------------------------------------

# low/med/high adoption
adopt_BB     <- fread("inst/extdata/adopt_BB_QtrCountry.csv")
adopt_HighBB <- fread("inst/extdata/adopt_highBB_QtrCountry.csv")
adopt_LowBB  <- fread("inst/extdata/adopt_lowBB_QtrCountry.csv")

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

devtools::use_data_raw()
# AveSpeed/AvePeakSpeed
aveSpeed     <- fread("inst/extdata/aveSpeed_QtrCountry.csv")
avePeakSpeed <- fread("inst/extdata/avePeakSpeed_QtrCountry.csv")
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


DT_adoption      <- m.adopt_HighBB[m.adopt_LowBB][m.adopt_BB]
DT_Broadband_Qtr <- DT_adoption[m.aveSpeed][m.avePeakSpeed]

#-----------------------------------------------------------------------------------------
# SAVE THESE DATASETS
#-----------------------------------------------------------------------------------------

devtools::use_data(DT_Broadband_Q313)
devtools::use_data(DT_Broadband_Qtr)
devtools::use_data(GDP_FiberAccess_MSA)
devtools::use_data(GDP_FiberAccess_County)

fwrite(DT_Broadband_Q313, "data/DT_Broadband_Q313")
fwrite(DT_Broadband_Qtr, "data/DT_Broadband_Qtr")
fwrite(GDP_FiberAccess_MSA, "data/GDP_FiberAccess_MSA")
fwrite(GDP_FiberAccess_County, "data/GDP_FiberAccess_County")

rm(list = ls())
