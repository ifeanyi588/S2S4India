## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 80, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  mc.cores = 6, ## set number of (mac) cores used in parallel processing
  start.time= Sys.time()
)
#### Load Libraries
library(readstata13)
library(data.table)
library(stringr)
library(questionr)
library(openxlsx)
library(psych)
library(stringdist)
library(tidyverse)
#library(xlsx)

#make sure to set the working directory to the project folder
#### always start by setting the working directory to the base project folder
p<-"C:/Users/wb559885/OneDrive - WBG/Documents/WorldBankProjects/INDIA/PovertyImputation2018/IndiaPoverty2018"
setwd(p)
for2014rainfall <- paste(p,"IND_2014_R71250H_v1_M/Data/R",sep="/")

## load functions from RFunctions.R file
source("IND_2018_R75250H_v1_M/Code/RFunctions.R")

### read in all the health dta files from the relevant folder

setwd("IND_2018_R75250H_v1_M/Data/STATA")
nsshealth18 <- Select.DTA(find.pattern = "R75250L[0-9][0-9]h.dta")

for (i in seq_along(nsshealth18)) {
  as.data.table(assign(paste("R75250L", i, "h", sep = ""), value = nsshealth18[[i]]))
}

rm(nsshealth18) # we dont need the data anymore


#now we compute some checks to be compared to the results from the SCE health reports
### read in the CPI data from the education space 
cpi.dt <- fread("../R/CPIData.csv")

## compute HH per capita expenditure
### compute deflator by sector and subround
cpi.dt <- cpi.dt[,mean(deflator),by = .(sector, subround)]

R75250L1h[,subround := var11]
R75250L1h[,subround := as.integer(subround)]
R75250L1h[,sector := as.integer(var6)]

R75250L1h[,hhid := substr(v1, 4, 34)]
R75250L2h[,hhid := substr(v1, 4, 34)]

setkey(R75250L1h, hhid)
setkey(R75250L2h, hhid)

R75250L1h[,district.code := var8]
R75250L1h[,qhclust := as.integer(var2)]

##create the district codes which are a combination and state and district
R75250L1h[,state := substr(var7, 1, 2)]

R75250L1h[,district.code := paste(state, district.code, sep = "")]

R75250L1h[,sector := ifelse(sector == 1, "RURAL", 
                            ifelse(sector == 2, "URBAN", "NA"))]
R75250L2h <- R75250L2h[R75250L1h[,c("hhid", "subround", "sector", "district.code", "qhclust", "state")]]

setkey(cpi.dt, subround, sector)
setkey(R75250L2h, subround, sector)
colnames(cpi.dt) <- c("subround", "sector", "deflator")

R75250L2h <- R75250L2h[cpi.dt]

#now compute real expenditure values with the deflator
R75250L2h[,real.hhexp := as.numeric(var19)*deflator]
R75250L2h[,hhsize := as.integer(var4)]
R75250L2h[,real.hhexp.pcap := real.hhexp/hhsize]
R75250L2h[,lnreal.hhexp.pcap := log(real.hhexp.pcap)]

R75250L2h[,MULT := as.numeric(var23)]

#re-compute household level weights
R75250L2h[var21 == var22,hhwt := MULT/100]
R75250L2h[var21 != var22,hhwt := MULT/200]

R75250L2h[,pop_wgt := hhwt*hhsize]


umpce.unw <- R75250L2h[,mean(lnreal.hhexp.pcap, na.rm = TRUE),by = sector]
umpce <- R75250L2h[,wtd.mean(lnreal.hhexp.pcap, weights = pop_wgt),by = sector]

#compute 2011 USD PPP equivalent for the consumption expenditures
R75250L2h[sector == "URBAN",yearcpi := 16.017724]
R75250L2h[sector == "RURAL",yearcpi := 13.173453]

R75250L2h[,umpce.ppp := real.hhexp.pcap/yearcpi]
umpce.ppp <- R75250L2h[,wtd.mean(umpce.ppp, weights = pop_wgt),by=sector]

R75250L2h[,lnumpce.ppp := log(umpce.ppp)]
R75250L2h[is.na(lnumpce.ppp) == TRUE | lnumpce.ppp == -Inf, lnumpce.ppp := log(1)]
lnumpce.ppp <- R75250L2h[,wtd.mean(lnumpce.ppp, weights = pop_wgt),by=sector]


## estimating proportion of hhs by household size
hhsize.str <- R75250L2h[,as.data.table(prop.table(wtd.table(hhsize, weights = pop_wgt))), by = sector]
colnames(hhsize.str) <- c("sector", "hhsize", "rates")
hhsize.str.unw <- R75250L2h[,as.data.table(prop.table(table(hhsize))), by = sector]
colnames(hhsize.str.unw) <- c("sector", "hhsize", "rates")

# estimating hhsize prevalence by district household size
#weighted
dist.hhsize <- R75250L2h[,as.data.table(prop.table(wtd.table(hhsize, weights = pop_wgt))), 
                         by = .(district.code, sector)]
colnames(dist.hhsize) <- c("district.code", "sector", "hhsize", "rates")

l2.wgts <- R75250L2h[,sum(pop_wgt),by=.(district.code, sector)]
dist.hhsize <- dist.hhsize[l2.wgts, on = .(district.code, sector)]
dist.hhsize <- dist.hhsize[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, hhsize)]
colnames(dist.hhsize) <- c("sector", "hhsize", "rates")

#unweighted
dist.hhsize.unw <- R75250L2h[,as.data.table(prop.table(table(hhsize))), by = .(district.code, sector)]
colnames(dist.hhsize.unw) <- c("district.code", "sector", "hhsize", "rates")
dist.hhsize.unw <- dist.hhsize.unw[,as.data.table(mean(rates)), by = .(sector, hhsize)]
colnames(dist.hhsize.unw) <- c("sector", "hhsize", "rates")

## compute the household age structure
R75250L3h[,MULT := as.numeric(var24)]
R75250L3h[,hhid := substr(v1, 4, 34)]

setkey(R75250L3h, hhid)
setkey(R75250L1h, hhid)

R75250L3h <- R75250L3h[R75250L1h[,c("hhid", "sector", "district.code", "subround")]]
R75250L3h[,age := as.numeric(var7)]
R75250L3h[,MULT := as.numeric(var24)] #turn the weights into a numeric vector 
R75250L3h[,age.group := cut(age, breaks = c(0,15,24,34,49,64,150), 
                            labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"),
                            include.lowest = TRUE)]
R75250L3h[var22 == var23,hhwt := MULT/100]
R75250L3h[var22 != var23,hhwt := MULT/200]
age.str.unw <- R75250L3h[,as.data.table(prop.table(table(age.group))), by=sector]
colnames(age.str.unw) <- c("sector", "age.group", "rates")
age.str <- R75250L3h[,as.data.table(prop.table(wtd.table(age.group, weights = hhwt))), by=sector]
colnames(age.str) <- c("sector", "age.group", "rates")

## compute the household age structure by district
#weighted 
##include district identifier first
dist.age.str <- R75250L3h[,as.data.table(prop.table(wtd.table(age.group, weights = hhwt))), 
                           by = .(district.code, sector)]
colnames(dist.age.str) <- c("district.code", "sector", "age.group", "rates")

l4.wgts <- R75250L3h[,sum(hhwt),by=.(district.code, sector)]
dist.age.str <- dist.age.str[l4.wgts, on = .(district.code, sector)]
dist.age.str <- dist.age.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, age.group)]
colnames(dist.age.str) <- c("sector", "age.group", "rates")

#unweighted
dist.age.str.unw <- R75250L3h[,as.data.table(prop.table(table(age.group))), by = .(district.code, sector)]
colnames(dist.age.str.unw) <- c("district.code", "sector", "age.group", "rates")
dist.age.str.unw <- dist.age.str.unw[,as.data.table(mean(rates)), by = .(sector, age.group)]
colnames(dist.age.str.unw) <- c("sector", "age.group", "rates")


#compute the religion and social group breakdowns
R75250L2h[,religion := ifelse(var9 == 1, "Hinduism",
                             ifelse(var9 == 2, "Islam", 
                                    ifelse(var9 == 3, "Christianity",
                                           ifelse(var9 == 4, "Sikhism", 
                                                  ifelse(var9 == 5, "Jainism", 
                                                         ifelse(var9 == 6, "Buddhism", 
                                                                ifelse(var9 == 7, "Zoroastrianism",
                                                                       ifelse(var9 == 9, "Others", "NA"))))))))]

rel.str.unw <- R75250L2h[,as.data.table(prop.table(table(religion))), by = sector]
colnames(rel.str.unw) <- c("sector", "religion", "rates")
rel.str <- R75250L2h[,as.data.table(prop.table(wtd.table(religion, weights = pop_wgt))), by = sector]
colnames(rel.str) <- c("sector", "religion", "rates")

#district level estimation for religion
dist.rel.str <- R75250L2h[,as.data.table(prop.table(wtd.table(religion, weights = pop_wgt))), 
                              by = .(district.code, sector)]
colnames(dist.rel.str) <- c("district.code", "sector", "religion", "rates")
#weighted
dist.rel.str <- dist.rel.str[l2.wgts, on = .(district.code, sector)]
dist.rel.str <- dist.rel.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, religion)]
colnames(dist.rel.str) <- c("sector", "religion", "rates")

#unweighted
dist.rel.str.unw <- R75250L2h[,as.data.table(prop.table(table(religion))), by = .(district.code, sector)]
colnames(dist.rel.str.unw) <- c("district.code", "sector", "religion", "rates")
dist.rel.str.unw <- dist.rel.str.unw[,as.data.table(mean(rates)), by = .(sector, religion)]
colnames(dist.rel.str.unw) <- c("sector", "religion", "rates")


#compute social group breakdowns
R75250L2h[,social.group := ifelse(var10 == 1, "scheduled tribe", 
                                 ifelse(var10 == 2, "scheduled caste",
                                        ifelse(var10 == 3, "backward class",
                                               ifelse(var10 == 9, "others", "NA"))))]

soc.grp.str.unw <- R75250L2h[,as.data.table(prop.table(table(social.group))),by=sector]
colnames(soc.grp.str.unw) <- c("sector", "social.group", "rates")
soc.grp.str <- R75250L2h[,as.data.table(prop.table(wtd.table(social.group, weights = pop_wgt))),by=sector]
colnames(soc.grp.str) <- c("sector", "social.group", "rates")

#compute social group breakdowns (district level means)
##include district identifier first
dist.soc.grp.str <- R75250L2h[,as.data.table(prop.table(wtd.table(social.group, weights = pop_wgt))), 
                             by = .(district.code, sector)]
colnames(dist.soc.grp.str) <- c("district.code", "sector", "social.group", "rates")
#weighted
dist.soc.grp.str <- dist.soc.grp.str[l2.wgts, on = .(district.code, sector)]
dist.soc.grp.str <- dist.soc.grp.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, social.group)]
colnames(dist.soc.grp.str) <- c("sector", "social.group", "rates")

#unweighted
dist.soc.grp.str.unw <- R75250L2h[,as.data.table(prop.table(table(social.group))), by = .(district.code, sector)]
colnames(dist.soc.grp.str.unw) <- c("district.code", "sector", "social.group", "rates")
dist.soc.grp.str.unw <- dist.soc.grp.str.unw[,as.data.table(mean(rates)), by = .(sector, social.group)]
colnames(dist.soc.grp.str.unw) <- c("sector", "social.group", "rates")

## compute by household type
R75250L2h[sector == "RURAL",hhtype := ifelse(var8 == 1, "Self-Employed in Agriculture",
                                            ifelse(var8 == 2, "Self-Employed in Non-Agriculture",
                                                   ifelse(var8 == 3, "Regular Wage/Salary Earning in Agriculture",
                                                          ifelse(var8 == 4, "Regular Wage/Salary Earning in Non-Agriculture",
                                                                 ifelse(var8 == 5, "Casual Labour in Agriculture",
                                                                        ifelse(var8 == 6, "Casual Labour in Non-Agriculture",
                                                                               ifelse(var8 == 9, "Others", "NA")))))))]
R75250L2h[sector == "URBAN", hhtype := ifelse(var8 == 1, "Self-Employed", 
                                             ifelse(var8 == 2, "Regular Wage/Salary Earning",
                                                    ifelse(var8 == 3, "Casual Labour",
                                                           ifelse(var8 == 9, "Others", "NA"))))]
hhtype.str.unw <- R75250L2h[,as.data.table(prop.table(table(hhtype))),by=sector]
colnames(hhtype.str.unw) <- c("sector", "hhtype", "rates")
hhtype.str <- R75250L2h[,as.data.table(prop.table(wtd.table(hhtype, weights = pop_wgt))),by=sector]
colnames(hhtype.str) <- c("sector", "hhtype", "rates")

#district level tabulation of household type
#weighted
dist.hhtype.str <- R75250L2h[,as.data.table(prop.table(wtd.table(hhtype, weights = pop_wgt))), 
                             by = .(district.code, sector)]
colnames(dist.hhtype.str) <- c("district.code", "sector", "hhtype", "rates")
dist.hhtype.str <- dist.hhtype.str[l2.wgts, on = .(district.code, sector)]
dist.hhtype.str <- dist.hhtype.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, hhtype)]
colnames(dist.hhtype.str) <- c("sector", "hhtype", "rates")

#unweighted
dist.hhtype.str.unw <- R75250L2h[,as.data.table(prop.table(table(hhtype))), by = .(district.code, sector)]
colnames(dist.hhtype.str.unw) <- c("district.code", "sector", "hhtype", "rates")
dist.hhtype.str.unw <- dist.hhtype.str.unw[,as.data.table(mean(rates)), by = .(sector, hhtype)]
colnames(dist.hhtype.str.unw) <- c("sector", "hhtype", "rates")


## principal industry (we need to figure out which ones classify as agriculture and industry)
# R75250L2h[,table(var6)]
# R75250L2h[,table(var7)]

## principal industry
R75250L2h[,var6 := as.integer(var6)]
R75250L2h[,principal.industry := cut(x = var6, breaks = c(0,3229,43900,99000),
                                    labels = c("Agriculture", "Industry", "Others"))]
prin.ind.str <- R75250L2h[,as.data.table(prop.table(wtd.table(principal.industry, weights = pop_wgt))),
                          by=sector]
colnames(prin.ind.str) <- c("Sector", "Principal.Industry", "rates")
prin.ind.str.unw <- R75250L2h[,as.data.table(prop.table(table(principal.industry))),by=sector]
colnames(prin.ind.str.unw) <- c("Sector", "Principal.Industry", "rates")


## district mean for principal industry tabulation
dist.prin.ind.str <- R75250L2h[,as.data.table(prop.table(wtd.table(principal.industry, 
                                                                  weights = pop_wgt))),
                               by=.(sector, district.code)]
colnames(dist.prin.ind.str) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str <- dist.prin.ind.str[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.prin.ind.str <- dist.prin.ind.str[,wtd.mean(rates, weights = V1), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str) <- c("Principal.Industry", "Sector", "rates")
##unweighted
dist.prin.ind.str.unw <- R75250L2h[,as.data.table(prop.table(table(principal.industry))),by=.(sector, district.code)]
colnames(dist.prin.ind.str.unw) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str.unw <- dist.prin.ind.str.unw[,mean(rates), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str.unw) <- c("Principal.Industry", "Sector", "rates")

## begin preparing variables for the imputation
##### for household size
R75250L2h[,dhhsizecat := cut(x = hhsize, breaks = c(0, 2, 3, 4, 5, 6, 1000))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(dhhsizecat)])
old.names <- c("(6,1e+03]","(4,5]", "(3,4]", "(2,3]", "(0,2]")
new.names <- c("Dhhsizecat6h","Dhhsizecat5h", "Dhhsizecat4h", "Dhhsizecat3h", "Dhhsizecat2h")
setnames(R75250L2h, old = old.names, new = new.names)

R75250L2h[,c("Dhhsizecat4", "Dhhsizecat5", "Dhhsizecat3", "Dhhsizecat2", "Dhhsizecat6") := 
            list(wtd.mean(Dhhsizecat4h, weights = pop_wgt), wtd.mean(Dhhsizecat5h, weights = pop_wgt),
                 wtd.mean(Dhhsizecat3h, weights = pop_wgt), wtd.mean(Dhhsizecat2h, weights = pop_wgt),
                 wtd.mean(Dhhsizecat6h, weights = pop_wgt)), 
          by = .(district.code)]
##### for age groups
R75250L3h[,age.group2 := cut(age, breaks = c(0,14,24,34,49,64,150),
                             labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"))]
R75250L3h[age == 0,age.group2 := "0-14"]
R75250L3h <- cbind(R75250L3h, R75250L3h[,dummy.code(age.group2)])
old.names <- c("0-14", "15-24", "25-34", "35-49", "50-64", "65+")
new.names <- c("Dagecat1h", "Dagecat2h", "Dagecat3h", "Dagecat4h", "Dagecat5h", "Dagecat6h")
setnames(R75250L3h, old = old.names, new = new.names)
#compute proportion of HH members within each age group by household

hhage.str <- 
  R75250L3h[,list(mean(Dagecat1h, na.rm = TRUE), mean(Dagecat2h, na.rm = TRUE), mean(Dagecat3h, na.rm = TRUE),
                  mean(Dagecat4h, na.rm = TRUE), mean(Dagecat5h, na.rm = TRUE), mean(Dagecat6h, na.rm = TRUE)), 
            by = hhid]
colnames(hhage.str) <- c("hhid", "Dagecat1h", "Dagecat2h", "Dagecat3h", "Dagecat4h", "Dagecat5h", "Dagecat6h")
#merge back into a household level file
R75250L2h <- R75250L2h[hhage.str, on = "hhid"]

R75250L2h[,c("Dagecat1", "Dagecat2", "Dagecat3", "Dagecat4", "Dagecat5", "Dagecat6") := 
            list(wtd.mean(Dagecat1h, weights = pop_wgt), wtd.mean(Dagecat2h, weights = pop_wgt),
                 wtd.mean(Dagecat3h, weights = pop_wgt), wtd.mean(Dagecat4h, weights = pop_wgt),
                 wtd.mean(Dagecat5h, weights = pop_wgt), wtd.mean(Dagecat6h, weights = pop_wgt)),
          by = .(district.code)]

##### for religion and social caste
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(religion)])
R75250L2h[,hinduh := Hinduism]
R75250L2h[,hindu := wtd.mean(hinduh, weights = pop_wgt), by = .(district.code)]
R75250L2h[,scsth := ifelse(social.group %in% c("backward class", "scheduled caste", "scheduled tribe"), 1, 0)]
R75250L2h[,scst := wtd.mean(scsth, weights = pop_wgt), by = .(district.code)]

##### for the household types (self employment, casual labor (only in urban areas), regular wage)
R75250L2h[,hhtype.new := ifelse(grepl("Self-Employed", x = hhtype), "selfemployedh",
                                ifelse(grepl("Casual Labour", x = hhtype), "casualurbanh",
                                       ifelse(grepl("Regular Wage", x = hhtype), "regwageurbanh", "otherh")))]

R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(hhtype.new)]) #creating the dummies for each level of hhtype.new
R75250L2h[,c("selfemployed", "casualurban", "regwageurban", "other") := 
            list(wtd.mean(selfemployedh, weights = pop_wgt), wtd.mean(casualurbanh, weights = pop_wgt),
                 wtd.mean(regwageurbanh, weights = pop_wgt), wtd.mean(otherh, weights = pop_wgt)),
          by = district.code]

##### occupation levels (middle and high skill)
R75250L2h[,occ.class := as.integer(substr(var7, 1, 1))]
R75250L2h[,occ.level := ifelse(occ.class == 1| occ.class == 2| occ.class == 3, "highskillocch",
                               ifelse(occ.class == 4| occ.class == 5, "middleskillocch",
                                      ifelse(occ.class == 6| occ.class == 7| occ.class == 8| occ.class == 9, 
                                             "lowskillocch", "othersh")))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(occ.level)])
R75250L2h[,c("highskillocc", "middleskillocc", "lowskillocc") := 
            list(wtd.mean(highskillocch, weights = pop_wgt), wtd.mean(middleskillocch, weights = pop_wgt),
                 wtd.mean(lowskillocch, weights = pop_wgt)), by = district.code]

##### industry levels
R75250L2h[,industry := ifelse(grepl("Agric", principal.industry), "agrih",
                              ifelse(grepl("Ind", principal.industry), "indh",
                                     ifelse(grepl("Other", principal.industry), "serviceh", "NA")))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(industry)])
R75250L2h[,c("agri", "ind", "service") := 
            list(wtd.mean(agrih, weights = pop_wgt), wtd.mean(indh, weights = pop_wgt),
                 wtd.mean(serviceh, weights = pop_wgt)),
          by = district.code]


## compute the rainfall shock variables
##### first read in the data
rainfall.dt <- as.data.table(read.dta13("data_chirps.dta"))
#drop the weird india-china l1 names
rainfall.dt <- rainfall.dt[!grepl("India/China", l1_name),]

## include district data from Sutirtha
nss.dist.code <- as.data.table(read.dta13("district_nss_census_master.dta"))
nss.dist.code[,state := as.character(state)]
nss.dist.code[nchar(state) == 1,state := paste("0", state, sep = "")]
nss.dist.code[,district := as.character(district)]
nss.dist.code[nchar(district) == 1,district := paste("0", district, sep = "")]
nss.dist.code[,district.code := paste(state, district, sep = "")]
rainfall.dt[,censuscode := l2_code]

rainfall.dt <- rainfall.dt[nss.dist.code , on = c(l2_name = "districtname")]

##### aggregate the data to quarterly data
rainfall.dt[,quarter := ifelse(month %in% 1:3, "Q1",
                               ifelse(month %in% 4:6, "Q2",
                                      ifelse(month %in% 7:9, "Q3",
                                             ifelse(month %in% 10:12, "Q4", "NA"))))]

### now to compute teh deviations
###### first estimate means and standard deviations for each month (combine all the years)
rainfall.dt[,mean.rainfall.mth := mean(mean), by = .(month, district.code)]
rainfall.dt[,std.rainfall.mth := sd(mean), by = .(month, district.code)]
rainfall.dt[,std.shock := (mean - mean.rainfall.mth)/std.rainfall.mth]
rainfall.dt[,std.shock := mean(std.shock), by = .(quarter, year, district.code)] #average over the quarters
rainfall.dt[,std.shock.sq := std.shock^2]

#compute the average shocks for tables 3 and 4 of David & Pallavi's paper
ave.shock <- rainfall.dt[year %in% 2004:2018, 
                         list(rainfall = mean(std.shock), rainfallsq = mean(std.shock.sq)),
                         by = .(year, quarter)]

## take the data for the years we need
rainfall14.dt <- unique(rainfall.dt[(year == 2014 & quarter %in% c("Q3", "Q4") | 
                                       year == 2015 & quarter %in% c("Q1", "Q2")),
                                    c("quarter", "district.code", "std.shock", "std.shock.sq")])
rainfall.dt <- unique(rainfall.dt[(year == 2017 & quarter %in% c("Q3", "Q4") | 
                            year == 2018 & quarter %in% c("Q1", "Q2")),
                           c("quarter", "district.code", "std.shock", "std.shock.sq")])

## transform the dataset to wide to have rainfall and rainfallsq by quarter
colnames(rainfall.dt) <- c("quarter", "district.code", "rainfall", "rainfallsq")
rainfall.dt <- dcast(rainfall.dt, district.code ~ quarter,  
                     value.var = c('rainfall', 'rainfallsq'))

colnames(rainfall14.dt) <- c("quarter", "district.code", "rainfall", "rainfallsq")
rainfall14.dt <- dcast(rainfall14.dt, district.code ~ quarter,  
                       value.var = c('rainfall', 'rainfallsq'))

write.csv(rainfall14.dt,"../../../IND_2014_R71250H_v1_M/Data/R/rainfall.csv")

#merge rainfall data into the health survey
#R75250L2h[,district.code := as.integer(district.code)]
R75250L2h <- rainfall.dt[R75250L2h, on = "district.code"]

old.names <- colnames(R75250L2h)[grepl("rainfall_", colnames(R75250L2h))]
new.names <- str_remove_all(colnames(R75250L2h)[grepl("rainfall_", colnames(R75250L2h))], "_")
setnames(R75250L2h, old = old.names, new = new.names)

old.names <- colnames(R75250L2h)[grepl("rainfallsq_", colnames(R75250L2h))]
new.names <- str_remove_all(colnames(R75250L2h)[grepl("rainfallsq_", colnames(R75250L2h))], "_")
setnames(R75250L2h, old = old.names, new = new.names)



### include all the time interactions as well 
R75250L2h[,time := 13]
##### interact with all the columns
R75250L2h[,Dhhsizecat2ht := Dhhsizecat2h*time]
R75250L2h[,Dhhsizecat3ht := Dhhsizecat3h*time]
R75250L2h[,Dhhsizecat4ht := Dhhsizecat4h*time]
R75250L2h[,Dhhsizecat5ht := Dhhsizecat5h*time]
R75250L2h[,Dhhsizecat2t := Dhhsizecat2*time]
R75250L2h[,Dhhsizecat3t := Dhhsizecat3*time]
R75250L2h[,Dhhsizecat4t:= Dhhsizecat4*time]
R75250L2h[,Dhhsizecat5t := Dhhsizecat5*time]
R75250L2h[,Dagecat1ht := Dagecat1h*time]
R75250L2h[,Dagecat2ht := Dagecat2h*time]
R75250L2h[,Dagecat3ht := Dagecat3h*time]
R75250L2h[,Dagecat4ht := Dagecat4h*time]
R75250L2h[,Dagecat5ht := Dagecat5h*time]
R75250L2h[,Dagecat1t := Dagecat1*time]
R75250L2h[,Dagecat2t := Dagecat2*time]
R75250L2h[,Dagecat3t := Dagecat3*time]
R75250L2h[,Dagecat4t := Dagecat4*time]
R75250L2h[,Dagecat5t := Dagecat5*time]
R75250L2h[,hindut := hindu*time]
R75250L2h[,hinduht := hinduh*time]
R75250L2h[,scstht := scsth*time]
R75250L2h[,scstt := scst*time]
R75250L2h[,selfemployedht := selfemployedh*time]
R75250L2h[,casualurbanht := casualurbanh*time]
R75250L2h[,regwageurbanht := regwageurbanh*time]
R75250L2h[,selfemployedt := selfemployed*time]
R75250L2h[,casualurbant := casualurban*time]
R75250L2h[,regwageurbant := regwageurban*time]
R75250L2h[,lowskilloccht := lowskillocch*time]
R75250L2h[,middleskilloccht := middleskillocch*time]
R75250L2h[,highskilloccht := highskillocch*time]
R75250L2h[,lowskillocct := lowskillocc*time]
R75250L2h[,middleskillocct := middleskillocc*time]
R75250L2h[,highskillocct := highskillocc*time]
R75250L2h[,agrit := agri*time]
R75250L2h[,agriht := agrih*time]
R75250L2h[,indt := ind*time]
R75250L2h[,indht := indh*time]

## irelabelling all the variables
# 
# colnames(R75250L1h)<- c("varcode", "centre_round", "fsu_serial_no", "round", "schedule", "sample", "sector",
#                         "nss_region", "district", "stratum", "sub_stratum", "sub_round", "sub_sample",
#                         "fod_sub_region", "hamlet_group_sub_block_no", "second_stage_stratum_no",
#                         "sample_hhld_no", "level", "filler", "sl_no_informant", "response_code",
#                         "survey_code", "sub_code_casualty_code", "employee_code1", "employee_code2",
#                         "employee_code3", "date_of_survey", "date_of_dispatch", "time_to_canvass",
#                         "no_of_investigators", "remarks_blk_12_13", "remarks_blk_12_13_2",
#                         "remarks_elsewhere1", "remarks_elsewhere_in_sch", "blank", "nss", "nsc", "MULT", 
#                         "subround_DUP", "sector_DUP", "hhid", "district_code", "state")
# 



### adding more variables which maybe included into the model
###### here are some variables to test out based on the email sent to me by Xiaomeng (March 9th 2020):
### demographics: female ratio, dependency ratio, education level,
### housing condition: wall material, water source, toilet facility, electricity
### livestock or agriculture: cattle, chicken, donkey, agricultural land, bicycle, 
### asset ownership: radio, mobile phone, television

######for dependency ratio
R75250L3h[,age.dep := ifelse((age <= 15 | age >= 64), 1, 0)]
R75250L3h[,depratioh := mean(age.dep), by = hhid]
########place this back into the HH level file for computations
depratio.dt <- unique(R75250L3h[,c("hhid", "depratioh")])
R75250L2h <- depratio.dt[R75250L2h, on = "hhid"]

depratioh.mean <- R75250L2h[,wtd.mean(depratioh, weights = pop_wgt), by = sector]
######## create the district level means variables
R75250L2h[,depratio := wtd.mean(depratioh, weights = pop_wgt), by = .(district.code)]
depratio.mean <- R75250L2h[,wtd.mean(depratio, weights = pop_wgt), by = sector]

###### create energy source use for cooking variable
# (01) firewood and chips; (07) dung cake; (02) LPG; (08) other biogas (03) other natural gas
# (10) charcoal; (04) dung cake; (11) electricity (incl. generated by solar or wind power generators); 
# (05) kerosene (19) others (06) coke ,coal; (12) no cooking arrangement

R75250L2h[,energy_cook := as.integer(var16)]
R75250L2h[,energy_cook := cut(energy_cook, breaks = c(0:8, 10:12, 19),
                              labels = c("firewood_chips", "lpg", "othernaturalgas", "dungcake",
                                         "kerosene", "cokecoal", "gobargas", "otherbiogas",
                                         "charcoal", "electricity", "nocooking", "others"))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(energy_cook)])
old.names <- c("firewood_chips", "dungcake", "nocooking", "cokecoal", "kerosene", "othernaturalgas", "electricity", 
               "charcoal", "gobargas", "otherbiogas", "lpg")
new.names <- c("Dfirewoodh", "Ddungcakeh", "Dnocookingh", "Dcokecoalh", "Dkeroseneh", "Dothernaturalgash",
               "Delectricityh", "Dcharcoalh", "Dgobargash", "Dotherbiogash", "Dlpgh")
setnames(R75250L2h, old = old.names, new = new.names)

#compute rates we can compare across surveys
#### for the household level
energy.cook.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(energy_cook, weights = pop_wgt))), 
                               by = sector]
#### at the district level
dist.energy.cook.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(energy_cook, 
                                                                   weights = pop_wgt))),
                               by=.(sector, district.code)]
colnames(dist.energy.cook.rates) <- c("Sector", "District", "CookingEnergySource", "rates")
#l2.wgts[,district.code := as.integer(district.code)]
dist.energy.cook.rates <- dist.energy.cook.rates[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.energy.cook.rates<- dist.energy.cook.rates[,wtd.mean(rates, weights = V1), by = .(CookingEnergySource, Sector)]
colnames(dist.energy.cook.rates) <- c("CookingEnergySource", "Sector", "rates")

#### compute the district level variables
R75250L2h[,c("Dfirewood", "Ddungcake", "Dnocooking", "Dcokecoal", "Dkerosene", "Dothernaturalgas",
             "Delectricity", "Dcharcoal", "Dgobargas", "Dotherbiogas", "Dlpg") := 
            list(wtd.mean(Dfirewoodh, weights = pop_wgt), wtd.mean(Ddungcakeh, weights = pop_wgt),
                 wtd.mean(Dnocookingh, weights = pop_wgt), wtd.mean(Dcokecoalh, weights = pop_wgt),
                 wtd.mean(Dkeroseneh, weights = pop_wgt), wtd.mean(Dothernaturalgash, weights = pop_wgt),
                 wtd.mean(Delectricityh, weights = pop_wgt), wtd.mean(Dcharcoalh, weights = pop_wgt),
                 wtd.mean(Dgobargash, weights = pop_wgt), wtd.mean(Dotherbiogash, weights = pop_wgt),
                 wtd.mean(Dlpgh, weights = pop_wgt)),
          by = district.code]

#### include the gender of the household head
R75250L3h[,gender := as.integer(var6)] ##male - 1, female - 2, transgender - 3
R75250L3h[,gender := ifelse(gender == 1, "Male",
                            ifelse(gender == 2, "Female",
                                   ifelse(gender == 3, "Transgender", "NA")))]
######## marital status variable
R75250L3h[,marital := as.integer(var8)] ##never married - 1, currently married - 2, widowed - 3, divorced/separated - 4
R75250L3h[,marital := ifelse(marital == 1, "Never Married", 
                             ifelse(marital == 2, "Currently Married",
                                    ifelse(marital == 3, "Widowed",
                                           ifelse(marital == 4, "Divorced/separated", "NA"))))]
## education variable
# not literate 01; literate without any schooling 02; without formal schooling:through NFEC 03; 
# through TLC/ AEC 04; others 05; with formal schooling: below primary 06; primary 07; upper primary/middle 08;
# secondary 10; higher secondary 11; diploma/certificate course (up to secondary) 12; 
# diploma/certificate course (higher secondary) 13; diploma/certificate course (graduation & above) 14;
# graduate 15; postgraduate & above 16
R75250L3h[,educ := as.integer(var9)]
R75250L3h[,educat7 := ifelse(educ == 1, "No education",
                             ifelse(educ %in% c(7,8), "Primary (complete or incomplete)",
                                    ifelse(educ %in% 10:14, "Secondary(complete or incomplete)",
                                           ifelse(educ %in% c(15,16), "Tertiary (complete or incomplete)", "NA"))))]

R75250L3h[,educ := cut(educ, breaks = c(0:8, 10:16), labels = c("not literate", "literate without any schooling",
                                                                "without formal schooling:through NFEC",
                                                                "through TLC/ AEC", "others", 
                                                                "with formal schooling below primary", 
                                                                "primary", "upper primary/middle",
                                                                "secondary", "higher secondary", 
                                                                "diploma/certificate course (up to secondary)",
                                                                "diploma/certificate course (higher secondary)",
                                                                "diploma/certificate course (graduation & above)",
                                                                "graduate", "postgraduate & above"))]
######creating some education levels (no education, primary, secondary, tertiary)

hhhead.info <- R75250L3h[var5 == "1", c("gender", "marital", "educ", "age", "hhid", "educat7")]

##add hh prefix to names
old.names <- colnames(hhhead.info)[!(colnames(hhhead.info) == "hhid")]
new.names <- paste("hh", old.names, sep = "")
setnames(hhhead.info, old.names, new.names)

####create variables for education 
######### for the household
R75250L2h <- R75250L2h[hhhead.info, on = "hhid"]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(hheducat7)])
old.names <- c("No education", "Primary (complete or incomplete)", "Secondary(complete or incomplete)", 
               "Tertiary (complete or incomplete)")
new.names <- c("Dnoeduch", "Dprimaryh", "Dsecondaryh", "Dtertiaryh")
setnames(R75250L2h, old = old.names, new = new.names)

######### for the district
R75250L2h[,c("Dnoeduc", "Dprimary", "Dsecondary", "Dtertiary") := 
            list(wtd.mean(Dnoeduch, weights = pop_wgt), wtd.mean(Dprimaryh, weights = pop_wgt),
                 wtd.mean(Dsecondaryh, weights = pop_wgt), wtd.mean(Dtertiaryh, weights = pop_wgt)),
          by = district.code]

###### compute educational attainment rates at household and district level
hheduc.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hheducat7, weights = pop_wgt))), by = sector]
#### at the district level
dist.hheduc.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hheducat7, weights = pop_wgt))),
                                    by=.(sector, district.code)]
colnames(dist.hheduc.rates) <- c("Sector", "District", "HHeduc", "rates")
dist.hheduc.rates <- dist.hheduc.rates[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.hheduc.rates<- dist.hheduc.rates[,wtd.mean(rates, weights = V1), by = .(HHeduc, Sector)]
colnames(dist.hheduc.rates) <- c("HHeduc", "Sector", "rates")

###### creating imputation variables for household head gender
######### for the household
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(hhgender)])
old.names <- c("Male", "Female", "Transgender")
new.names <- c("Dmaleh", "Dfemaleh", "Dtransgenderh")
setnames(R75250L2h, old = old.names, new = new.names)
########## for district level
R75250L2h[,c("Dmale", "Dfemale", "Dtransgender") := 
            list(wtd.mean(Dmaleh, weights = pop_wgt), wtd.mean(Dfemaleh, weights = pop_wgt),
                 wtd.mean(Dtransgenderh, weights = pop_wgt)),
          by = district.code]

###### compute hh gender rates at household and district level
hhgender.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hhgender, weights = pop_wgt))), by = sector]
#### at the district level
dist.hhgender.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hhgender, weights = pop_wgt))),
                               by=.(sector, district.code)]
colnames(dist.hhgender.rates) <- c("Sector", "District", "HHgender", "rates")
dist.hhgender.rates <- dist.hhgender.rates[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.hhgender.rates<- dist.hhgender.rates[,wtd.mean(rates, weights = V1), by = .(HHgender, Sector)]
colnames(dist.hhgender.rates) <- c("HHgender", "Sector", "rates")

####### creating imputation variables for the hh marital status
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(hhmarital)])
old.names <- c("Currently Married", "Divorced/separated", "Never Married", "Widowed")
new.names <- c("Dmarriedh", "Ddivorcedh", "Dnevermarriedh", "Dwidowedh")
setnames(R75250L2h, old = old.names, new = new.names)
########## for district level
R75250L2h[,c("Dmarried", "Ddivorced", "Dnevermarried", "Dwidowed") := 
            list(wtd.mean(Dmarriedh, weights = pop_wgt), wtd.mean(Ddivorcedh, weights = pop_wgt),
                 wtd.mean(Dnevermarriedh, weights = pop_wgt), wtd.mean(Dwidowedh, weights = pop_wgt)),
          by = district.code]

###### compute hh gender rates at household and district level
hhgender.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hhgender, weights = pop_wgt))), by = sector]
#### at the district level
dist.hhgender.rates <- R75250L2h[,as.data.table(prop.table(wtd.table(hhgender, weights = pop_wgt))),
                                 by=.(sector, district.code)]
colnames(dist.hhgender.rates) <- c("Sector", "District", "HHgender", "rates")
dist.hhgender.rates <- dist.hhgender.rates[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.hhgender.rates<- dist.hhgender.rates[,wtd.mean(rates, weights = V1), by = .(HHgender, Sector)]
colnames(dist.hhgender.rates) <- c("HHgender", "Sector", "rates")

## estimate dependency ratio means at the household and district


### bring in the econwide data
# econwide.dt <- as.data.table(read.csv("../../../econwide.csv"))
# 
# R75250L2h[,state_codes := as.integer(state)]
# R75250L2h <- R75250L2h[econwide.dt[year == 2017.5,], on = "state_codes"]


### use newly created tables to create computations
hhmarital.rates <- HHdist.Tab(hhdt = R75250L2h, var = "hhmarital", tab.name = "Marital",
                              pop.wgt = "pop_wgt")
hheduc.rates <- HHdist.Tab(hhdt = R75250L2h, var = "hheducat7", tab.name = "Education", pop.wgt = "pop_wgt")
hhgender.rates <- HHdist.Tab(hhdt = R75250L2h, var = "hhgender", tab.name = "Sex", pop.wgt = "pop_wgt")
depratio.rates <- HHdist.Mean()
##relabel energy_cook variable first to use before using it to create tables
old.names <- R75250L2h[,unique(energy_cook)]
new.names <- c("Liquiefied Petroleum Gas", "Firewood & Chips", "No cooking", "Others", "Dung Cake",
               "Coke, Coal", "Electricity", "Kerosene", "Other Natural Gas", "Charcoal", 
               "Other Biogas", "Gobar Gas")
for (i in seq_along(old.names)){
  R75250L2h[energy_cook == old.names[i], energy_cook := new.names[i]]
}


energy.cook.rates <- HHdist.Tab(hhdt = R75250L2h, var = "energy_cook", tab.name = "EnergyCook", 
                                pop.wgt = "pop_wgt")
energy.cook.rates[[1]] <- energy.cook.rates[[1]][EnergyCook.Rates != 0,]
energy.cook.rates[[2]] <- energy.cook.rates[[2]][DistMean.EnergyCook != 0,]
# energy.cook.rates[[1]] <- energy.cook.rates[[1]][EnergyCook %in% c("Liquiefied Petroleum Gas", 
#                                                             "Firewood & Chips", "No cooking"),] 
# energy.cook.rates[[2]] <- energy.cook.rates[[2]][EnergyCook %in% c("Liquiefied Petroleum Gas", 
#                                                             "Firewood & Chips", "No cooking"),] 

R75250L2h[,hhsizeh := hhsize]
R75250L2h[,hhsize := wtd.mean(hhsizeh, weights = pop_wgt), by = district.code]


## include log total medical expenditures from the level 6 file
####### first create hhid
R75250L6h[,hhid := substr(v1, 4, 34)]

exp <- R75250L6h[,sum(as.numeric(var14)),by=hhid]

### create rainfall dummies

# R75250L2h[,DrainfallQ1 := cut(rainfallQ1, breaks = c(-100, -0.615, -0.295, -0.038, 100))]
# R75250L2h[,DrainfallQ1k1 := ifelse(DrainfallQ1 == "(-100,-0.615]", rainfallQ1, 0)]
# R75250L2h[,DrainfallQ1k2 := ifelse(DrainfallQ1 == "(-0.615,-0.295]", rainfallQ1, 0)]
# R75250L2h[,DrainfallQ1k3 := ifelse(DrainfallQ1 == "(-0.295,-0.038]", rainfallQ1, 0)]
# R75250L2h[,DrainfallQ1k4 := ifelse(DrainfallQ1 == "(-0.038,100]", rainfallQ1, 0)]
# 
# R75250L2h[,DrainfallQ2 := cut(rainfallQ2, breaks = c(-100, -0.377, -0.140, -0.220, 100))]
# R75250L2h[,DrainfallQ2k1 := ifelse(DrainfallQ2 == "(-100,-0.377]", rainfallQ2, 0)]
# R75250L2h[,DrainfallQ2k2 := ifelse(DrainfallQ2 == "(-0.377,-0.22]", rainfallQ2, 0)]
# R75250L2h[,DrainfallQ2k3 := ifelse(DrainfallQ2 == "(-0.22,-0.14]", rainfallQ2, 0)]
# R75250L2h[,DrainfallQ2k4 := ifelse(DrainfallQ2 == "(-0.14,100]", rainfallQ2, 0)]
# 
# R75250L2h[,DrainfallQ3 := cut(rainfallQ3, breaks = c(-100, 0.15, 0.53, 0.79, 100))]
# R75250L2h[,DrainfallQ3k1 := ifelse(DrainfallQ3 == "(-100,0.15]", rainfallQ3, 0)]
# R75250L2h[,DrainfallQ3k2 := ifelse(DrainfallQ3 == "(0.15,0.53]", rainfallQ3, 0)]
# R75250L2h[,DrainfallQ3k3 := ifelse(DrainfallQ3 == "(0.53,0.79]", rainfallQ3, 0)]
# R75250L2h[,DrainfallQ3k4 := ifelse(DrainfallQ3 == "(0.79,100]", rainfallQ3, 0)]
# 
# R75250L2h[,DrainfallQ4 := cut(rainfallQ4, breaks = c(-100, -0.844, -0.703, -0.4, 100))]
# R75250L2h[,DrainfallQ4k1 := ifelse(DrainfallQ4 == "(-100,-0.844]", rainfallQ4, 0)]
# R75250L2h[,DrainfallQ4k2 := ifelse(DrainfallQ4 == "(-0.844,-0.703]", rainfallQ4, 0)]
# R75250L2h[,DrainfallQ4k3 := ifelse(DrainfallQ4 == "(-0.703,-0.4]", rainfallQ4, 0)]
# R75250L2h[,DrainfallQ4k4 := ifelse(DrainfallQ4 == "(-0.4,100]", rainfallQ4, 0)]

### create rainfall spline variables
R75250L2h[,DrainfallQ1k1 := ifelse(rainfallQ1 < -0.615, 0, rainfallQ1 - (-0.615))]
R75250L2h[,DrainfallQ1k2 := ifelse(rainfallQ1 < -0.295, 0, rainfallQ1 - (-0.295))]
R75250L2h[,DrainfallQ1k3 := ifelse(rainfallQ1 < -0.038, 0, rainfallQ1 - (-0.038))]
R75250L2h[,DrainfallQ2k1 := ifelse(rainfallQ2 < -0.377, 0, rainfallQ2 - (-0.377))]
R75250L2h[,DrainfallQ2k2 := ifelse(rainfallQ2 < -0.140, 0, rainfallQ2 - (-0.140))]
R75250L2h[,DrainfallQ2k3 := ifelse(rainfallQ2 < -0.220, 0, rainfallQ2 - (-0.220))]
R75250L2h[,DrainfallQ3k1 := ifelse(rainfallQ3 < 0.15, 0, rainfallQ3 - (0.15))]
R75250L2h[,DrainfallQ3k2 := ifelse(rainfallQ3 < 0.53, 0, rainfallQ3 - (0.53))]
R75250L2h[,DrainfallQ3k3 := ifelse(rainfallQ3 < 0.79, 0, rainfallQ3 - (0.79))]
R75250L2h[,DrainfallQ4k1 := ifelse(rainfallQ4 < -0.844, 0, rainfallQ4 - (-0.844))]
R75250L2h[,DrainfallQ4k2 := ifelse(rainfallQ4 < -0.703, 0, rainfallQ4 - (-0.703))]
R75250L2h[,DrainfallQ4k3 := ifelse(rainfallQ4 < -0.400, 0, rainfallQ4 - (-0.400))]

### test out the square terms as well
R75250L2h[,DrainfallQ1k1sq := DrainfallQ1k1^2]
R75250L2h[,DrainfallQ1k2sq := DrainfallQ1k2^2]
R75250L2h[,DrainfallQ1k3sq := DrainfallQ1k3^2]
R75250L2h[,DrainfallQ2k1sq := DrainfallQ2k1^2]
R75250L2h[,DrainfallQ2k2sq := DrainfallQ2k2^2]
R75250L2h[,DrainfallQ2k3sq := DrainfallQ2k3^2]
R75250L2h[,DrainfallQ3k1sq := DrainfallQ3k1^2]
R75250L2h[,DrainfallQ3k2sq := DrainfallQ3k2^2]
R75250L2h[,DrainfallQ3k3sq := DrainfallQ3k3^2]
R75250L2h[,DrainfallQ4k1sq := DrainfallQ4k1^2]
R75250L2h[,DrainfallQ4k2sq := DrainfallQ4k2^2]
R75250L2h[,DrainfallQ4k3sq := DrainfallQ4k3^2]


#### read in the results of the 2018 simulations for Ani and David
### first we need to include household ids that are just values from 1 to N
ani.dt <- read.dta13("C:/Users/wb559885/OneDrive - WBG/Documents/WorldBankProjects/INDIA/PovertyImputation2018/IndiaPoverty2018/sae18.dta")
ani.dt <- ani.dt[,c("id_sae", "hhid")]

R75250L3h <- R75250L3h[ani.dt, on = "hhid"]

sim.dt <- as.data.table(read.dta13("mi_data_18_using1112_lasso_ruralnn_rainsq_forani.dta"))

welfcols <- colnames(sim.dt)[grepl("_welfare", colnames(sim.dt))]
sim.dt <- sim.dt[,c(welfcols, "id_sae"),with=F]

ani.dt <- R75250L3h[,c("id_sae", "var4", "age", "hhwt")][sim.dt, on = "id_sae"]
new.names <- c("hhid", "pid")
old.names <- c("id_sae", "var4")

ani.dt <- setnames(ani.dt, old = old.names, new = new.names)

ani.dt$welfare_average <- ani.dt[,welfcols,with=F][,apply(.SD, 1, mean)]


## save the workspace
save.image("../R/health18.RData")



######################################################################################################################

## writing the results to file
wb <- createWorkbook()

Setup.Worksheet <- function(tab.title = "Health18_Urban"){
  
  ## Add worksheets
  addWorksheet(wb, tab.title)
  
  x <- c("2018 Health")
  writeData(wb, tab.title, x, startCol = 2, startRow = 1)
  
  x <- c("Log HH per capita expenditure", "2011 USD Mean", "Log HH per capita expenditure", 
         "(Real Rupees) Mean")
  writeData(wb, tab.title, x, startCol = 1, startRow = 2)
  
  x <- c("HH Size", "1 or 2", 3, 4, 5, "6 and more")
  writeData(wb, tab.title, x, startCol = 1, startRow = 7)
  
  x <- c("HH size (District Avg)", "Share 1 or 2", "Share 3", "Share 4", "Share 5", "Share 6 and more")
  writeData(wb, tab.title, x, startCol = 1, startRow = 14)
  
  x <- c("HH age structure", "Share 0-14", "Share 15-24", 
         "Share 25-34", "Share 35-49", "Share 50-64", "Share 65 and over")
  writeData(wb, tab.title, x, startCol = 1, startRow = 21)
  
  x <- c("HH age structure (District Avg)", "Share 0-14", "Share 15-24", 
         "Share 25-34", "Share 35-49", "Share 50-64", "Share 65 and over")
  writeData(wb, tab.title, x, startCol = 1, startRow = 29)
  
  x <- c("Religion", "Hindu", "Other")
  writeData(wb, tab.title, x, startCol = 1, startRow = 37)
  
  x <- c("Religion (District Avg)", "Hindu", "Other")
  writeData(wb, tab.title, x, startCol = 1, startRow = 41)
  
  x <- c("Social Group", "Scheduled Caste", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 45)
  
  x <- c("Social Group (District Avg)", "Scheduled Caste", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 49)
  
  x <- c("Household Type", "Regular Wage", "Self-Employed", "Casual Labor", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 53)
  
  x <- c("Household Type (District Avg)", "Regular Wage", "Self-Employed", "Casual Labor", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 59)
  
  x <- c("Principal Industry", "Agriculture", "Industry", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 65)
  
  x <- c("Principal Industry (District Avg)", "Agriculture", "Industry", "Others")
  writeData(wb, tab.title, x, startCol = 1, startRow = 70)
  
  # x <- c("Educational Attainment HH", hheduc.rates[[1]][,unique(Education)])
  # writeData(wb, tab.title, x, startCol = 1, startRow = 52)
  # 
  # x <- c("Educational Attainment HH (District Avg)", hheduc.rates[[2]][,unique(Education)])
  # writeData(wb, tab.title, x, startCol = 1, startRow = 58)
  
  x <- c("Sex HH", hhgender.rates[[1]][Sex != "Transgender",unique(Sex)])
  writeData(wb, tab.title, x, startCol = 1, startRow = 75)
  
  x <- c("Sex HH (District Avg)", hhgender.rates[[2]][Sex != "Transgender",unique(Sex)])
  writeData(wb, tab.title, x, startCol = 1, startRow = 79)
  
  x <- c("Marital Status HH", "Married", "Not Married")
  writeData(wb, tab.title, x, startCol = 1, startRow = 83)
  
  x <- c("Marital Status HH (District Avg)", "Married", "Not Married")
  writeData(wb, tab.title, x, startCol = 1, startRow = 87)
  
  x <- c("Dependency Ratio", "Sample Average", "District Average")
  writeData(wb, tab.title, x, startCol = 1, startRow = 91)
  
   x <- c("Primary Energy Source for Cooking", energy.cook.rates[[1]][,unique(EnergyCook)])
  writeData(wb, tab.title, x, startCol = 1, startRow = 95)

  x <- c("Primary Energy Source for Cooking (District Avg)", 
         energy.cook.rates[[2]][,unique(EnergyCook)])
  writeData(wb, tab.title, x, startCol = 1, startRow = 109)
  
  x <- c("District Rainfall Shock", "July - September", "July - September (squared)", "October - December", 
         "October - December (squared)", "January - March", "January - March (squared)",
         "April - June", "April - June (squared)")
  writeData(wb, tab.title, x, startCol = 1, startRow = 123)
  
}

#placing the right numbers in their place
Setup.Worksheet(tab.title = "Health18_Urban")
Setup.Worksheet(tab.title = "Health18_Rural")


#log expenditure
writeData(wb, "Health18_Rural", umpce[[1,2]], startCol = 2, startRow = 5)

#hhsize
x <- hhsize.str[,hhsize := as.integer(hhsize)]
writeData(wb, "Health18_Rural", x[sector == "RURAL" & hhsize <= 2,
                                  sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Health18_Rural", x[sector == "RURAL" & hhsize > 2 & hhsize <= 5, 
                                  rates], startCol = 2, startRow = 9)
writeData(wb, "Health18_Rural", x[sector == "RURAL" & hhsize >= 6, 
                                  sum(rates)], startCol = 2, startRow = 12)

##hhsize dist
x <- R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat2, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 15)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat3, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 16)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat4, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 17)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat5, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 18)
x <- 1 - R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat2, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat3, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat4, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dhhsizecat5, weights = pop_wgt)]  
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 19)

#hh age structure
x <- age.str
writeData(wb, "Health18_Rural", x[sector == "RURAL",rates], startCol = 2, startRow = 22)

##dist hh age structure
x <- R75250L2h[sector == "RURAL",wtd.mean(Dagecat1, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 30)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dagecat2, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 31)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dagecat3, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 32)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dagecat4, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 33)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 34)
x <- 1 - R75250L2h[sector == "RURAL",wtd.mean(Dagecat1, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dagecat2, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dagecat3, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL",wtd.mean(Dagecat4, weights = pop_wgt)] - 
  R75250L2h[sector == "RURAL",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 35)

## religion 
writeData(wb, "Health18_Rural", rel.str[sector == "RURAL" & religion == "Hinduism",rates], 
          startCol = 2, startRow = 38)
writeData(wb, "Health18_Rural", rel.str[sector == "RURAL" & religion != "Hinduism",sum(rates)], 
          startCol = 2, startRow = 39)

## religion dist
writeData(wb, "Health18_Rural", R75250L2h[sector == "RURAL",wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 42)
writeData(wb, "Health18_Rural", R75250L2h[sector == "RURAL",1 - wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 43)

## social group 
writeData(wb, "Health18_Rural", soc.grp.str[sector == "RURAL" & social.group %in% c("backward class",
                                                                                         "scheduled caste",
                                                                                         "scheduled tribe"), 
                                                 sum(rates)], 
          startCol = 2, startRow = 46)
writeData(wb, "Health18_Rural", soc.grp.str[sector == "RURAL" & social.group == "others", rates], 
          startCol = 2, startRow = 47)

## social grp dist
writeData(wb, "Health18_Rural", R75250L2h[sector == "RURAL",wtd.mean(scst, weights = pop_wgt)],
          startCol = 2, startRow = 50)
writeData(wb, "Health18_Rural", R75250L2h[sector == "RURAL",wtd.mean(1 - scst, weights = pop_wgt)],
          startCol = 2, startRow = 51)

## household type
x <- hhtype.str[sector == "RURAL" & grepl("Regular Wage/Salary Earning", hhtype),sum(rates)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 54)
x <- hhtype.str[sector == "RURAL" & grepl("Self-Employed", hhtype),sum(rates)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 55)
x <- hhtype.str[sector == "RURAL" & grepl("Casual Labour", hhtype), sum(rates)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 56)
x <- hhtype.str[sector == "RURAL" & grepl("Others", hhtype), sum(rates)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 57)

## household type dist
x <- R75250L2h[sector == "RURAL",wtd.mean(regwageurban, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 60)
x <- R75250L2h[sector == "RURAL",wtd.mean(selfemployed, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 61)
x <- R75250L2h[sector == "RURAL",wtd.mean(casualurban, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 62)
x <- R75250L2h[sector == "RURAL",wtd.mean(1 - (regwageurban + selfemployed + casualurban), 
                                          weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 63)

## principal industry
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 66)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 67)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Others",rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 68)

## principal industry dist
x <- R75250L2h[sector == "RURAL", wtd.mean(agri, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 71)
x <- R75250L2h[sector == "RURAL", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 72)
x <- 1 - R75250L2h[sector == "RURAL", wtd.mean(agri, weights = pop_wgt)] -
  R75250L2h[sector == "RURAL", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 73)


## educational attainment for the household head
# x <- hheduc.rates[[1]][sector == "URBAN", Education.Rates]
# writeData(wb, "Health18_Urban", x, startCol = 3, startRow = 52)
# x <- hheduc.rates[[2]][sector == "URBAN", DistMean.Education]
# writeData(wb, "Health18_Urban", x, startCol = 3, startRow = 58)
# 
## household gender for the household head
x <- hhgender.rates[[1]][sector == "RURAL" & Sex != "Transgender", Sex.Rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 76)
## household gender for the household head district
x <- R75250L2h[sector == "RURAL",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 80)
x <- 1 - R75250L2h[sector == "RURAL",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 81)

## household head marital status
x <- hhmarital.rates[[1]][sector == "RURAL" & Marital == "Currently Married",Marital.Rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 84)
x <- 1 - hhmarital.rates[[1]][sector == "RURAL" & Marital == "Currently Married",sum(Marital.Rates)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 85)

## household head marital status district
x <- R75250L2h[sector == "RURAL",wtd.mean(Dmarried, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 88)
x <- 1 - R75250L2h[sector == "RURAL",wtd.mean(Dmarried, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 89)

## dependency ratio of the household head
x <- depratio.rates[[1]][sector == "RURAL", Mean.DepRatio]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 92)
x <- R75250L2h[sector == "RURAL",wtd.mean(depratio, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 93)

## primary source of energy for cooking within the household
x <- energy.cook.rates[[1]][sector == "RURAL", EnergyCook.Rates]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 96)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dlpg, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 110)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dfirewood, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 111)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dnocooking, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 112)
x <- R75250L2h[sector == "RURAL",wtd.mean(1 - Dlpg - Dfirewood - Dnocooking - 
                                          Ddungcake - Dcokecoal - Delectricity -
                                          Dkerosene - Dothernaturalgas - Dcharcoal -
                                          Dotherbiogas - Dgobargas, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 113)
x <- R75250L2h[sector == "RURAL",wtd.mean(Ddungcake, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 114)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dcokecoal, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 115)
x <- R75250L2h[sector == "RURAL",wtd.mean(Delectricity, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 116)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dkerosene, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 117)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dothernaturalgas, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 118)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dcharcoal, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 119)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dotherbiogas, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 120)
x <- R75250L2h[sector == "RURAL",wtd.mean(Dgobargas, weights = pop_wgt)]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 121)

## district rainfall shocks
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 124)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 125)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 126)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 127)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 128)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 129)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 130)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Rural", x, startCol = 2, startRow = 131)



##write results into the Rural sheet

#log expenditure
writeData(wb, "Health18_Urban", umpce[[2,2]], startCol = 2, startRow = 5)

#hhsize
x <- hhsize.str[,hhsize := as.integer(hhsize)]
writeData(wb, "Health18_Urban", x[sector == "URBAN" & hhsize <= 2,
                                  sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Health18_Urban", x[sector == "URBAN" & hhsize > 2 & hhsize <= 5, 
                                  rates], startCol = 2, startRow = 9)
writeData(wb, "Health18_Urban", x[sector == "URBAN" & hhsize >= 6, 
                                  sum(rates)], startCol = 2, startRow = 12)

##hhsize dist
x <- R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat2, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 15)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat3, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 16)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat4, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 17)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat5, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 18)
x <- 1 - R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat2, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat3, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat4, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dhhsizecat5, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 19)

#hh age structure
x <- age.str
writeData(wb, "Health18_Urban", x[sector == "URBAN",rates], startCol = 2, startRow = 22)

##dist hh age structure
x <- R75250L2h[sector == "URBAN",wtd.mean(Dagecat1, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 30)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dagecat2, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 31)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dagecat3, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 32)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dagecat4, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 33)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 34)
x <- 1 - R75250L2h[sector == "URBAN",wtd.mean(Dagecat1, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dagecat2, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dagecat3, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dagecat4, weights = pop_wgt)] -
  R75250L2h[sector == "URBAN",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 35)

## religion 
writeData(wb, "Health18_Urban", rel.str[sector == "URBAN" & religion == "Hinduism",rates], 
          startCol = 2, startRow = 38)
writeData(wb, "Health18_Urban", rel.str[sector == "URBAN" & religion != "Hinduism",sum(rates)], 
          startCol = 2, startRow = 39)

## religion dist
writeData(wb, "Health18_Urban", R75250L2h[sector == "URBAN",wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 42)
writeData(wb, "Health18_Urban", R75250L2h[sector == "URBAN",1 - wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 43)

## social group 
writeData(wb, "Health18_Urban", soc.grp.str[sector == "URBAN" & social.group %in% c("backward class",
                                                                                         "scheduled caste",
                                                                                         "scheduled tribe"), 
                                                 sum(rates)], 
          startCol = 2, startRow = 46)
writeData(wb, "Health18_Urban", soc.grp.str[sector == "URBAN" & social.group == "others", rates], 
          startCol = 2, startRow = 47)

## social grp dist
writeData(wb, "Health18_Urban", R75250L2h[sector == "URBAN",wtd.mean(scst, weights = pop_wgt)],
          startCol = 2, startRow = 50)
writeData(wb, "Health18_Urban", R75250L2h[sector == "URBAN",wtd.mean(1 - scst, weights = pop_wgt)],
          startCol = 2, startRow = 51)

## household type
x <- hhtype.str[sector == "URBAN" & grepl("Regular Wage/Salary Earning", hhtype),sum(rates)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 54)
x <- hhtype.str[sector == "URBAN" & grepl("Self-Employed", hhtype),sum(rates)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 55)
x <- hhtype.str[sector == "URBAN" & grepl("Casual Labour", hhtype), sum(rates)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 56)
x <- hhtype.str[sector == "URBAN" & grepl("Others", hhtype), sum(rates)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 57)

## household type dist
x <- R75250L2h[sector == "URBAN",wtd.mean(regwageurban, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 60)
x <- R75250L2h[sector == "URBAN",wtd.mean(selfemployed, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 61)
x <- R75250L2h[sector == "URBAN",wtd.mean(casualurban, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 62)
x <- R75250L2h[sector == "URBAN",wtd.mean(1 - (regwageurban + selfemployed + casualurban), 
                                          weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 63)

## principal industry
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 66)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 67)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Others",rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 68)

## principal industry dist
x <- R75250L2h[sector == "URBAN", wtd.mean(agri, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 71)
x <- R75250L2h[sector == "URBAN", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 72)
x <- 1 - R75250L2h[sector == "URBAN", wtd.mean(agri, weights = pop_wgt)] - 
  R75250L2h[sector == "URBAN", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 73)


## educational attainment for the household head
# x <- hheduc.rates[[1]][sector == "URBAN", Education.Rates]
# writeData(wb, "Health18_Urban", x, startCol = 3, startRow = 52)
# x <- hheduc.rates[[2]][sector == "URBAN", DistMean.Education]
# writeData(wb, "Health18_Urban", x, startCol = 3, startRow = 58)
# 
## household gender for the household head
x <- hhgender.rates[[1]][sector == "URBAN" & Sex != "Transgender", Sex.Rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 76)
## household gender for the household head district
x <- R75250L2h[sector == "URBAN",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 80)
x <- 1 - R75250L2h[sector == "URBAN",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 81)

## household head marital status
x <- hhmarital.rates[[1]][sector == "URBAN" & Marital == "Currently Married",Marital.Rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 84)
x <- hhmarital.rates[[1]][sector == "URBAN" & Marital != "Currently Married",sum(Marital.Rates)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 85)

## household head marital status district
x <- R75250L2h[sector == "URBAN",wtd.mean(Dmarried, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 88)
x <- R75250L2h[sector == "URBAN",wtd.mean(1 - Dmarried, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 89)

## dependency ratio of the household head
x <- depratio.rates[[1]][sector == "URBAN", Mean.DepRatio]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 92)
x <- R75250L2h[sector == "URBAN",wtd.mean(depratio, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 93)

## primary source of energy for cooking within the household
x <- energy.cook.rates[[1]][sector == "URBAN", EnergyCook.Rates]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 96)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dlpg, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 110)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dfirewood, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 111)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dnocooking, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 112)
x <- R75250L2h[sector == "URBAN",wtd.mean(1 - Dlpg - Dfirewood - Dnocooking - 
                                            Ddungcake - Dcokecoal - Delectricity -
                                            Dkerosene - Dothernaturalgas - Dcharcoal -
                                            Dotherbiogas - Dgobargas, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 113)
x <- R75250L2h[sector == "URBAN",wtd.mean(Ddungcake, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 114)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dcokecoal, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 115)
x <- R75250L2h[sector == "URBAN",wtd.mean(Delectricity, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 116)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dkerosene, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 117)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dothernaturalgas, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 118)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dcharcoal, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 119)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dotherbiogas, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 120)
x <- R75250L2h[sector == "URBAN",wtd.mean(Dgobargas, weights = pop_wgt)]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 121)

## district rainfall shocks
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 124)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 125)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 126)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 127)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 128)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 129)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 130)
x <- R75250L2h[,as.data.table(wtd.mean(rainfallsqQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health18_Urban", x, startCol = 2, startRow = 131)


#saveWorkbook(wb, "../R/DescHealth18.xlsx", overwrite = TRUE)
saveWorkbook(wb, "deschealth18.xlsx", overwrite = TRUE)



























