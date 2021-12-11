## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 80, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  mc.cores = 6, ## set number of (mac) cores used in parallel processing
  start.time= Sys.time()
)

library(dplyr)
library(questionr)
library(tidyr)
library(lubridate)

#### always start by setting the working directory to the base project folder
p<-"C:/Users/wb559885/OneDrive - WBG/Documents/WorldBankProjects/INDIA/PovertyImputation2018/IndiaPoverty2018"
setwd(p)

## load functions from RFunctions.R file
source("IND_2018_R75250H_v1_M/Code/RFunctions.R")

setwd("IND_2018_R75252E_v1_M/Data/STATA")

nsseduc18 <- Select.DTA() #load the education data

for (i in seq_along(nsseduc18)) {
  as.data.table(assign(paste("R75252L", i, sep = ""), value = nsseduc18[[i]]))
}

rm(nsseduc18) # we dont need the data anymore

#first we perform a left-join of the R75252L1 (level 1 data - blocks 1, 2, 11) and R75252L2 (level 2 data - block 3)
R75252L1[,hhid := substr(v1, 4, 34)]

R75252L2[,hhid := substr(v1, 4, 34)]

#include the weights into the level 2 data
R75252L1[,MULT := var37]
#compute the proper household level weights 
R75252L1[var35 == var36,hhwt := as.numeric(MULT)/100]
R75252L1[var35 != var36,hhwt := as.numeric(MULT)/200]

R75252L2 <- as.data.table(left_join(R75252L2, R75252L1[,c("hhid", "hhwt")], by = "hhid"))

#compute HH size tabulations for rural and urban areas
R75252L1[,sector := ifelse(var6 == 1, "RURAL", 
                           ifelse(var6 == 2, "URBAN", "NA"))]
R75252L1[,district.code := var8]

#create the correct district codes that combine state and district number
R75252L1[,state := substr(var7, 1, 2)]
R75252L1[,district.code := paste(state, district.code, sep = ".")]

R75252L2 <- left_join(R75252L2, R75252L1[,c("hhid", "sector", "district.code")], by = "hhid")
R75252L2 <- as.data.table(R75252L2)

R75252L2[,var4 := as.integer(var4)]
#R75252L2[,MULT := as.numeric(MULT)]


#compute household size breakdowns for rural and urban
hhsize.str <- R75252L2[,as.data.table(prop.table(wtd.table(var4, weights = hhwt))), by = sector]
colnames(hhsize.str) <- c("sector", "hhsize", "rates")
hhsize.str.unw <- R75252L2[,as.data.table(prop.table(table(var4))), by = sector]
colnames(hhsize.str.unw) <- c("sector", "hhsize", "rates")

#compute district HH size rates (we round down the means, fractional hh member is not up to one rational)
#weighted
dist.hhsize <- R75252L2[,as.data.table(prop.table(wtd.table(var4, weights = hhwt))), by = .(district.code, sector)]
colnames(dist.hhsize) <- c("district.code", "sector", "hhsize", "rates")

l2.wgts <- R75252L2[,sum(hhwt),by=.(district.code, sector)]
dist.hhsize <- dist.hhsize[l2.wgts, on = .(district.code, sector)]
dist.hhsize <- dist.hhsize[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, hhsize)]
colnames(dist.hhsize) <- c("sector", "hhsize", "rates")

#unweighted
dist.hhsize.unw <- R75252L2[,as.data.table(prop.table(table(var4))), by = .(district.code, sector)] 
colnames(dist.hhsize.unw) <- c("district.code", "sector", "hhsize", "rates")
dist.hhsize.unw <- dist.hhsize.unw[,as.data.table(mean(rates)), by = .(sector, hhsize)]
colnames(dist.hhsize.unw) <- c("sector", "hhsize", "rates")

#compute household age structure
R75252L4[,hhid := substr(v1, 4, 34)]

setkey(R75252L4, hhid)
setkey(R75252L1, hhid)

R75252L4.new <- R75252L4[R75252L1[,c("hhid", "sector")]]
R75252L4.new[,var7 := as.numeric(var7)]
R75252L4.new[,var23 := as.numeric(var23)] #turn the weights into a numeric vector 
R75252L4.new[,age.group := cut(var7, breaks = c(0,15,24,34,49,64,150), 
                               labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"),
                               include.lowest = TRUE)]

age.str.unw <- R75252L4.new[,as.data.table(prop.table(table(age.group))), by=sector]
colnames(age.str.unw) <- c("sector", "age.group", "rates")
age.str <- R75252L4.new[,as.data.table(prop.table(wtd.table(age.group, weights = var23))), by=sector]
colnames(age.str) <- c("sector", "age.group", "rates")

#compute hh age structure at the district level means
#weighted 
##include district identifier first
R75252L4.new <- R75252L4.new[R75252L2[,c("hhid", "district.code")], on = "hhid"]

dist.age.str <- R75252L4.new[,as.data.table(prop.table(wtd.table(age.group, weights = var23))), 
                             by = .(district.code, sector)]
colnames(dist.age.str) <- c("district.code", "sector", "age.group", "rates")

l4.wgts <- R75252L4.new[,sum(var23),by=.(district.code, sector)]
dist.age.str <- dist.age.str[l4.wgts, on = .(district.code, sector)]
dist.age.str <- dist.age.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, age.group)]
colnames(dist.age.str) <- c("sector", "age.group", "rates")

#unweighted
dist.age.str.unw <- R75252L4.new[,as.data.table(prop.table(table(age.group))), by = .(district.code, sector)]
colnames(dist.age.str.unw) <- c("district.code", "sector", "age.group", "rates")
dist.age.str.unw <- dist.age.str.unw[,as.data.table(mean(rates)), by = .(sector, age.group)]
colnames(dist.age.str.unw) <- c("sector", "age.group", "rates")


#compute religious breakdowns
R75252L2[,var8 := as.integer(var8)]
R75252L2[,religion := ifelse(var8 == 1, "Hinduism",
                             ifelse(var8 == 2, "Islam", 
                                    ifelse(var8 == 3, "Christianity",
                                           ifelse(var8 == 4, "Sikhism", 
                                                  ifelse(var8 == 5, "Jainism", 
                                                         ifelse(var8 == 6, "Buddhism", 
                                                                ifelse(var8 == 7, "Zoroastrianism",
                                                                       ifelse(var8 == 9, "Others", "NA"))))))))]

rel.str.unw <- R75252L2[,as.data.table(prop.table(table(religion))), by = sector]
colnames(rel.str.unw) <- c("sector", "religion", "rates")
rel.str <- R75252L2[,as.data.table(prop.table(wtd.table(religion, weights = hhwt))), by = sector]
colnames(rel.str) <- c("sector", "religion", "rates")

#compute religious breakdowns (district level means)
##include district identifier first
dist.rel.str <- R75252L2[,as.data.table(prop.table(wtd.table(religion, weights = hhwt))), 
                         by = .(district.code, sector)]
colnames(dist.rel.str) <- c("district.code", "sector", "religion", "rates")

dist.rel.str <- dist.rel.str[l2.wgts, on = .(district.code, sector)]
dist.rel.str <- dist.rel.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, religion)]
colnames(dist.rel.str) <- c("sector", "religion", "rates")

#unweighted
dist.rel.str.unw <- R75252L2[,as.data.table(prop.table(table(religion))), by = .(district.code, sector)]
colnames(dist.rel.str.unw) <- c("district.code", "sector", "religion", "rates")
dist.rel.str.unw <- dist.rel.str.unw[,as.data.table(mean(rates)), by = .(sector, religion)]
colnames(dist.rel.str.unw) <- c("sector", "religion", "rates")


#compute social group breakdowns
R75252L2[,social.group := ifelse(var9 == 1, "scheduled tribe", 
                                 ifelse(var9 == 2, "scheduled caste",
                                        ifelse(var9 == 3, "backward class",
                                               ifelse(var9 == 9, "others", "NA"))))]

soc.grp.str.unw <- R75252L2[,as.data.table(prop.table(table(social.group))),by=sector]
colnames(soc.grp.str.unw) <- c("sector", "social.group", "rates")
soc.grp.str <- R75252L2[,as.data.table(prop.table(wtd.table(social.group, weights = hhwt))),by=sector]
colnames(soc.grp.str) <- c("sector", "social.group", "rates")

#compute social group breakdowns (district level means)
##include district identifier first
dist.soc.grp.str <- R75252L2[,as.data.table(prop.table(wtd.table(social.group, weights = hhwt))), 
                              by = .(district.code, sector)]
colnames(dist.soc.grp.str) <- c("district.code", "sector", "social.group", "rates")
#weighted
dist.soc.grp.str <- dist.soc.grp.str[l2.wgts, on = .(district.code, sector)]
dist.soc.grp.str <- dist.soc.grp.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, social.group)]
colnames(dist.soc.grp.str) <- c("sector", "social.group", "rates")

#unweighted
dist.soc.grp.str.unw <- R75252L2[,as.data.table(prop.table(table(social.group))), by = .(district.code, sector)]
colnames(dist.soc.grp.str.unw) <- c("district.code", "sector", "social.group", "rates")
dist.soc.grp.str.unw <- dist.soc.grp.str.unw[,as.data.table(mean(rates)), by = .(sector, social.group)]
colnames(dist.soc.grp.str.unw) <- c("sector", "social.group", "rates")


#cross-tab of religion and social class
rrel.soc.tab2 <- R75252L2[sector == "URBAN",prop.table(table(religion, social.group, weights = hhwt),2)] #by columns
urel.soc.tab2 <- R75252L2[sector == "RURAL",prop.table(table(religion, social.group, weights = hhwt),2)] #by columns
rrel.soc.tab2.unw <- R75252L2[sector == "URBAN",prop.table(table(religion, social.group, weights = hhwt),2)] #by columns
urel.soc.tab2.unw <- R75252L2[sector == "RURAL",prop.table(table(religion, social.group, weights = hhwt),2)] #by columns

rrel.soc.tab1 <- R75252L2[sector == "URBAN",prop.table(table(religion, social.group),1)] #by rows
urel.soc.tab1 <- R75252L2[sector == "RURAL",prop.table(table(religion, social.group),1)] #by rows
rrel.soc.tab1.unw <- R75252L2[sector == "URBAN",prop.table(table(religion, social.group),1)] #by rows
urel.soc.tab1.unw <- R75252L2[sector == "RURAL",prop.table(table(religion, social.group),1)] #by rows


#tabulation of household type
R75252L2[sector == "RURAL",hhtype := ifelse(var7 == 1, "Self-Employed in Agriculture",
                           ifelse(var7 == 2, "Self-Employed in Non-Agriculture",
                                  ifelse(var7 == 3, "Regular Wage/Salary Earning in Agriculture",
                                         ifelse(var7 == 4, "Regular Wage/Salary Earning in Non-Agriculture",
                                                ifelse(var7 == 5, "Casual Labour in Agriculture",
                                                       ifelse(var7 == 6, "Casual Labour in Non-Agriculture",
                                                              ifelse(var7 == 9, "Others", "NA")))))))]
R75252L2[sector == "URBAN", hhtype := ifelse(var7 == 1, "Self-Employed", 
                                             ifelse(var7 == 2, "Regular Wage/Salary Earning",
                                                    ifelse(var7 == 3, "Casual Labour",
                                                           ifelse(var7 == 9, "Others", "NA"))))]
hhtype.str.unw <- R75252L2[,as.data.table(prop.table(table(hhtype))),by=sector]
colnames(hhtype.str.unw) <- c("sector", "hhtype", "rates")
hhtype.str <- R75252L2[,as.data.table(prop.table(wtd.table(hhtype, weights = hhwt))),by=sector]
colnames(hhtype.str) <- c("sector", "hhtype", "rates")

#district level tabulation of household type
#weighted
dist.hhtype.str <- R75252L2[,as.data.table(prop.table(wtd.table(hhtype, weights = hhwt))), 
                             by = .(district.code, sector)]
colnames(dist.hhtype.str) <- c("district.code", "sector", "hhtype", "rates")
dist.hhtype.str <- dist.hhtype.str[l2.wgts, on = .(district.code, sector)]
dist.hhtype.str <- dist.hhtype.str[,as.data.table(wtd.mean(rates, weights = V1)), by = .(sector, hhtype)]
colnames(dist.hhtype.str) <- c("sector", "hhtype", "rates")

#unweighted
dist.hhtype.str.unw <- R75252L2[,as.data.table(prop.table(table(hhtype))), by = .(district.code, sector)]
colnames(dist.hhtype.str.unw) <- c("district.code", "sector", "hhtype", "rates")
dist.hhtype.str.unw <- dist.hhtype.str.unw[,as.data.table(mean(rates)), by = .(sector, hhtype)]
colnames(dist.hhtype.str.unw) <- c("sector", "hhtype", "rates")


## principal industry
R75252L2[,var5 := as.integer(var5)]
R75252L2[,principal.industry := cut(x = var5, breaks = c(0,3229,43900,99000),
                                    labels = c("Agriculture", "Industry", "Others"))]
prin.ind.str <- R75252L2[,as.data.table(prop.table(wtd.table(principal.industry, weights = hhwt))),by=sector]
colnames(prin.ind.str) <- c("Sector", "Principal.Industry", "rates")
prin.ind.str.unw <- R75252L2[,as.data.table(prop.table(table(principal.industry))),by=sector]
colnames(prin.ind.str.unw) <- c("Sector", "Principal.Industry", "rates")


## district mean for principal industry tabulation
dist.prin.ind.str <- R75252L2[,as.data.table(prop.table(wtd.table(principal.industry, 
                                                                  weights = hhwt))),by=.(sector, district.code)]
colnames(dist.prin.ind.str) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str <- dist.prin.ind.str[l2.wgts, on = .(Sector = sector, District = district.code)]
dist.prin.ind.str <- dist.prin.ind.str[,wtd.mean(rates, weights = V1), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str) <- c("Principal.Industry", "Sector", "rates")
##unweighted
dist.prin.ind.str.unw <- R75252L2[,as.data.table(prop.table(table(principal.industry))),by=.(sector, district.code)]
colnames(dist.prin.ind.str.unw) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str.unw <- dist.prin.ind.str.unw[,mean(rates), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str.unw) <- c("Principal.Industry", "Sector", "rates")


## compute consumption expenditure
## we need to assign cpi values to each individual to obtain real values

# we compute CPI from July 2011 to June 2012

cpi.dt <- data.table(month = rep(c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec",
                                   "Jan", "Feb", "Mar", "Apr", "May", "Jun"), 2),
                     sector = c(rep("RURAL", 12), rep("URBAN", 12)),
                     cpi11 = c(rep(622.3333, 12), rep(mean(c(193, 194, 197, 198, 199, 197, 198, 
                                                             199, 201, 205, 206, 208)), 12)),
                     cpi18 = c(884, 894, 893, 901, 905, 900, 895, 889, 887, 888, 891, 894,
                               285, 285, 285, 287, 288, 286, 288, 287, 287, 288, 289, 291),
                     subround = rep(c(rep(1,3), rep(2,3), rep(3,3), rep(4,3))),2)
cpi.dt[,deflator := cpi11/cpi18]
write.csv(cpi.dt, "C:/Users/wb559885/WBG/Laura Liliana Moreno Herrera - India/IND_2018_R75250H_v1_M/Data/R/CPIData.csv")
write.csv(cpi.dt, "C:/Users/wb559885/WBG/Laura Liliana Moreno Herrera - India/IND_2018_R75252E_v1_M/Data/R/CPIData.csv")
cpi.dt <- cpi.dt[,mean(deflator),by=.(subround, sector)]


#include the cpi and deflator values in the relevant files
colnames(cpi.dt) <- c("subround", "sector", "deflator")
#bring in subround into the L2 file
setkey(R75252L1, hhid)
setkey(R75252L2, hhid)

R75252L1[,subround := var11]
R75252L2 <- R75252L2[R75252L1[,c("hhid", "subround")]] 

R75252L2[,subround := as.integer(subround)]
setkey(cpi.dt, subround, sector)
setkey(R75252L2, subround, sector)

R75252L2 <- R75252L2[cpi.dt]

#compute real values for household's usual consumer expenditure
R75252L2[,var20 := as.numeric(var20)]
R75252L2[,real.hhexp := var20*deflator]
R75252L2[,real.hhexp.pcap := real.hhexp/var4]
R75252L2[,lnreal.hhexp := log(real.hhexp)]
R75252L2[,lnreal.hhexp.pcap := log(real.hhexp.pcap)]
umpce.unw <- R75252L2[,mean(lnreal.hhexp.pcap, na.rm = TRUE), by = sector]
umpce <- R75252L2[,wtd.mean(lnreal.hhexp.pcap, weights = hhwt), by = sector]

#compute 2011 USD PPP equivalent for the consumption expenditures
R75252L2[sector == "URBAN",yearcpi := 15.69499969482422]
R75252L2[sector == "RURAL",yearcpi := 12.90799999237061]

R75252L2[,umpce.ppp := real.hhexp.pcap/yearcpi]
umpce.ppp <- R75252L2[,wtd.mean(umpce.ppp, weights = hhwt),by=sector]

R75252L2[,lnumpce.ppp := log(umpce.ppp)]
R75252L2[is.na(lnumpce.ppp) == TRUE | lnumpce.ppp == -Inf, lnumpce.ppp := log(1)]
lnumpce.ppp <- R75252L2[,wtd.mean(lnumpce.ppp, weights = hhwt),by=sector]


##save the image in the R folder within the Data section of the 2017/18 Education files
save.image(file = "../R/educ18.RData")

######################################################################################################################

## writing the results to file
wb <- createWorkbook()

Setup.Worksheet <- function(tab.title = "Health14_Urban"){
  
  ## Add worksheets
  addWorksheet(wb, tab.title)
  
  ### create the column indicators
  x <- c("2018 Educ Unwgt")
  writeData(wb, tab.title, x, startCol = 2, startRow = 1)
  
  x <- c("2018 Educ")
  writeData(wb, tab.title, x, startCol = 3, startRow = 1)
  
  x <- c("Log HH per capita expenditure", "2011 USD Mean", "Log HH per capita expenditure", "(Real Rupees) Mean")
  writeData(wb, tab.title, x, startCol = 1, startRow = 2)
  
  x <- c("HH Size", "1 or 2", 3, 4, 5)
  writeData(wb, tab.title, x, startCol = 1, startRow = 7)
  
  x <- c("District HH size", "Share 1 or 2", "Share 3", "Share 4", "Share 5")
  writeData(wb, tab.title, x, startCol = 1, startRow = 13)
  
  x <- c("HH age structure", "Share 0-14", "Share 15-24", "Share 25-34", "Share 35-49", "Share 50-64")
  writeData(wb, tab.title, x, startCol = 1, startRow = 19)
  
  x <- c("District avg HH age structure", "Share 0-14", "Share 15-24", "Share 25-34", "Share 35-49", "Share 50-64")
  writeData(wb, tab.title, x, startCol = 1, startRow = 26)
  
  x <- c("Religion and Social Group", "Hindu", "Share Hindu in District", "Shared sched caste in district")
  writeData(wb, tab.title, x, startCol = 1, startRow = 33)
  
  x <- c("Household Type", "Self-Employed", "Shared self-employed in district", "Share casual labour",
         "Share casual labour in district", "Regular wage worker", "Share reg wage worker in district")
  writeData(wb, tab.title, x, startCol = 1, startRow = 38)
  
  x <- c("Principal Industry", "Agriculture", "Industry", "Share industry in district")
  writeData(wb, tab.title, x, startCol = 1, startRow = 46)
  
}

#placing the right numbers in their place
Setup.Worksheet(tab.title = "Education18_Urban")
Setup.Worksheet(tab.title = "Education18_Rural")


#log expenditure
writeData(wb, "Education18_Urban", umpce.unw[[2,2]], startCol = 2, startRow = 5)
writeData(wb, "Education18_Urban", umpce[[2,2]], startCol = 3, startRow = 5)

#hhsize
x <- hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize > 2,rates], startCol = 2, startRow = 9)
x <- hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 8)
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize > 2,rates], startCol = 3, startRow = 9)

#dist hhsize
x <- dist.hhsize.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 14)
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize > 2,rates], startCol = 2, startRow = 15)
x <- dist.hhsize[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 14)
writeData(wb, "Education18_Urban", x[sector == "URBAN" & hhsize > 2,rates], startCol = 3, startRow = 15)

#hh age structure
x <- age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education18_Urban", x[sector == "URBAN",rates], startCol = 2, startRow = 20)
x <- age.str[!(age.group %in% "65+"),]
writeData(wb, "Education18_Urban", x[sector == "URBAN",rates], startCol = 3, startRow = 20)

##dist hh age structure
x <- dist.age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education18_Urban", x[sector == "URBAN",rates], startCol = 2, startRow = 27)
x <- dist.age.str[!(age.group %in% "65+"),]
writeData(wb, "Education18_Urban", x[sector == "URBAN",rates], startCol = 3, startRow = 27)

## religion and social group
writeData(wb, "Education18_Urban", rel.str.unw[sector == "URBAN" & religion == "Hinduism",rates], 
          startCol = 2, startRow = 34)
writeData(wb, "Education18_Urban", rel.str[sector == "URBAN" & religion == "Hinduism",rates], 
          startCol = 3, startRow = 34)
writeData(wb, "Education18_Urban", dist.rel.str.unw[sector == "URBAN" & religion == "Hinduism",rates], 
          startCol = 2, startRow = 35)
writeData(wb, "Education18_Urban", dist.rel.str[sector == "URBAN" & religion == "Hinduism",rates],
          startCol = 3, startRow = 35)
writeData(wb, "Education18_Urban", dist.soc.grp.str.unw[sector == "URBAN" & social.group %in% 
                                                          c("scheduled caste", "scheduled tribe", "backward class"), 
                                                        sum(rates)],
          startCol = 2, startRow = 36)
writeData(wb, "Education18_Urban", dist.soc.grp.str[sector == "URBAN" & social.group %in% 
                                                      c("scheduled caste", "scheduled tribe", "backward class"), 
                                                    sum(rates)], 
          startCol = 3, startRow = 36)

## household type
x <- hhtype.str.unw[sector == "URBAN" & hhtype == "Self-Employed", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 39)
x <- hhtype.str[sector == "URBAN" & hhtype == "Self-Employed", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 39)
x <- dist.hhtype.str.unw[sector == "URBAN" & hhtype == "Self-Employed", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 40)
x <- dist.hhtype.str[sector == "URBAN" & hhtype == "Self-Employed", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 40)

x <- hhtype.str.unw[sector == "URBAN" & hhtype == "Casual Labour", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 41)
x <- hhtype.str[sector == "URBAN" & hhtype == "Casual Labour", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 41)
x <- dist.hhtype.str.unw[sector == "URBAN" & hhtype == "Casual Labour", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 42)
x <- dist.hhtype.str[sector == "URBAN" & hhtype == "Casual Labour", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 42)

x <- hhtype.str.unw[sector == "URBAN" & hhtype == "Regular Wage/Salary Earning", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 43)
x <- hhtype.str[sector == "URBAN" & hhtype == "Regular Wage/Salary Earning", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 43)
x <- dist.hhtype.str.unw[sector == "URBAN" & hhtype == "Regular Wage/Salary Earning", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 44)
x <- dist.hhtype.str[sector == "URBAN" & hhtype == "Regular Wage/Salary Earning", sum(rates)]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 44)

## principal industry
x <- prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 47)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 47)
x <- prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 48)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 48)
x <- dist.prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Urban", x, startCol = 2, startRow = 49)
x <- dist.prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Urban", x, startCol = 3, startRow = 49)







##write results into the Rural sheet

#log expenditure
writeData(wb, "Education18_Rural", umpce.unw[[1,2]], startCol = 2, startRow = 5)
writeData(wb, "Education18_Rural", umpce[[1,2]], startCol = 3, startRow = 5)

#hhsize
x <- hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize > 2,rates], startCol = 2, startRow = 9)
x <- hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 8)
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize > 2,rates], startCol = 3, startRow = 9)

#dist hhsize
x <- dist.hhsize.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 14)
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize > 2,rates], startCol = 2, startRow = 15)
x <- dist.hhsize[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 14)
writeData(wb, "Education18_Rural", x[sector == "RURAL" & hhsize > 2,rates], startCol = 3, startRow = 15)

#hh age structure
x <- age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education18_Rural", x[sector == "RURAL",rates], startCol = 2, startRow = 20)
x <- age.str[!(age.group %in% "65+"),]
writeData(wb, "Education18_Rural", x[sector == "RURAL",rates], startCol = 3, startRow = 20)

##dist hh age structure
x <- dist.age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education18_Rural", x[sector == "RURAL",rates], startCol = 2, startRow = 27)
x <- dist.age.str[!(age.group %in% "65+"),]
writeData(wb, "Education18_Rural", x[sector == "RURAL",rates], startCol = 3, startRow = 27)

## religion and social group
writeData(wb, "Education18_Rural", rel.str.unw[religion == "Hinduism" & sector == "RURAL",rates],
          startCol = 2, startRow = 34)
writeData(wb, "Education18_Rural", rel.str[religion == "Hinduism" & sector == "RURAL",rates], 
          startCol = 3, startRow = 34)
writeData(wb, "Education18_Rural", dist.rel.str.unw[religion == "Hinduism" & sector == "RURAL",rates],
          startCol = 2, startRow = 35)
writeData(wb, "Education18_Rural", dist.rel.str[religion == "Hinduism" & sector == "RURAL",rates],
          startCol = 3, startRow = 35)
writeData(wb, "Education18_Rural", dist.soc.grp.str.unw[sector == "RURAL" & social.group %in% 
                                                          c("scheduled caste", "scheduled tribe", "backward class"), 
                                                        sum(rates)], 
          startCol = 2, startRow = 36)
writeData(wb, "Education18_Rural", dist.soc.grp.str[sector == "RURAL" & social.group %in% 
                                                      c("scheduled caste", "scheduled tribe", "backward class"), 
                                                    sum(rates)], 
          startCol = 3, startRow = 36)

## household type
x <- hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Self-Employed in Agriculture", 
                                                      "Self-Employed in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 39)
x <- hhtype.str[sector == "RURAL" & hhtype %in% c("Self-Employed in Agriculture", 
                                                  "Self-Employed in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 39)
x <- dist.hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Self-Employed in Agriculture", 
                                                           "Self-Employed in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 40)
x <- dist.hhtype.str[sector == "RURAL" & hhtype %in% c("Self-Employed in Agriculture", 
                                                       "Self-Employed in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 40)

x <- hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Casual Labour in Agriculture",
                                                      "Casual Labour in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 41)
x <- hhtype.str[sector == "RURAL" & hhtype %in% c("Casual Labour in Agriculture",
                                                  "Casual Labour in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 41)
x <- dist.hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Casual Labour in Agriculture",
                                                           "Casual Labour in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 42)
x <- dist.hhtype.str[sector == "RURAL" & hhtype %in% c("Casual Labour in Agriculture",
                                                       "Casual Labour in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 42)

x <- hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Regular Wage/Salary Earning in Agriculture", 
                                                      "Regular Wage/Salary Earning in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 43)
x <- hhtype.str[sector == "RURAL" & hhtype %in% c("Regular Wage/Salary Earning in Agriculture", 
                                                  "Regular Wage/Salary Earning in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 43)
x <- dist.hhtype.str.unw[sector == "RURAL" & hhtype %in% c("Regular Wage/Salary Earning in Agriculture", 
                                                           "Regular Wage/Salary Earning in Non-Agriculture"), 
                         sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 44)
x <- dist.hhtype.str[sector == "RURAL" & hhtype %in% c("Regular Wage/Salary Earning in Agriculture", 
                                                       "Regular Wage/Salary Earning in Non-Agriculture"), sum(rates)]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 44)

## principal industry
x <- prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 47)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 47)
x <- prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 48)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 48)
x <- dist.prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Rural", x, startCol = 2, startRow = 49)
x <- dist.prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education18_Rural", x, startCol = 3, startRow = 49)


saveWorkbook(wb, "../R/DescEducation18.xlsx", overwrite = TRUE)



