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


#### always start by setting the working directory to the base project folder
p<-"C:/Users/wb559885/OneDrive - WBG/Documents/WorldBankProjects/INDIA/PovertyImputation2018/IndiaPoverty2018"
setwd(p)

## load functions from RFunctions.R file
source("IND_2018_R75250H_v1_M/Code/RFunctions.R")

### read in all the education dta files from the relevant folder

setwd("IND_2014_R71252E_v1_M/Data/STATA")

nsseduc14 <- Select.DTA(find.pattern = "R71252EL[0-9][0-9].dta")

for (i in seq_along(nsseduc14)) {
  as.data.table(assign(paste("R71252EL", i,  sep = ""), value = nsseduc14[[i]]))
}

rm(nsseduc14) # we dont need the data anymore


###can we replicate the number of villages/blocks, households and persons surveyed by state/UT

check.hhid <- R71252EL1[,length(unique(HH_ID)),by=state_cd]
check.hhid[,sum(V1)] ### we get 65932 which means we match perfectly

### compute log HH per capita expenditure mean
cpi.dt <- data.table(month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "June"),2),
                     Sector = c(rep("RURAL", 6), rep("URBAN", 6)),
                     cpi11 = c(rep(622.3333, 6), rep(199.5833, 6)),
                     cpi14 = c(757, 757, 763, 771, 777, 785, 237, 238, 239, 242, 244, 246),
                     SubRound = as.integer(rep(c(rep(1, 3), rep(2, 3)),2)))

cpi.dt[,cpi14 := mean(cpi14), by = .(SubRound, Sector)]
cpi.dt[,deflator := cpi11/cpi14]

cpi.dt <- unique(cpi.dt[,c("Sector", "cpi11", "cpi14", "SubRound", "deflator")])
write.csv(cpi.dt, "../R/CPIdata.csv")

#####merge in the cpi data into L2 file
cpi.dt[,SubRound := as.factor(SubRound)]
R71252EL2[,sector := ifelse(sector == "Rural", "RURAL",
                            ifelse(sector == "Urban", "URBAN", "NA"))]
R71252EL2[,sround := ifelse(sround == "Sub-Round 1", 1,
                            ifelse(sround == "Sub-Round 2", 2, "NA"))]

R71252EL2 <- cpi.dt[R71252EL2, on = .(Sector = sector, SubRound = sround)]

##### UMPCE computations
R71252EL2[,realhh.con.exp := deflator*hh_cons_exp]
R71252EL2[,realhh.con.exp.pcap := realhh.con.exp/hhsize]
R71252EL2[,ln.realhh.cons.exp.pcap := log(realhh.con.exp.pcap)]
umpce <- R71252EL2[,wtd.mean(ln.realhh.cons.exp.pcap, weights = wgt_ss),by = Sector]
umpce.unw <- R71252EL2[,mean(ln.realhh.cons.exp.pcap, na.rm = TRUE),by = Sector]


## compute dollar cpi
R71252EL2[Sector == "URBAN",yearcpi := 15.69499969482422]
R71252EL2[Sector == "RURAL",yearcpi := 12.90799999237061]

R71252EL2[,umpce.ppp := realhh.con.exp.pcap/yearcpi]
umpce.ppp <- R71252EL2[,wtd.mean(umpce.ppp, weights = wgt_ss),by=Sector]

R71252EL2[,lnumpce.ppp := log(umpce.ppp)]
umpce.ppp <- R71252EL2[,wtd.mean(lnumpce.ppp, weights = wgt_ss),by=Sector]


## compute household size structure
hhsize.str <- R71252EL2[,as.data.table(prop.table(wtd.table(hhsize, weights = wgt_ss))),by = Sector]
hhsize.str.unw <- R71252EL2[,as.data.table(prop.table(table(hhsize))),by = Sector]
colnames(hhsize.str) <- c("Sector", "hhsize", "rates")
colnames(hhsize.str.unw) <- colnames(hhsize.str)

## compute district level means for household size structure
dist.hhsize.str <- R71252EL2[,as.data.table(prop.table(wtd.table(hhsize, weights = wgt_ss))),by = .(Sector, 
                                                                                                    district)]
colnames(dist.hhsize.str) <- c("Sector", "District", "hhsize", "rates")

##### compute district level weights for the above tabulation
l2.wgts <- R71252EL2[,sum(wgt_ss),by=.(Sector, district)]
colnames(l2.wgts) <- c("Sector", "District", "weights")
dist.hhsize.str <- dist.hhsize.str[l2.wgts, on = .(Sector, District)] ##merge back into district level data
######### weighted computation
dist.hhsize.str <- dist.hhsize.str[,wtd.mean(rates, weights = weights), by=.(Sector, hhsize)]
colnames(dist.hhsize.str) <- c("Sector", "hhsize", "rates")
######### unweighted computation
dist.hhsize.str.unw <- R71252EL2[,as.data.table(prop.table(table(hhsize))),by = .(Sector, district)]
colnames(dist.hhsize.str.unw) <- c("Sector", "District", "hhsize", "rates")
dist.hhsize.str.unw <- dist.hhsize.str.unw[,mean(rates), by=.(Sector, hhsize)]
colnames(dist.hhsize.str.unw) <- c("Sector", "hhsize", "rates")

## compute household age structure
R71252EL3[,age.group := cut(x = age, breaks = c(0,14,24,34,49,64,150), 
                            labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"),
                            include.lowest = TRUE)]
age.str <- R71252EL3[,as.data.table(prop.table(wtd.table(age.group, weights = wgt_ss))),by = sector]
colnames(age.str) <- c("sector", "agegroup", "rates")
age.str.unw <- R71252EL3[,as.data.table(prop.table(table(age.group))),by = sector]
colnames(age.str.unw) <- c("sector", "agegroup", "rates")

## compute district level random effect mean for household age structure
###### weighted
dist.age.str <- R71252EL3[,as.data.table(prop.table(wtd.table(age.group, weights = wgt_ss))),
                          by = .(sector, district)]
colnames(dist.age.str) <- c("sector", "district", "agegroup", "rates") 
l3.wgts <- R71252EL3[,sum(wgt_ss),by=.(sector, district)]
colnames(l3.wgts) <- c("sector", "district", "weights")
dist.age.str <- dist.age.str[l3.wgts, on = .(sector, district)]
dist.age.str <- dist.age.str[,as.data.table(wtd.mean(rates, weights = weights)),
                          by = .(sector, agegroup)]
colnames(dist.age.str) <- c("sector", "age.group", "rates")

###### unweighted
dist.age.str.unw <- R71252EL3[,as.data.table(prop.table(table(age.group))), by = .(sector, district)]
colnames(dist.age.str.unw) <- c("sector", "district", "age.group", "rates")
dist.age.str.unw <- dist.age.str.unw[,mean(rates),by=.(sector, age.group)]
colnames(dist.age.str.unw) <- c("sector", "age.group", "rates")

## compute religious and social group structure 
rel.str <-R71252EL2[,as.data.table(prop.table(wtd.table(religion, weights = wgt_ss))),by=Sector]
colnames(rel.str) <- c("Sector", "Religion", "rates")
rel.str.unw <-R71252EL2[,as.data.table(prop.table(table(religion))),by=Sector]
colnames(rel.str.unw) <- c("Sector", "Religion", "rates")

soc.grp.str <- R71252EL2[,as.data.table(prop.table(wtd.table(social_grp, weights = wgt_ss))),by=Sector]
colnames(soc.grp.str) <- c("Sector", "SocialGroup", "rates")
soc.grp.str.unw <- R71252EL2[,as.data.table(prop.table(table(social_grp))),by=Sector]
colnames(soc.grp.str.unw) <- c("Sector", "SocialGroup", "rates")

## compute district religious and social group structure
dist.rel.str <- R71252EL2[,as.data.table(prop.table(wtd.table(religion, 
                                                              weights = wgt_ss))), by = .(district, Sector)]
colnames(dist.rel.str) <- c("District", "Sector", "Religion", "rates")
dist.rel.str <- dist.rel.str[l2.wgts, on = .(Sector, District)]
colnames(dist.rel.str) <- c("District", "Sector", "Religion", "rates", "weights")
dist.rel.str <- dist.rel.str[,wtd.mean(rates, weights = weights), by=.(Sector, Religion)]
colnames(dist.rel.str) <- c("Sector", "Religion", "rates")

dist.rel.str.unw <- R71252EL2[,as.data.table(prop.table(table(religion))), by = .(district, Sector)]
colnames(dist.rel.str.unw) <- c("District", "Sector", "Religion", "rates")
dist.rel.str.unw <- dist.rel.str.unw[,mean(rates), by=.(Sector, Religion)]
colnames(dist.rel.str.unw) <- c("Sector", "Religion", "rates")

dist.soc.grp.str <- R71252EL2[,as.data.table(prop.table(wtd.table(social_grp, weights = wgt_ss))), 
                              by = .(district, Sector)]
colnames(dist.soc.grp.str) <- c("District", "Sector", "SocialGroup", "rates")
dist.soc.grp.str <- dist.soc.grp.str[l2.wgts, on = .(Sector, District)]
colnames(dist.soc.grp.str) <- c("District", "Sector", "SocialGroup", "rates", "weights")
dist.soc.grp.str <- dist.soc.grp.str[,wtd.mean(rates, weights = weights), by=.(Sector, SocialGroup)]
colnames(dist.soc.grp.str) <- c("Sector", "SocialGroup", "rates")

dist.soc.grp.str.unw <- R71252EL2[,as.data.table(prop.table(table(social_grp))), by = .(district, Sector)]
colnames(dist.soc.grp.str.unw) <- c("District", "Sector", "SocialGroup", "rates")
dist.soc.grp.str.unw <- dist.soc.grp.str.unw[,mean(rates), by=.(Sector, SocialGroup)]
colnames(dist.soc.grp.str.unw) <- c("Sector", "SocialGroup", "rates")

## household type structure
hhtype.str <- R71252EL2[,as.data.table(prop.table(wtd.table(hhtype, weights = wgt_ss))), by = Sector]
hhtype.str.unw <- R71252EL2[,as.data.table(prop.table(table(hhtype))), by = Sector]

## compute household type (district level)
dist.hhtype.str <- R71252EL2[,as.data.table(prop.table(wtd.table(hhtype, weights = wgt_ss))), 
                             by = .(district, Sector)]
colnames(dist.hhtype.str) <- c("District", "Sector", "hhtype", "rates")
dist.hhtype.str <- dist.hhtype.str[l2.wgts, on = .(Sector, District)]
colnames(dist.hhtype.str) <- c("District", "Sector", "hhtype", "rates", "weights")
dist.hhtype.str <- dist.hhtype.str[,wtd.mean(rates, weights = weights), by=.(Sector, hhtype)]
colnames(dist.hhtype.str) <- c("Sector", "hhtype", "rates")

dist.hhtype.str.unw <- R71252EL2[,as.data.table(prop.table(table(hhtype))), by = .(stratum, Sector)]
colnames(dist.hhtype.str.unw) <- c("District", "Sector", "hhtype", "rates")
dist.hhtype.str.unw <- dist.hhtype.str.unw[,mean(rates), by=.(Sector, hhtype)]
colnames(dist.hhtype.str.unw) <- c("Sector", "hhtype", "rates")


# compute industry level breakdowns
## first we need to reclassify the NCO 2004 codes into principal industries

R71252EL2[,nic2008 := as.integer(NIC2008)]
R71252EL2[,principal.industry := cut(x = nic2008, breaks = c(0,3229,43900,99000),
                                     labels = c("Agriculture", "Industry", "Others"))]
prin.ind.str <- R71252EL2[,as.data.table(prop.table(wtd.table(principal.industry, weights = wgt_ss))),by=Sector]
colnames(prin.ind.str) <- c("Sector", "Principal.Industry", "rates")
prin.ind.str.unw <- R71252EL2[,as.data.table(prop.table(table(principal.industry))),by=Sector]
colnames(prin.ind.str.unw) <- c("Sector", "Principal.Industry", "rates")

#compute industry level breakdowns for the districts
##weighted
dist.prin.ind.str <- R71252EL2[,as.data.table(prop.table(wtd.table(principal.industry, 
                                                                   weights = wgt_ss))),by=.(Sector, district)]
colnames(dist.prin.ind.str) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str <- dist.prin.ind.str[l2.wgts, on = .(Sector, District)]
dist.prin.ind.str <- dist.prin.ind.str[,wtd.mean(rates, weights = weights), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str) <- c("Principal.Industry", "Sector", "rates")
##unweighted
dist.prin.ind.str.unw <- R71252EL2[,as.data.table(prop.table(table(principal.industry))),by=.(Sector, district)]
colnames(dist.prin.ind.str.unw) <- c("Sector", "District", "Principal.Industry", "rates")
dist.prin.ind.str.unw <- dist.prin.ind.str.unw[,mean(rates), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str.unw) <- c("Principal.Industry", "Sector", "rates")


### save the workspace
save.image("../R/educ14.RData")


#### write results into Excel worksheet
wb <- createWorkbook()

Setup.Worksheet <- function(tab.title = "Education14_Urban"){
  
  ## Add worksheets
  addWorksheet(wb, tab.title)
  
  ### create the column indicators
  x <- c("2014 Educ Unwgt")
  writeData(wb, tab.title, x, startCol = 2, startRow = 1)
  
  x <- c("2014 Educ")
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

Setup.Worksheet(tab.title = "Education14_Urban")
Setup.Worksheet(tab.title = "Education14_Rural")

#placing the right numbers in their place

#log expenditure
writeData(wb, "Education14_Urban", umpce.unw[[2,2]], startCol = 2, startRow = 5)
writeData(wb, "Education14_Urban", umpce[[2,2]], startCol = 3, startRow = 5)

#hhsize
x <- hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize > 2,rates], startCol = 2, startRow = 9)
x <- hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 8)
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize > 2,rates], startCol = 3, startRow = 9)

#dist hhsize
x <- dist.hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 14)
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize > 2,rates], startCol = 2, startRow = 15)
x <- dist.hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 14)
writeData(wb, "Education14_Urban", x[Sector == "URBAN" & hhsize > 2,rates], startCol = 3, startRow = 15)

#hh age structure
x <- age.str.unw[!(agegroup %in% "65+"),]
writeData(wb, "Education14_Urban", x[sector == "Urban",rates], startCol = 2, startRow = 20)
x <- age.str[!(agegroup %in% "65+"),]
writeData(wb, "Education14_Urban", x[sector == "Urban",rates], startCol = 3, startRow = 20)

##dist hh age structure
x <- dist.age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education14_Urban", x[sector == "Urban",rates], startCol = 2, startRow = 27)
x <- dist.age.str[!(age.group %in% "65+"),]
writeData(wb, "Education14_Urban", x[sector == "Urban",rates], startCol = 3, startRow = 27)

## religion and social group
writeData(wb, "Education14_Urban", rel.str.unw[[9,3]], startCol = 2, startRow = 34)
writeData(wb, "Education14_Urban", rel.str[[9,3]], startCol = 3, startRow = 34)
writeData(wb, "Education14_Urban", dist.rel.str.unw[[9,3]], startCol = 2, startRow = 35)
writeData(wb, "Education14_Urban", dist.rel.str[[9,3]], startCol = 3, startRow = 35)
writeData(wb, "Education14_Urban", dist.soc.grp.str.unw[[6,3]], startCol = 2, startRow = 36)
writeData(wb, "Education14_Urban", dist.soc.grp.str[[6,3]], startCol = 3, startRow = 36)

## household type
x <- hhtype.str.unw[Sector == "URBAN" & hhtype %in% c("Self-employment in agricultur", 
                                                      "Self-employment in non-agriculture"), sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 39)
x <- hhtype.str[Sector == "URBAN" & V1 %in% c("Self-employment in agricultur", 
                                              "Self-employment in non-agriculture"), sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 39)
x <- dist.hhtype.str.unw[Sector == "URBAN" & hhtype %in% c("Self-employment in agricultur", 
                                                      "Self-employment in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 40)
x <- dist.hhtype.str[Sector == "URBAN" & hhtype %in% c("Self-employment in agricultur", 
                                              "Self-employment in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 40)

x <- hhtype.str.unw[Sector == "URBAN" & hhtype %in% c("Casual labour in agricultur", 
                                                      "Casual labour in non-agriculture"), sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 41)
x <- hhtype.str[Sector == "URBAN" & V1 %in% c("Casual labour in agricultur", 
                                              "Casual labour in non-agriculture"), sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 41)
x <- dist.hhtype.str.unw[Sector == "URBAN" & hhtype %in% c("Casual labour in agricultur", 
                                                           "Casual labour in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 42)
x <- dist.hhtype.str[Sector == "URBAN" & hhtype %in% c("Casual labour in agricultur", 
                                                       "Casual labour in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 42)

x <- hhtype.str.unw[Sector == "URBAN" & hhtype == "Regular wage/Salary earning", sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 43)
x <- hhtype.str[Sector == "URBAN" & V1 == "Regular wage/Salary earning", sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 43)
x <- dist.hhtype.str.unw[Sector == "URBAN" & hhtype == "Regular wage/Salary earning", sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 44)
x <- dist.hhtype.str[Sector == "URBAN" & hhtype == "Regular wage/Salary earning", sum(rates)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 44)

## principal industry
x <- prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 47)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 47)
x <- prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 48)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 48)
x <- dist.prin.ind.str.unw[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Urban", x, startCol = 2, startRow = 49)
x <- dist.prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 49)








### write in rural statistics
#log expenditure
writeData(wb, "Education14_Rural", umpce.unw[[1,2]], startCol = 2, startRow = 5)
writeData(wb, "Education14_Rural", umpce[[1,2]], startCol = 3, startRow = 5)

#hhsize
x <- hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize > 2,rates], startCol = 2, startRow = 9)
x <- hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 8)
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize > 2,rates], startCol = 3, startRow = 9)

#dist hhsize
x <- dist.hhsize.str.unw[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 2, startRow = 14)
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize > 2,rates], startCol = 2, startRow = 15)
x <- dist.hhsize.str[as.numeric(hhsize) <= 5,]
x[,hhsize := as.integer(hhsize)]
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize <= 2,sum(rates)], startCol = 3, startRow = 14)
writeData(wb, "Education14_Rural", x[Sector == "RURAL" & hhsize > 2,rates], startCol = 3, startRow = 15)

#hh age structure
x <- age.str.unw[!(agegroup %in% "65+"),]
writeData(wb, "Education14_Rural", x[sector == "Rural",rates], startCol = 2, startRow = 20)
x <- age.str[!(agegroup %in% "65+"),]
writeData(wb, "Education14_Rural", x[sector == "Rural",rates], startCol = 3, startRow = 20)

##dist hh age structure
x <- dist.age.str.unw[!(age.group %in% "65+"),]
writeData(wb, "Education14_Rural", x[sector == "Rural",rates], startCol = 2, startRow = 27)
x <- dist.age.str[!(age.group %in% "65+"),]
writeData(wb, "Education14_Rural", x[sector == "Rural",rates], startCol = 3, startRow = 27)

## religion and social group
writeData(wb, "Education14_Rural", rel.str.unw[[1,3]], startCol = 2, startRow = 34)
writeData(wb, "Education14_Rural", rel.str[[1,3]], startCol = 3, startRow = 34)
writeData(wb, "Education14_Rural", dist.rel.str.unw[[1,3]], startCol = 2, startRow = 35)
writeData(wb, "Education14_Rural", dist.rel.str[[1,3]], startCol = 3, startRow = 35)
writeData(wb, "Education14_Rural", dist.soc.grp.str.unw[[2,3]], startCol = 2, startRow = 36)
writeData(wb, "Education14_Rural", dist.soc.grp.str[[2,3]], startCol = 3, startRow = 36)

## household type
x <- hhtype.str.unw[Sector == "RURAL" & hhtype %in% c("Self-employment in agricultur", 
                                                      "Self-employment in non-agriculture"), sum(N)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 39)
x <- hhtype.str[Sector == "RURAL" & V1 %in% c("Self-employment in agricultur", 
                                              "Self-employment in non-agriculture"), sum(N)]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 39)
x <- dist.hhtype.str.unw[Sector == "RURAL" & hhtype %in% c("Self-employment in agricultur", 
                                                           "Self-employment in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 40)
x <- dist.hhtype.str[Sector == "RURAL" & hhtype %in% c("Self-employment in agricultur", 
                                                       "Self-employment in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 40)

x <- hhtype.str.unw[Sector == "RURAL" & hhtype %in% c("Casual labour in agricultur", 
                                                      "Casual labour in non-agriculture"), sum(N)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 41)
x <- hhtype.str[Sector == "Rural" & V1 %in% c("Casual labour in agricultur", 
                                              "Casual labour in non-agriculture"), sum(N)]
writeData(wb, "Education14_Urban", x, startCol = 3, startRow = 41)
x <- dist.hhtype.str.unw[Sector == "RURAL" & hhtype %in% c("Casual labour in agricultur", 
                                                           "Casual labour in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 42)
x <- dist.hhtype.str[Sector == "RURAL" & hhtype %in% c("Casual labour in agricultur", 
                                                       "Casual labour in non-agriculture"), sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 42)

x <- hhtype.str.unw[Sector == "RURAL" & hhtype == "Regular wage/Salary earning", sum(N)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 43)
x <- hhtype.str[Sector == "RURAL" & V1 == "Regular wage/Salary earning", sum(N)]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 43)
x <- dist.hhtype.str.unw[Sector == "RURAL" & hhtype == "Regular wage/Salary earning", sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 44)
x <- dist.hhtype.str[Sector == "RURAL" & hhtype == "Regular wage/Salary earning", sum(rates)]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 44)

## principal industry
x <- prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 47)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 47)
x <- prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 48)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 48)
x <- dist.prin.ind.str.unw[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Rural", x, startCol = 2, startRow = 49)
x <- dist.prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Education14_Rural", x, startCol = 3, startRow = 49)





saveWorkbook(wb, "../R/DescEducation14.xlsx", overwrite = TRUE)
















