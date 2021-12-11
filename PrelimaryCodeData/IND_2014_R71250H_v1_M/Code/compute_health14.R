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
library(plyr)

#### always start by setting the working directory to the base project folder
p<-"C:/Users/wb559885/OneDrive - WBG/Documents/WorldBankProjects/INDIA/PovertyImputation2018/IndiaPoverty2018"
setwd(p)


## load functions from RFunctions.R file
source("IND_2018_R75250H_v1_M/Code/RFunctions.R")

### read in all the health dta files from the relevant folder

setwd("IND_2014_R71250H_v1_M/Data/STATA")

nsshealth14 <- Select.DTA(find.pattern = "R71250L[0-9][0-9]h.dta")

for (i in seq_along(nsshealth14)) {
  as.data.table(assign(paste("R71250L", i, "h", sep = ""), value = nsshealth14[[i]]))
}

rm(nsshealth14) # we dont need the data anymore

###can we replicate the number of villages/blocks, households and persons surveyed by state/UT

check.hhid <- R71250L1h[,length(unique(HHID)),by=State]
check.hhid[,sum(V1)] ### we get 65932 which means we match perfectly

###### compute real log household per capita expenditure
### first bring in the CPI data and update with 2014 numbers
# cpi.dt <- data.table(month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "June"),2),
#                      Sector = c(rep("RURAL", 6), rep("URBAN", 6)),
#                      cpi11 = c(rep(622.3333, 6), rep(199.5833, 6)),
#                      cpi14 = c(757, 757, 763, 771, 777, 785, 237, 238, 239, 242, 244, 246),
#                      SubRound = as.integer(rep(c(rep(1, 3), rep(2, 3)),2)))

# cpi.dt[,cpi14 := mean(cpi14), by = .(SubRound, Sector)]
# cpi.dt[,deflator := cpi11/cpi14]
# 
# cpi.dt <- unique(cpi.dt[,c("Sector", "cpi11", "cpi14", "SubRound", "deflator")])
#write cpi.dt to a csv into the R folder in this subsection's folder

cpi.dt <- as.data.table(readstata13::read.dta13("cpi2011.dta"))
cpi.dt <- cpi.dt[year == 2014 & month %in% 1:6,]
cpi.dt <- cpi.dt[,cpi14_urban := cpi_urban/cpi11_urban]
cpi.dt <- cpi.dt[,cpi14_rural := cpi_rural/cpi11_rural]

cpi14_urban <- cpi.dt[,mean(cpi14_urban, na.rm = T)]
cpi14_rural <- cpi.dt[,mean(cpi14_rural, na.rm = T)]

R71250L2h[,SubRound := as.integer(SubRound)]
R71250L2h[,Sector := ifelse(Sector == 1, "RURAL",
                            ifelse(Sector == 2, "URBAN", "NA"))]
R71250L2h[,sector := Sector]

# setkey(R71250L2h, SubRound, Sector)
# setkey(cpi.dt, SubRound, Sector)
# R71250L2h <- R71250L2h[cpi.dt]

R71250L2h[,hhsize := as.integer(b3_q1)]
R71250L2h[,hhexp := b3_q12]
# R71250L2h[,real.hhexp := hhexp*deflator]
# R71250L2h[,real.hhexp.pcap := real.hhexp/hhsize]
# R71250L2h[,lnreal.hhexp.pcap := log(real.hhexp.pcap)]
# R71250L2h[is.na(lnreal.hhexp.pcap) == TRUE | lnreal.hhexp.pcap == -Inf, lnreal.hhexp.pcap := log(1)]

#re-compute household level weights
R71250L2h[NSC == NSS,hhwt := MLT/100]
R71250L2h[NSC != NSS,hhwt := MLT/200]

R71250L2h[,cpi2014 := 0]
R71250L2h[sector == "RURAL", cpi2014 := cpi14_rural]
R71250L2h[sector == "URBAN", cpi2014 := cpi14_urban]

R71250L2h[sector == "URBAN",yearppp := 16.017724]
R71250L2h[sector == "RURAL",yearppp := 13.173453]


R71250L2h[,hhexp.pcap := hhexp/hhsize]
R71250L2h[,real.hhexp.pcap := hhexp.pcap/cpi2014/yearppp]

R71250L2h[,pop_wgt := hhwt*hhsize]

# umpce <- R71250L2h[,wtd.mean(lnreal.hhexp.pcap, weights = pop_wgt),by=Sector]

umpce <- R71250L2h[,wtd.mean(real.hhexp.pcap, weights = pop_wgt, na.rm = TRUE),by=Sector]
#umpce.unw[,umpce.unw := log(V1)]


### compute 2011 USD PPP equivalent for the consumption expenditures
## compute dollar cpi

# #R71250L2h[,umpce.ppp := real.hhexp.pcap/yearppp]
# umpce.ppp <- R71250L2h[,wtd.mean(umpce.ppp, weights = hhwt),by=Sector]
# 
# R71250L2h[,lnumpce.ppp := log(umpce.ppp)]
# R71250L2h[is.na(lnumpce.ppp) == TRUE | lnumpce.ppp == -Inf, lnumpce.ppp := log(1)]
# lnumpce.ppp <- R71250L2h[,wtd.mean(lnumpce.ppp, weights = hhwt),by=Sector]

### compute distribution of household size
hhsize.str <- R71250L2h[,as.data.table(prop.table(wtd.table(hhsize, weights = pop_wgt))), by = Sector]
colnames(hhsize.str) <- c("Sector", "hhsize", "rates")
hhsize.str.unw <- R71250L2h[,as.data.table(prop.table(table(hhsize))), by = Sector]
colnames(hhsize.str.unw) <- c("Sector", "hhsize", "rates")

### compute distribution of household size at the district level
#### compute stratum (district) level weights first by aggregation
l2.wgts <- R71250L2h[,as.data.table(sum(pop_wgt)),by=.(Stratum, Sector)]
dist.hhsize.str <- R71250L2h[,as.data.table(prop.table(wtd.table(hhsize, Stratum,
                                                            weights = pop_wgt),2)),
                        by = Sector]
colnames(dist.hhsize.str) <- c("Sector", "hhsize", "Stratum", "rates")
dist.hhsize.str <- dist.hhsize.str[l2.wgts, on = .(Stratum, Sector)]
colnames(dist.hhsize.str)[colnames(dist.hhsize.str) %in% "V1"] <- "weights"
dist.hhsize.str <- dist.hhsize.str[,wtd.mean(rates, weights = weights), by = .(Sector, hhsize)]
dist.hhsize.str.unw <- R71250L2h[,as.data.table(prop.table(table(hhsize, Stratum),2)), by = Sector]
colnames(dist.hhsize.str.unw) <- c("Sector", "hhsize", "Stratum", "rates")
dist.hhsize.str.unw <- dist.hhsize.str.unw[,mean(rates),by=.(Sector,hhsize)]

# R71250L2h[,dist.hhsize.ave := mean(hhsize,na.rm = TRUE), by = .(Stratum, Sector)]
# R71250L2h[,dist.hhsize.ave := as.integer(dist.hhsize.ave)]
# dist.hhsize.str <- R71250L2h[,as.data.table(prop.table(table(dist.hhsize.ave))),
#                              by = Sector]

### compute household age structure
R71250L3h[NSC == NSS,hhwt := MLT/100]
R71250L3h[NSC != NSS,hhwt := MLT/200]

R71250L3h[,age := as.numeric(b4_q5)]
R71250L3h[,age.group := cut(age, 
                            breaks = c(0,14,24,34,49,64,150), 
                            labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"),
                            include.lowest = TRUE)]
R71250L3h[,Sector := ifelse(Sector == 1, "RURAL",
                            ifelse(Sector == 2, "URBAN", "NA"))]
R71250L3h[,sector := Sector]

age.str <- R71250L3h[,as.data.table(prop.table(wtd.table(age.group, weights = hhwt))),by=Sector]
colnames(age.str) <- c("Sector", "age.group", "rates")
hhage.str.unw <- R71250L3h[,as.data.table(prop.table(table(age.group))),by=Sector]
colnames(hhage.str.unw) <- c("Sector", "age.group", "rates")

### compute district level household age structure
#### first compute the district level weights 
l3.wgts <- R71250L3h[,as.data.table(sum(hhwt)),by=.(Stratum, Sector)]
colnames(l3.wgts) <- c("Stratum", "Sector", "weights")
dist.age.str <- R71250L3h[,as.data.table(prop.table(wtd.table(age.group, 
                                                              weights = hhwt))),by=.(Stratum, Sector)]
colnames(dist.age.str) <- c("Stratum", "Sector", "age.group", "rates")
dist.age.str <- dist.age.str[l3.wgts, on =.(Stratum, Sector)]
dist.age.str <- dist.age.str[,wtd.mean(rates, weights = weights),by=.(age.group, Sector)]
colnames(dist.age.str) <- c("age.group", "Sector", "rates")

dist.age.str.unw <- R71250L3h[,as.data.table(prop.table(table(age.group))),by=.(Stratum, Sector)]
colnames(dist.age.str.unw) <- c("Stratum", "Sector", "age.group", "rates")
dist.age.str.unw <- dist.age.str.unw[,mean(rates),by=.(age.group, Sector)]
colnames(dist.age.str.unw) <- c("age.group", "Sector", "rates")

### compute religion and social group structure
R71250L2h[,religion := ifelse(b3_q5 == 1, "Hinduism",
                              ifelse(b3_q5 == 2, "Islam", 
                                     ifelse(b3_q5 == 3, "Christianity",
                                            ifelse(b3_q5 == 4, "Sikhism", 
                                                   ifelse(b3_q5 == 5, "Jainism", 
                                                          ifelse(b3_q5 == 6, "Buddhism", 
                                                                 ifelse(b3_q5 == 7, 
                                                                        "Zoroastrianism",
                                                                        ifelse(b3_q5 == 9, 
                                                                               "Others", 
                                                                               "NA"))))))))]
rel.str <-R71250L2h[,as.data.table(prop.table(wtd.table(religion, weights = pop_wgt))),by=Sector]
colnames(rel.str) <- c("Sector", "Religion", "rates")
rel.str.unw <-R71250L2h[,as.data.table(prop.table(table(religion))),by=Sector]
colnames(rel.str.unw) <- c("Sector", "Religion", "rates")

R71250L2h[,social.group := ifelse(b3_q6 == 1, "scheduled tribe", 
                                  ifelse(b3_q6 == 2, "scheduled caste",
                                         ifelse(b3_q6 == 3, "other backward class",
                                                ifelse(b3_q6 == 9, "others", "NA"))))]
soc.grp.str <-R71250L2h[,as.data.table(prop.table(wtd.table(social.group, weights = pop_wgt))),by=Sector]
colnames(soc.grp.str) <- c("Sector", "SocialGroup", "rates")
soc.grp.str.unw <-R71250L2h[,as.data.table(prop.table(table(social.group))),by=Sector]
colnames(soc.grp.str.unw) <- c("Sector", "SocialGroup", "rates")

### compute district level religion and social group structure
dist.rel.str <- R71250L2h[,as.data.table(prop.table(wtd.table(religion, 
                                                              weights = pop_wgt))), by = .(Stratum, Sector)]
colnames(dist.rel.str) <- c("Stratum", "Sector", "Religion", "rates")
dist.rel.str <- dist.rel.str[l2.wgts, on = .(Sector, Stratum)]
colnames(dist.rel.str) <- c("Stratum", "Sector", "Religion", "rates", "weights")
dist.rel.str <- dist.rel.str[,wtd.mean(rates, weights = weights), by=.(Sector, Religion)]
colnames(dist.rel.str) <- c("Sector", "Religion", "rates")

dist.rel.str.unw <- R71250L2h[,as.data.table(prop.table(table(religion))), by = .(Stratum, Sector)]
colnames(dist.rel.str.unw) <- c("Stratum", "Sector", "Religion", "rates")
dist.rel.str.unw <- dist.rel.str.unw[,mean(rates), by=.(Sector, Religion)]
colnames(dist.rel.str.unw) <- c("Sector", "Religion", "rates")

dist.soc.grp.str <- R71250L2h[,as.data.table(prop.table(wtd.table(social.group, weights = pop_wgt))), 
                              by = .(Stratum, Sector)]
colnames(dist.soc.grp.str) <- c("Stratum", "Sector", "SocialGroup", "rates")
dist.soc.grp.str <- dist.soc.grp.str[l2.wgts, on = .(Sector, Stratum)]
colnames(dist.soc.grp.str) <- c("Stratum", "Sector", "SocialGroup", "rates", "weights")
dist.soc.grp.str <- dist.soc.grp.str[,wtd.mean(rates, weights = weights), by=.(Sector, SocialGroup)]
colnames(dist.rel.str) <- c("Sector", "SocialGroup", "rates")

dist.soc.grp.str.unw <- R71250L2h[,as.data.table(prop.table(table(social.group))), by = .(Stratum, Sector)]
colnames(dist.soc.grp.str.unw) <- c("Stratum", "Sector", "SocialGroup", "rates")
dist.soc.grp.str.unw <- dist.soc.grp.str.unw[,mean(rates), by=.(Sector, SocialGroup)]
colnames(dist.soc.grp.str.unw) <- c("Sector", "SocialGroup", "rates")

## compute household type
R71250L2h[,hhtype := ifelse(HH_type == 11, "Self-Employed in Agriculture",
                            ifelse(HH_type == 12, "Self-Employed in Non-Agriculture",
                                   ifelse(HH_type == 13, "Regular Wage/Salary Earning",
                                          ifelse(HH_type == 14, "Casual Labour in Agriculture",
                                                 ifelse(HH_type == 15, "Casual Labour in Non-Agriculture",
                                                        ifelse(HH_type == 19, "Others",
                                                               ifelse(HH_type == 21, "Self-Employed",
                                                                      ifelse(HH_type == 22, "Regular Wage/Salary Earning",
                                                                             ifelse(HH_type == 23, "Casual Labour",
                                                                                    ifelse(HH_type == 29, "Others", "NA"))))))))))]

hhtype.str <- R71250L2h[,as.data.table(prop.table(wtd.table(hhtype, weights = pop_wgt))), by = Sector]
colnames(hhtype.str) <- c("Sector", "hhtype", "rates")
hhtype.str.unw <- R71250L2h[,as.data.table(prop.table(table(hhtype))), by = Sector]
colnames(hhtype.str.unw) <- c("Sector", "hhtype", "rates")
## compute household type (district level)
dist.hhtype.str <- R71250L2h[,as.data.table(prop.table(wtd.table(hhtype, weights = pop_wgt))), 
                              by = .(Stratum, Sector)]
colnames(dist.hhtype.str) <- c("Stratum", "Sector", "hhtype", "rates")
dist.hhtype.str <- dist.hhtype.str[l2.wgts, on = .(Sector, Stratum)]
colnames(dist.hhtype.str) <- c("Stratum", "Sector", "hhtype", "rates", "weights")
dist.hhtype.str <- dist.hhtype.str[,wtd.mean(rates, weights = weights), by=.(Sector, hhtype)]
colnames(dist.hhtype.str) <- c("Sector", "hhtype", "rates")

dist.hhtype.str.unw <- R71250L2h[,as.data.table(prop.table(table(hhtype))), by = .(Stratum, Sector)]
colnames(dist.hhtype.str.unw) <- c("Stratum", "Sector", "hhtype", "rates")
dist.hhtype.str.unw <- dist.hhtype.str.unw[,mean(rates), by=.(Sector, hhtype)]
colnames(dist.hhtype.str.unw) <- c("Sector", "hhtype", "rates")


# compute industry level breakdowns
## first we need to reclassify the NCO 2004 codes into principal industries

R71250L2h[,nic2008 := as.integer(b3_q2)]
R71250L2h[,principal.industry := cut(x = nic2008, breaks = c(0,3229,43900,99000),
                                     labels = c("Agriculture", "Industry", "Others"))]
prin.ind.str <- R71250L2h[,as.data.table(prop.table(wtd.table(principal.industry, weights = pop_wgt))),by=Sector]
colnames(prin.ind.str) <- c("Sector", "Principal.Industry", "rates")
prin.ind.str.unw <- R71250L2h[,as.data.table(prop.table(table(principal.industry))),by=Sector]
colnames(prin.ind.str.unw) <- c("Sector", "Principal.Industry", "rates")

#compute industry level breakdowns for the districts
##weighted
dist.prin.ind.str <- R71250L2h[,as.data.table(prop.table(wtd.table(principal.industry, 
                                                              weights = pop_wgt))),by=.(Sector, Stratum)]
colnames(dist.prin.ind.str) <- c("Sector", "Stratum", "Principal.Industry", "rates")
dist.prin.ind.str <- dist.prin.ind.str[l2.wgts, on = .(Sector, Stratum)]
dist.prin.ind.str <- dist.prin.ind.str[,wtd.mean(rates, weights = V1), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str) <- c("Principal.Industry", "Sector", "rates")
##unweighted
dist.prin.ind.str.unw <- R71250L2h[,as.data.table(prop.table(table(principal.industry))),by=.(Sector, Stratum)]
colnames(dist.prin.ind.str.unw) <- c("Sector", "Stratum", "Principal.Industry", "rates")
dist.prin.ind.str.unw <- dist.prin.ind.str.unw[,mean(rates), by = .(Principal.Industry, Sector)]
colnames(dist.prin.ind.str.unw) <- c("Principal.Industry", "Sector", "rates")


###### include more data to match up with additions made to the R712502h data
######for dependency ratio
R71250L3h[,age.dep := ifelse((age <= 15 | age >= 64), 1, 0)]
R71250L3h[,depratioh := mean(age.dep), by = HHID]
########place this back into the HH level file for computations
depratio.dt <- unique(R71250L3h[,c("HHID", "depratioh")])
R71250L2h <- depratio.dt[R71250L2h, on = "HHID"]

###### create energy source use for cooking variable
# (01) firewood and chips; (07) dung cake; (02) LPG; (08) other biogas (03) other natural gas
# (10) charcoal; (04) dung cake; (11) electricity (incl. generated by solar or wind power generators); 
# (05) kerosene (19) others (06) coke ,coal; (12) no cooking arrangement

R71250L2h[,energy_cook := as.integer(b3_q10)]
R71250L2h[,energy_cook := cut(energy_cook, breaks = 0:10,
                              labels = c("cokecoal", "firewood_chips", "lpg", "gobargas",   
                                         "dungcake", "charcoal", "kerosene", "electricity", 
                                         "others", "nocooking"))]
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(energy_cook)])
old.names <- c("cokecoal", "firewood_chips", "lpg", "gobargas",   
               "dungcake", "charcoal", "kerosene", "electricity", 
               "others", "nocooking")
new.names <- c("Dcokecoalh", "Dfirewoodh", "Dlpgh", "Dgobargash", "Ddungcakeh", "Dcharcoalh", "Dkeroseneh",
               "Delectricityh", "Dothersh", "Dnocookingh")
setnames(R71250L2h, old = old.names, new = new.names)


#### compute the district level variables
R71250L2h[,district.code := State_District]
R71250L2h[,c("Dcokecoal", "Dfirewood", "Dlpg", "Dgobargas", "Ddungcake", "Dcharcoal", "Dkerosene",
             "Delectricity", "Dothers", "Dnocooking") := 
            list(wtd.mean(Dcokecoalh, weights = pop_wgt), wtd.mean(Dfirewoodh, weights = pop_wgt), 
                 wtd.mean(Dlpgh, weights = pop_wgt), wtd.mean(Dgobargash, weights = pop_wgt),
                 wtd.mean(Ddungcakeh, weights = pop_wgt), wtd.mean(Dcharcoalh, weights = pop_wgt),  
                 wtd.mean(Dkeroseneh, weights = pop_wgt), wtd.mean(Delectricityh, weights = pop_wgt),
                 wtd.mean(Dothersh, weights = pop_wgt), wtd.mean(Dnocookingh, weights = pop_wgt)),
          by = district.code]

#### include the gender of the household head
R71250L3h[,district.code := State_District]
R71250L3h[,gender := as.integer(b4_q4)] ##male - 1, female - 2, transgender - 3
R71250L3h[,gender := ifelse(gender == 1, "Male",
                            ifelse(gender == 2, "Female",
                                   ifelse(gender == 3, "Transgender", "NA")))]
######## marital status variable
R71250L3h[,marital := as.integer(b4_q6)] ##never married - 1, currently married - 2, widowed - 3, divorced/separated - 4
R71250L3h[,marital := ifelse(marital == 1, "Never Married", 
                             ifelse(marital == 2, "Married",
                                    ifelse(marital == 4, "Divorced/Separated",
                                           ifelse(marital == 3, "Widowed", "NA"))))]
## education variable
# not literate 01; literate without any schooling 02; without formal schooling:through NFEC 03; 
# through TLC/ AEC 04; others 05; with formal schooling: below primary 06; primary 07; upper primary/middle 08;
# secondary 10; higher secondary 11; diploma/certificate course (up to secondary) 12; 
# diploma/certificate course (higher secondary) 13; diploma/certificate course (graduation & above) 14;
# graduate 15; postgraduate & above 16
R71250L3h[,educ := as.integer(b4_q7)]
R71250L3h[,educat7 := ifelse(educ == 1, "No education",
                             ifelse(educ %in% c(7,8), "Primary (complete or incomplete)",
                                    ifelse(educ %in% 10:14, "Secondary(complete or incomplete)",
                                           ifelse(educ %in% c(15,16), "Tertiary (complete or incomplete)", "NA"))))]

R71250L3h[,educ := cut(educ, breaks = c(0:8, 10:16), labels = c("not literate", "literate without any schooling",
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
R71250L3h[,hhid := HHID]
R71250L2h[,hhid := HHID]
hhhead.info <- R71250L3h[b4_q3 == "1", c("gender", "marital", "educ", "age", "hhid", "educat7")]

##add hh prefix to names
old.names <- colnames(hhhead.info)[!(colnames(hhhead.info) == "hhid")]
new.names <- paste("hh", old.names, sep = "")
setnames(hhhead.info, old.names, new.names)

####create variables for education 
######### for the household
R71250L2h <- R71250L2h[hhhead.info, on = "hhid"]
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(hheducat7)])
old.names <- c("No education", "Primary (complete or incomplete)", 
               "Secondary(complete or incomplete)", 
               "Tertiary (complete or incomplete)")
new.names <- c("Dnoeduch", "Dprimaryh", "Dsecondaryh", "Dtertiaryh")
setnames(R71250L2h, old = old.names, new = new.names)

######### for the district
R71250L2h[,c("Dnoeduc", "Dprimary", "Dsecondary", "Dtertiary") := 
            list(wtd.mean(Dnoeduch, weights = pop_wgt), wtd.mean(Dprimaryh, weights = pop_wgt),
                 wtd.mean(Dsecondaryh, weights = pop_wgt), wtd.mean(Dtertiaryh, weights = pop_wgt)),
          by = district.code]


###### creating imputation variables for household head gender
######### for the household
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(hhgender)])
old.names <- c("Male", "Female")
new.names <- c("Dmaleh", "Dfemaleh")
setnames(R71250L2h, old = old.names, new = new.names)
########## for district level
R71250L2h[,c("Dmale", "Dfemale") := 
            list(wtd.mean(Dmaleh, weights = pop_wgt), wtd.mean(Dfemaleh, weights = pop_wgt)),
          by = district.code]

####### creating imputation variables for the hh marital status
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(hhmarital)])
old.names <- c("Married", "Divorced/Separated", "Never Married", "Widowed")
new.names <- c("Dmarriedh", "Ddivorcedh", "Dnevermarriedh", "Dwidowedh")
setnames(R71250L2h, old = old.names, new = new.names)
########## for district level
R71250L2h[,c("Dmarried", "Ddivorced", "Dnevermarried", "Dwidowed") := 
            list(wtd.mean(Dmarriedh, weights = pop_wgt), wtd.mean(Ddivorcedh, weights = pop_wgt),
                 wtd.mean(Dnevermarriedh, weights = pop_wgt), wtd.mean(Dwidowedh, weights = pop_wgt)),
          by = district.code]


### use newly created tables to create computations
hhmarital.rates <- HHdist.Tab(hhdt = R71250L2h, var = "hhmarital", tab.name = "Marital",
                              pop.wgt = "pop_wgt")
hheduc.rates <- HHdist.Tab(hhdt = R71250L2h, var = "hheducat7", tab.name = "Education", pop.wgt = "pop_wgt")
hhgender.rates <- HHdist.Tab(hhdt = R71250L2h, var = "hhgender", tab.name = "Sex", pop.wgt = "pop_wgt")
depratio.rates <- HHdist.Mean(hhdt = R71250L2h)
##relabel energy_cook variable first to use before using it to create tables
old.names <- R71250L2h[,unique(energy_cook)]
new.names <- c("Firewood & Chips", "Liquiefied Petroleum Gas", "Dung Cake", "Electricity", 
               "Kerosene", "Coke, Coal", "Gobar Gas",
               "Charcoal", "Others", "No cooking")
R71250L2h[,energy_cook := mapvalues(energy_cook, old.names, new.names)]
# for (i in seq_along(old.names)){
#   R71250L2h[energy_cook == old.names[i], energy_cook := new.names[i]]
# }


energy.cook.rates <- HHdist.Tab(hhdt = R71250L2h, var = "energy_cook", tab.name = "EnergyCook", 
                                pop.wgt = "pop_wgt")
energy.cook.rates[[1]] <- energy.cook.rates[[1]][EnergyCook.Rates != 0,]
energy.cook.rates[[2]] <- energy.cook.rates[[2]][DistMean.EnergyCook != 0,]

##include the rainfall data
rainfall.dt <- fread("../R/rainfall.csv")
R71250L2h[,district.code := as.integer(district.code)]

R71250L2h <- rainfall.dt[R71250L2h, on = "district.code"]

old.names <- colnames(R71250L2h)[grepl("rainfall_", colnames(R71250L2h))]
new.names <- str_remove_all(colnames(R71250L2h)[grepl("rainfall_", colnames(R71250L2h))], "_")
setnames(R71250L2h, old = old.names, new = new.names)

old.names <- colnames(R71250L2h)[grepl("rainfallsq_", colnames(R71250L2h))]
new.names <- str_remove_all(colnames(R71250L2h)[grepl("rainfallsq_", colnames(R71250L2h))], "_")
setnames(R71250L2h, old = old.names, new = new.names)

##### for household size
R71250L2h[,dhhsizecat := cut(x = hhsize, breaks = c(0, 2, 3, 4, 5, 6, 1000))]
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(dhhsizecat)])
old.names <- c("(6,1e+03]","(4,5]", "(3,4]", "(2,3]", "(0,2]")
new.names <- c("Dhhsizecat6h","Dhhsizecat5h", "Dhhsizecat4h", "Dhhsizecat3h", "Dhhsizecat2h")
setnames(R71250L2h, old = old.names, new = new.names)

R71250L2h[,c("Dhhsizecat4", "Dhhsizecat5", "Dhhsizecat3", "Dhhsizecat2", "Dhhsizecat6") := 
            list(wtd.mean(Dhhsizecat4h, weights = pop_wgt), wtd.mean(Dhhsizecat5h, weights = pop_wgt),
                 wtd.mean(Dhhsizecat3h, weights = pop_wgt), wtd.mean(Dhhsizecat2h, weights = pop_wgt),
                 wtd.mean(Dhhsizecat6h, weights = pop_wgt)), 
          by = .(district.code)]

##### for age groups
R71250L3h[,age.group2 := cut(age, breaks = c(0,14,24,34,49,64,150),
                             labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"))]
R71250L3h[age == 0,age.group2 := "0-14"]
R71250L3h <- cbind(R71250L3h, R71250L3h[,dummy.code(age.group2)])
old.names <- c("0-14", "15-24", "25-34", "35-49", "50-64", "65+")
new.names <- c("Dagecat1h", "Dagecat2h", "Dagecat3h", 
               "Dagecat4h", "Dagecat5h", "Dagecat6h")
setnames(R71250L3h, old = old.names, new = new.names)

#compute proportion of HH members within each age group by household
hhage.str <- 
  R71250L3h[,list(mean(Dagecat1h, na.rm = TRUE), mean(Dagecat2h, na.rm = TRUE), 
                  mean(Dagecat3h, na.rm = TRUE),
                  mean(Dagecat4h, na.rm = TRUE), mean(Dagecat5h, na.rm = TRUE), 
                  mean(Dagecat6h, na.rm = TRUE)), 
            by = hhid]
colnames(hhage.str) <- c("hhid", "Dagecat1h", "Dagecat2h", "Dagecat3h", 
                         "Dagecat4h", "Dagecat5h", "Dagecat6h")
#merge back into a household level file
R71250L2h <- R71250L2h[hhage.str, on = "hhid"]

R71250L2h[,c("Dagecat1", "Dagecat2", "Dagecat3", "Dagecat4", "Dagecat5", "Dagecat6") := 
            list(wtd.mean(Dagecat1h, weights = pop_wgt), wtd.mean(Dagecat2h, weights = pop_wgt),
                 wtd.mean(Dagecat3h, weights = pop_wgt), wtd.mean(Dagecat4h, weights = pop_wgt),
                 wtd.mean(Dagecat5h, weights = pop_wgt), wtd.mean(Dagecat6h, weights = pop_wgt)),
          by = .(district.code)]

##### for religion and social caste
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(religion)])
R71250L2h[,hinduh := Hinduism]
R71250L2h[,hindu := wtd.mean(hinduh, weights = pop_wgt), by = .(district.code)]
R71250L2h[,scsth := ifelse(social.group %in% c("other backward class", "scheduled caste", "scheduled tribe"), 1, 0)]
R71250L2h[,scst := wtd.mean(scsth, weights = pop_wgt), by = .(district.code)]

##### for the household types (self employment, casual labor (only in urban areas), regular wage)
R71250L2h[,hhtype.new := ifelse(grepl("Self-Employed", x = hhtype), "selfemployedh",
                                ifelse(grepl("Casual Labour", x = hhtype), "casualurbanh",
                                       ifelse(grepl("Regular Wage", x = hhtype), "regwageurbanh", "otherh")))]

R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(hhtype.new)]) #creating the dummies for each level of hhtype.new
R71250L2h[,c("selfemployed", "casualurban", "regwageurban", "other") := 
            list(wtd.mean(selfemployedh, weights = pop_wgt), wtd.mean(casualurbanh, weights = pop_wgt),
                 wtd.mean(regwageurbanh, weights = pop_wgt), wtd.mean(otherh, weights = pop_wgt)),
          by = district.code]

##### occupation levels (middle and high skill)
R71250L2h[,occ.class := as.integer(substr(b3_q3, 1, 1))]
R71250L2h[,occ.level := ifelse(occ.class == 1| occ.class == 2| occ.class == 3, "highskillocch",
                               ifelse(occ.class == 4| occ.class == 5, "middleskillocch",
                                      ifelse(occ.class == 6| occ.class == 7| occ.class == 8| occ.class == 9,
                                             "lowskillocch", "othersh")))]
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(occ.level)])
R71250L2h[,c("highskillocc", "middleskillocc", "lowskillocc") :=
            list(wtd.mean(highskillocch, weights = pop_wgt), wtd.mean(middleskillocch, weights = pop_wgt),
                 wtd.mean(lowskillocch, weights = pop_wgt)), by = district.code]

##### industry levels
R71250L2h[,industry := ifelse(grepl("Agric", principal.industry), "agrih",
                              ifelse(grepl("Ind", principal.industry), "indh",
                                     ifelse(grepl("Other", principal.industry), "serviceh", "NA")))]
R71250L2h <- cbind(R71250L2h, R71250L2h[,dummy.code(industry)])
R71250L2h[,c("agri", "ind", "service") := 
            list(wtd.mean(agrih, weights = pop_wgt), wtd.mean(indh, weights = pop_wgt),
                 wtd.mean(serviceh, weights = pop_wgt)),
          by = district.code]

######for dependency ratio
R71250L3h[,age.dep := ifelse((age <= 15 | age >= 64), 1, 0)]
R71250L3h[,depratioh := mean(age.dep), by = hhid]
########place this back into the HH level file for computations
depratio.dt <- unique(R71250L3h[,c("hhid", "depratioh")])
R71250L2h <- depratio.dt[R71250L2h, on = "hhid"]

depratioh.mean <- R71250L2h[,wtd.mean(depratioh, weights = pop_wgt), by = sector]
######## create the district level means variables
R71250L2h[,depratio := wtd.mean(depratioh, weights = pop_wgt), by = .(district.code)]
depratio.mean <- R71250L2h[,wtd.mean(depratio, weights = pop_wgt), by = sector]

#levels for the full energy source that need to be used for created the tables
levels <- c("Liquiefied Petroleum Gas", "Firewood & Chips", "No cooking", "Others", "Dung Cake", 
            "Coke, Coal", "Electricity", "Kerosene", "Other Natural Gas", "Charcoal", "Other Biogas",
            "Gobar Gas")



####relabel or create as needed the rest of the variables to be used for imputation
R71250L2h[,hhsizeh := hhsize]

old.names <- colnames(R71250L2h)[grepl("rainfallsq", colnames(R71250L2h))]
new.names <- c("rainfallQ1sq", "rainfallQ2sq", "rainfallQ3sq", "rainfallQ4sq")
setnames(R71250L2h, old.names, new.names)

##create the rainfall splines
# ### using the knots in the same locations as created for the imputation data in 2011
# R71250L2h[,DrainfallQ1 := cut(rainfallQ1, breaks = c(-100, -0.615, -0.295, -0.038, 100))]
# R71250L2h[,DrainfallQ1k1 := ifelse(DrainfallQ1 == "(-100,-0.615]", rainfallQ1, 0)]
# R71250L2h[,DrainfallQ1k2 := ifelse(DrainfallQ1 == "(-0.615,-0.295]", rainfallQ1, 0)]
# R71250L2h[,DrainfallQ1k3 := ifelse(DrainfallQ1 == "(-0.295,-0.038]", rainfallQ1, 0)]
# R71250L2h[,DrainfallQ1k4 := ifelse(DrainfallQ1 == "(-0.038,100]", rainfallQ1, 0)]
# 
# 
# R71250L2h[,DrainfallQ2 := cut(rainfallQ2, breaks = c(-100, -0.377, -0.140, -0.220, 100))]
# R71250L2h[,DrainfallQ2k1 := ifelse(DrainfallQ2 == "(-100,-0.377]", rainfallQ2, 0)]
# R71250L2h[,DrainfallQ2k2 := ifelse(DrainfallQ2 == "(-0.377,-0.22]", rainfallQ2, 0)]
# R71250L2h[,DrainfallQ2k3 := ifelse(DrainfallQ2 == "(-0.22,-0.14]", rainfallQ2, 0)]
# R71250L2h[,DrainfallQ2k4 := ifelse(DrainfallQ2 == "(-0.14,100]", rainfallQ2, 0)]
# 
# R71250L2h[,DrainfallQ3 := cut(rainfallQ3, breaks = c(-100, 0.15, 0.53, 0.79, 100))]
# R71250L2h[,DrainfallQ3k1 := ifelse(DrainfallQ3 == "(-100,0.15]", rainfallQ3, 0)]
# R71250L2h[,DrainfallQ3k2 := ifelse(DrainfallQ3 == "(0.15,0.53]", rainfallQ3, 0)]
# R71250L2h[,DrainfallQ3k3 := ifelse(DrainfallQ3 == "(0.53,0.79]", rainfallQ3, 0)]
# R71250L2h[,DrainfallQ3k4 := ifelse(DrainfallQ3 == "(0.79,100]", rainfallQ3, 0)]
# 
# R71250L2h[,DrainfallQ4 := cut(rainfallQ4, breaks = c(-100, -0.844, -0.703, -0.4, 100))]
# R71250L2h[,DrainfallQ4k1 := ifelse(DrainfallQ4 == "(-100,-0.844]", rainfallQ4, 0)]
# R71250L2h[,DrainfallQ4k2 := ifelse(DrainfallQ4 == "(-0.844,-0.703]", rainfallQ4, 0)]
# R71250L2h[,DrainfallQ4k3 := ifelse(DrainfallQ4 == "(-0.703,-0.4]", rainfallQ4, 0)]
# R71250L2h[,DrainfallQ4k4 := ifelse(DrainfallQ4 == "(-0.4,100]", rainfallQ4, 0)]

### create rainfall spline variables
R71250L2h[,DrainfallQ1k1 := ifelse(rainfallQ1 < -0.615, 0, rainfallQ1 - (-0.615))]
R71250L2h[,DrainfallQ1k2 := ifelse(rainfallQ1 < -0.295, 0, rainfallQ1 - (-0.295))]
R71250L2h[,DrainfallQ1k3 := ifelse(rainfallQ1 < -0.038, 0, rainfallQ1 - (-0.038))]
R71250L2h[,DrainfallQ2k1 := ifelse(rainfallQ2 < -0.377, 0, rainfallQ2 - (-0.377))]
R71250L2h[,DrainfallQ2k2 := ifelse(rainfallQ2 < -0.140, 0, rainfallQ2 - (-0.140))]
R71250L2h[,DrainfallQ2k3 := ifelse(rainfallQ2 < -0.220, 0, rainfallQ2 - (-0.220))]
R71250L2h[,DrainfallQ3k1 := ifelse(rainfallQ3 < 0.15, 0, rainfallQ3 - (0.15))]
R71250L2h[,DrainfallQ3k2 := ifelse(rainfallQ3 < 0.53, 0, rainfallQ3 - (0.53))]
R71250L2h[,DrainfallQ3k3 := ifelse(rainfallQ3 < 0.79, 0, rainfallQ3 - (0.79))]
R71250L2h[,DrainfallQ4k1 := ifelse(rainfallQ4 < -0.844, 0, rainfallQ4 - (-0.844))]
R71250L2h[,DrainfallQ4k2 := ifelse(rainfallQ4 < -0.703, 0, rainfallQ4 - (-0.703))]
R71250L2h[,DrainfallQ4k3 := ifelse(rainfallQ4 < -0.400, 0, rainfallQ4 - (-0.400))]

### test out the square terms as well
R71250L2h[,DrainfallQ1k1sq := DrainfallQ1k1^2]
R71250L2h[,DrainfallQ1k2sq := DrainfallQ1k2^2]
R71250L2h[,DrainfallQ1k3sq := DrainfallQ1k3^2]
R71250L2h[,DrainfallQ2k1sq := DrainfallQ2k1^2]
R71250L2h[,DrainfallQ2k2sq := DrainfallQ2k2^2]
R71250L2h[,DrainfallQ2k3sq := DrainfallQ2k3^2]
R71250L2h[,DrainfallQ3k1sq := DrainfallQ3k1^2]
R71250L2h[,DrainfallQ3k2sq := DrainfallQ3k2^2]
R71250L2h[,DrainfallQ3k3sq := DrainfallQ3k3^2]
R71250L2h[,DrainfallQ4k1sq := DrainfallQ4k1^2]
R71250L2h[,DrainfallQ4k2sq := DrainfallQ4k2^2]
R71250L2h[,DrainfallQ4k3sq := DrainfallQ4k3^2]


### create the id_sae, qhclust and qhweight
R71250L2h[,id_sae := 1:.N]
R71250L2h[,district_code := district.code]
R71250L2h[,qhweight := pop_wgt]
R71250L2h[,qhclust := as.integer(Vill_Blk_Slno)]


save.dta13(R71250L2h, "scssae14.dta")
save.dta13(R71250L2h[sector == "RURAL",], "scssae14r.dta")
save.dta13(R71250L2h[sector == "URBAN",], "scssae14u.dta")

#save.image(file = "../R/health14.RData")
######################################################################################################################


## writing the results to file
wb <- createWorkbook()

Setup.Worksheet <- function(tab.title = "Health14_Urban"){
  
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
  
  x <- c("Primary Energy Source for Cooking", levels)
  writeData(wb, tab.title, x, startCol = 1, startRow = 95)
  
  x <- c("Primary Energy Source for Cooking (District Avg)", 
         levels)
  writeData(wb, tab.title, x, startCol = 1, startRow = 109)
  
  x <- c("District Rainfall Shock", "July - September", "July - September (squared)", "October - December", 
         "October - December (squared)", "January - March", "January - March (squared)",
         "April - June", "April - June (squared)")
  writeData(wb, tab.title, x, startCol = 1, startRow = 123)
  
}

#placing the right numbers in their place
Setup.Worksheet(tab.title = "Health14_Urban")
Setup.Worksheet(tab.title = "Health14_Rural")


#log expenditure
writeData(wb, "Health14_Rural", umpce[[1,2]], startCol = 2, startRow = 5)

#hhsize
x <- hhsize.str[,hhsize := as.integer(hhsize)]
writeData(wb, "Health14_Rural", x[Sector == "RURAL" & hhsize <= 2,
                                  sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Health14_Rural", x[Sector == "RURAL" & hhsize > 2 & hhsize <= 5, 
                                  rates], startCol = 2, startRow = 9)
writeData(wb, "Health14_Rural", x[Sector == "RURAL" & hhsize >= 6, 
                                  sum(rates)], startCol = 2, startRow = 12)

##hhsize dist
x <- R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat2, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 15)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat3, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 16)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat4, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 17)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat5, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 18)
x <- 1 - R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat2, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat3, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat4, weights = pop_wgt)] -
  R71250L2h[sector == "RURAL",wtd.mean(Dhhsizecat5, weights = pop_wgt)] 
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 19)

#hh age structure
x <- age.str
writeData(wb, "Health14_Rural", x[Sector == "RURAL",rates], startCol = 2, startRow = 22)

##dist hh age structure
x <- R71250L2h[sector == "RURAL",wtd.mean(Dagecat1, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 30)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dagecat2, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 31)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dagecat3, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 32)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dagecat4, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 33)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 34)
x <- 1 - R71250L2h[sector == "RURAL",wtd.mean(Dagecat1, weights = pop_wgt)] -
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat2, weights = pop_wgt)] -
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat3, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat4, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 35)

## religion 
writeData(wb, "Health14_Rural", rel.str[Sector == "RURAL" & Religion == "Hinduism",rates], 
          startCol = 2, startRow = 38)
writeData(wb, "Health14_Rural", rel.str[Sector == "RURAL" & Religion != "Hinduism",sum(rates)], 
          startCol = 2, startRow = 39)

## religion dist
writeData(wb, "Health14_Rural", R71250L2h[sector == "RURAL",wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 42)
writeData(wb, "Health14_Rural", R71250L2h[sector == "RURAL",1 - wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 43)

## social group 
writeData(wb, "Health14_Rural", soc.grp.str[Sector == "RURAL" & 
                                                   SocialGroup %in% c("other backward class",
                                                                      "scheduled caste",
                                                                      "scheduled tribe"), 
                                                 sum(rates)], 
          startCol = 2, startRow = 46)
writeData(wb, "Health14_Rural", soc.grp.str[Sector == "RURAL" & SocialGroup == "others", rates], 
          startCol = 2, startRow = 47)

## social grp dist
writeData(wb, "Health14_Rural", R71250L2h[Sector == "RURAL",wtd.mean(scst, weights = pop_wgt)],
          startCol = 2, startRow = 50)
writeData(wb, "Health14_Rural", 1-R71250L2h[Sector == "RURAL",wtd.mean(scst, weights = pop_wgt)],
          startCol = 2, startRow = 51)

## household type
x <- hhtype.str[Sector == "RURAL" & grepl("Regular Wage/Salary Earning", hhtype),sum(rates)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 54)
x <- hhtype.str[Sector == "RURAL" & grepl("Self-Employed", hhtype),sum(rates)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 55)
x <- hhtype.str[Sector == "RURAL" & grepl("Casual Labour", hhtype), sum(rates)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 56)
x <- hhtype.str[Sector == "RURAL" & grepl("Others", hhtype), sum(rates)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 57)

## household type dist
x <- R71250L2h[sector == "RURAL",wtd.mean(regwageurban, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 60)
x <- R71250L2h[sector == "RURAL",wtd.mean(selfemployed, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 61)
x <- R71250L2h[sector == "RURAL",wtd.mean(casualurban, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 62)
x <- R71250L2h[sector == "RURAL",wtd.mean(1 - (regwageurban + selfemployed + casualurban), 
                                          weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 63)

## principal industry
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 66)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Industry",rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 67)
x <- prin.ind.str[Sector == "RURAL" & Principal.Industry == "Others",rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 68)

## principal industry dist
x <- R71250L2h[sector == "RURAL", wtd.mean(agri, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 71)
x <- R71250L2h[sector == "RURAL", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 72)
x <- R71250L2h[sector == "RURAL", 1 - wtd.mean(agri, weights = pop_wgt) -
                 wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 73)


## educational attainment for the household head
# x <- hheduc.rates[[1]][sector == "URBAN", Education.Rates]
# writeData(wb, "Health14_Urban", x, startCol = 3, startRow = 52)
# x <- hheduc.rates[[2]][sector == "URBAN", DistMean.Education]
# writeData(wb, "Health14_Urban", x, startCol = 3, startRow = 58)
# 
## household gender for the household head
x <- hhgender.rates[[1]][sector == "RURAL" & Sex != "Transgender", Sex.Rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 76)
## household gender for the household head district
x <- R71250L2h[sector == "RURAL",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 80)
x <- R71250L2h[sector == "RURAL",wtd.mean(1 - Dfemale, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 81)

## household head marital status
x <- hhmarital.rates[[1]][sector == "RURAL" & Marital == "Married",Marital.Rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 84)
x <- 1 - hhmarital.rates[[1]][sector == "RURAL" & Marital == "Married",sum(Marital.Rates)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 85)

## household head marital status district
x <- R71250L2h[sector == "RURAL",wtd.mean(Dmarried, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 88)
x <- R71250L2h[sector == "RURAL",wtd.mean(1 - Dmarried, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 89)

## dependency ratio of the household head
x <- depratio.rates[[1]][sector == "RURAL", Mean.DepRatio]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 92)
x <- R71250L2h[sector == "RURAL",wtd.mean(depratio, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 93)

## primary source of energy for cooking within the household
### add missing rows to the energy.cook.rates data

addtoenergy <- 
data.table(sector = c(rep("RURAL", 2), rep("URBAN", 2)),
           EnergyCook = rep(c("Other Natural Gas", "Other Biogas"),2),
           EnergyCook.Rates = rep(NA, 4))
energy.cook.rates[[1]] <- rbind(energy.cook.rates[[1]], addtoenergy)
energy.cook.rates[[1]][,EnergyCook := factor(EnergyCook, levels = levels)]
setorder(energy.cook.rates[[1]], EnergyCook)
x <- energy.cook.rates[[1]][sector == "RURAL",EnergyCook.Rates]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 96)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dlpg, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 110)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dfirewood, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 111)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dnocooking, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 112)
x <- R71250L2h[sector == "RURAL",wtd.mean(1 - Dlpg - Dfirewood - Dnocooking - 
                                            Ddungcake - Dcokecoal - Delectricity -
                                            Dkerosene - Dcharcoal - Dgobargas, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 113)
x <- R71250L2h[sector == "RURAL",wtd.mean(Ddungcake, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 114)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dcokecoal, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 115)
x <- R71250L2h[sector == "RURAL",wtd.mean(Delectricity, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 116)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dkerosene, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 117)
x <- ""
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 118)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dcharcoal, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 119)
x <- ""
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 120)
x <- R71250L2h[sector == "RURAL",wtd.mean(Dgobargas, weights = pop_wgt)]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 121)

## district rainfall shocks
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 124)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 125)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 126)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 127)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 128)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 129)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 130)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Rural", x, startCol = 2, startRow = 131)



##write results into the Rural sheet

#log expenditure
writeData(wb, "Health14_Urban", umpce[[2,2]], startCol = 2, startRow = 5)

#hhsize
x <- hhsize.str[,hhsize := as.integer(hhsize)]
writeData(wb, "Health14_Urban", x[Sector == "URBAN" & hhsize <= 2,
                                  sum(rates)], startCol = 2, startRow = 8)
writeData(wb, "Health14_Urban", x[Sector == "URBAN" & hhsize > 2 & hhsize <= 5, 
                                  rates], startCol = 2, startRow = 9)
writeData(wb, "Health14_Urban", x[Sector == "URBAN" & hhsize >= 6, 
                                  sum(rates)], startCol = 2, startRow = 12)

##hhsize dist
x <- R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat2, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 15)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat3, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 16)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat4, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 17)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat5, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 18)
x <- 1 - R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat2, weights = pop_wgt)] - 
  R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat3, weights = pop_wgt)] - 
  R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat4, weights = pop_wgt)] -
  R71250L2h[sector == "URBAN",wtd.mean(Dhhsizecat5, weights = pop_wgt)] 
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 19)

#hh age structure
x <- age.str
writeData(wb, "Health14_Urban", x[Sector == "URBAN",rates], startCol = 2, startRow = 22)

##dist hh age structure
x <- R71250L2h[sector == "URBAN",wtd.mean(Dagecat1, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 30)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dagecat2, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 31)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dagecat3, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 32)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dagecat4, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 33)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 34)
x <- 1 - R71250L2h[sector == "RURAL",wtd.mean(Dagecat1, weights = pop_wgt)] -
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat2, weights = pop_wgt)] -
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat3, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat4, weights = pop_wgt)] - 
  R71250L2h[sector == "RURAL",wtd.mean(Dagecat5, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 35)

## religion 
writeData(wb, "Health14_Urban", rel.str[Sector == "URBAN" & Religion == "Hinduism",rates], 
          startCol = 2, startRow = 38)
writeData(wb, "Health14_Urban", rel.str[Sector == "URBAN" & Religion != "Hinduism",sum(rates)], 
          startCol = 2, startRow = 39)

## religion dist
writeData(wb, "Health14_Urban", R71250L2h[sector == "URBAN",wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 42)
writeData(wb, "Health14_Urban", R71250L2h[sector == "URBAN",1 - wtd.mean(hindu, weights = pop_wgt)],
          startCol = 2, startRow = 43)

## social group 
writeData(wb, "Health14_Urban", soc.grp.str[Sector == "URBAN" & SocialGroup %in% c("other backward class",
                                                                                         "scheduled caste",
                                                                                         "scheduled tribe"), 
                                                 sum(rates)], 
          startCol = 2, startRow = 46)
writeData(wb, "Health14_Urban", soc.grp.str[Sector == "URBAN" & SocialGroup == "others",rates], 
          startCol = 2, startRow = 47)

## social grp dist
writeData(wb, "Health14_Urban", R71250L2h[sector == "URBAN",wtd.mean(scst, weights = pop_wgt)],
          startCol = 2, startRow = 50)
writeData(wb, "Health14_Urban", R71250L2h[sector == "URBAN",wtd.mean(1 - scst, weights = pop_wgt)],
          startCol = 2, startRow = 51)

## household type
x <- hhtype.str[Sector == "URBAN" & grepl("Regular Wage/Salary Earning", hhtype),sum(rates)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 54)
x <- hhtype.str[Sector == "URBAN" & grepl("Self-Employed", hhtype),sum(rates)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 55)
x <- hhtype.str[Sector == "URBAN" & grepl("Casual Labour", hhtype), sum(rates)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 56)
x <- hhtype.str[Sector == "URBAN" & grepl("Others", hhtype), sum(rates)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 57)

## household type dist
x <- R71250L2h[Sector == "URBAN",wtd.mean(regwageurban, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 60)
x <- R71250L2h[Sector == "URBAN",wtd.mean(selfemployed, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 61)
x <- R71250L2h[Sector == "URBAN",wtd.mean(casualurban, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 62)
x <- R71250L2h[Sector == "URBAN",wtd.mean(1 - (regwageurban + selfemployed + casualurban), 
                                          weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 63)

## principal industry
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Agriculture",rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 66)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Industry",rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 67)
x <- prin.ind.str[Sector == "URBAN" & Principal.Industry == "Others",rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 68)

## principal industry dist
x <- R71250L2h[sector == "URBAN", wtd.mean(agri, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 71)
x <- R71250L2h[sector == "URBAN", wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 72)
x <- R71250L2h[sector == "URBAN", 1 - wtd.mean(agri, weights = pop_wgt) -
                 wtd.mean(ind, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 73)


## educational attainment for the household head
# x <- hheduc.rates[[1]][sector == "URBAN", Education.Rates]
# writeData(wb, "Health14_Urban", x, startCol = 3, startRow = 52)
# x <- hheduc.rates[[2]][sector == "URBAN", DistMean.Education]
# writeData(wb, "Health14_Urban", x, startCol = 3, startRow = 58)
# 
## household gender for the household head
x <- hhgender.rates[[1]][sector == "URBAN" & Sex != "Transgender", Sex.Rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 76)
## household gender for the household head district
x <- R71250L2h[sector == "URBAN",wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 80)
x <- R71250L2h[sector == "URBAN",1-wtd.mean(Dfemale, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 81)

## household head marital status
x <- hhmarital.rates[[1]][sector == "URBAN" & Marital == "Married",Marital.Rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 84)
x <- 1-hhmarital.rates[[1]][sector == "URBAN" & Marital == "Married",sum(Marital.Rates)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 85)

## household head marital status district
x <- R71250L2h[sector == "URBAN",wtd.mean(Dmarried, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 88)
x <- R71250L2h[sector == "URBAN",wtd.mean(1 - Dmarried, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 89)

## dependency ratio of the household head
x <- depratio.rates[[1]][sector == "URBAN", Mean.DepRatio]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 92)
x <- R71250L2h[sector == "URBAN",wtd.mean(depratio, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 93)

## primary source of energy for cooking within the household
x <- energy.cook.rates[[1]][sector == "URBAN", EnergyCook.Rates]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 96)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dlpg, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 110)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dfirewood, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 111)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dnocooking, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 112)
x <- R71250L2h[sector == "URBAN",wtd.mean(1 - Dlpg - Dfirewood - Dnocooking - 
                                            Ddungcake - Dcokecoal - Delectricity -
                                            Dkerosene - Dcharcoal - Dgobargas, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 113)
x <- R71250L2h[sector == "URBAN",wtd.mean(Ddungcake, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 114)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dcokecoal, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 115)
x <- R71250L2h[sector == "URBAN",wtd.mean(Delectricity, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 116)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dkerosene, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 117)
x <- ""
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 118)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dcharcoal, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 119)
x <- ""
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 120)
x <- R71250L2h[sector == "URBAN",wtd.mean(Dgobargas, weights = pop_wgt)]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 121)

## district rainfall shocks
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 124)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ3, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 125)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 126)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ4, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 127)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 128)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ1, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 129)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 130)
x <- R71250L2h[,as.data.table(wtd.mean(rainfallsqQ2, weights = pop_wgt)),by = sector][[1,2]]
writeData(wb, "Health14_Urban", x, startCol = 2, startRow = 131)

saveWorkbook(wb, "deschealth14.xlsx", overwrite = TRUE)



































