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
library(sae)

setwd("C:/Users/wb559885/WBG/Laura Liliana Moreno Herrera - India/IND_2018_R75250H_v1_M/Data")
## we are writing this script to replicate the analysis of (Newhouse and Vyas, 2019) towards performing the imputation
## exercise needed to estimate poverty in India for 2018

load("C:/Users/wb559885/WBG/Laura Liliana Moreno Herrera - India/IND_2018_R75250H_v1_M/Data/R/health18.RData")

## read in the data

## begin preparing variables for the imputation
##### for household size
R75250L2h[,dhhsizecat := cut(x = hhsize, breaks = c(0, 2, 3, 4, 5, 6, 1000))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(dhhsizecat)])
colnames(R75250L2h)[colnames(R75250L2h) %in% c("(4,5]", "(3,4]", "(2,3]", "(0,2]")] <- 
  c("Dhhsizecat5H", "Dhhsizecat4H", "Dhhsizecat3H", "Dhhsizecat2H")

R75250L2h[,c("Dhhsizecat4D", "Dhhsizecat5D", "Dhhsizecat3D", "Dhhsizecat2D") := 
            list(wtd.mean(Dhhsizecat4H, weights = MULT), wtd.mean(Dhhsizecat5H, weights = MULT),
              wtd.mean(Dhhsizecat3H, weights = MULT), wtd.mean(Dhhsizecat2H, weights = MULT)), by = .(district.code)]
##### for age groups
R75250L3h[,age.group2 := cut(age, breaks = c(0,14,24,34,49,64,150),
                             labels = c("0-14", "15-24", "25-34", "35-49", "50-64", "65+"))]
R75250L3h[age == 0,age.group2 := "0-14"]
R75250L3h <- cbind(R75250L3h, R75250L3h[,dummy.code(age.group2)])
colnames(R75250L3h)[colnames(R75250L3h) %in% c("0-14", "15-24", "25-34", "35-49", "50-64", "65+")] <- 
  c("Dagecat1H", "Dagecat2H", "Dagecat3H", "Dagecat4H", "Dagecat5H", "Dagecat6H")
#compute proportion of HH members within each age group by household
hhage.str <- 
R75250L3h[,list(mean(Dagecat1H, na.rm = TRUE), mean(Dagecat2H, na.rm = TRUE), mean(Dagecat3H, na.rm = TRUE),
                mean(Dagecat4H, na.rm = TRUE), mean(Dagecat5H, na.rm = TRUE), mean(Dagecat6H, na.rm = TRUE)), 
          by = hhid]
colnames(hhage.str) <- c("hhid", "Dagecat1H", "Dagecat2H", "Dagecat3H", "Dagecat4H", "Dagecat5H", "Dagecat6H")
#merge back into a household level file
R75250L2h <- R75250L2h[hhage.str, on = "hhid"]

R75250L2h[,c("Dagecat1D", "Dagecat2D", "Dagecat3D", "Dagecat4D", "Dagecat5D", "Dagecat6D") := 
            list(wtd.mean(Dagecat1H, weights = MULT), wtd.mean(Dagecat2H, weights = MULT),
                 wtd.mean(Dagecat3H, weights = MULT), wtd.mean(Dagecat4H, weights = MULT),
                 wtd.mean(Dagecat5H, weights = MULT), wtd.mean(Dagecat6H, weights = MULT)),
          by = .(district.code)]

##### for religion and social caste
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(religion)])
R75250L2h[,hinduH := Hinduism]
R75250L2h[,hinduD := wtd.mean(hinduH, weights = MULT), by = .(district.code)]
R75250L2h[,scstH := ifelse(social.group %in% c("backward class", "scheduled caste", "scheduled tribe"), 1, 0)]
R75250L2h[,scstD := wtd.mean(scstH, weights = MULT), by = .(district.code)]

##### for the household types (self employment, casual labor (only in urban areas), regular wage)
R75250L2h[,hhtype.new := ifelse(grepl("Self-Employed", x = hhtype), "selfemployedH",
                                ifelse(grepl("Casual Labour", x = hhtype), "casualurbanH",
                                       ifelse(grepl("Regular Wage", x = hhtype), "regwageurbanH", "otherH")))]

R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(hhtype.new)]) #creating the dummies for each level of hhtype.new
R75250L2h[,c("selfemployedD", "casualurbanD", "regwageurbanD", "otherD") := 
            list(wtd.mean(selfemployedH, weights = MULT), wtd.mean(casualurbanH, weights = MULT),
                 wtd.mean(regwageurbanH, weights = MULT), wtd.mean(otherH, weights = MULT)),
          by = district.code]

##### occupation levels (middle and high skill)
R75250L2h[,occ.class := as.integer(substr(var7, 1, 1))]
R75250L2h[,occ.level := ifelse(occ.class == 1| occ.class == 2| occ.class == 3, "highskilloccH",
                               ifelse(occ.class == 4| occ.class == 5, "middleskilloccH",
                                      ifelse(occ.class == 6| occ.class == 7| occ.class == 8| occ.class == 9, 
                                             "lowskilloccH", "othersH")))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(occ.level)])
R75250L2h[,c("highskilloccD", "middleskilloccD", "lowskilloccD") := 
            list(wtd.mean(highskilloccH, weights = MULT), wtd.mean(middleskilloccH, weights = MULT),
                 wtd.mean(lowskilloccH, weights = MULT)), by = district.code]

##### industry levels
R75250L2h[,industry := ifelse(grepl("Agric", principal.industry), "agriH",
                              ifelse(grepl("Ind", principal.industry), "indH",
                                     ifelse(grepl("Other", principal.industry), "serviceH", "NA")))]
R75250L2h <- cbind(R75250L2h, R75250L2h[,dummy.code(industry)])
R75250L2h[,c("agriD", "indD") := 
            list(wtd.mean(agriH, weights = MULT), wtd.mean(indH, weights = MULT)),
          by = district.code]


## compute the rainfall shock variables
##### first read in the data
rainfall.dt <- as.data.table(read.dta13("STATA/data_chirps.dta"))
##### aggregate the data to quarterly data
rainfall.dt[,quarter := ifelse(month %in% 1:3, "Q1", 
                               ifelse(month %in% 4:6, "Q2",
                                      ifelse(month %in% 7:9, "Q3",
                                             ifelse(month %in% 10:12, "Q4", "NA"))))]
rainfall.dt[,avgqrain := mean(mean),by=quarter] #compute average quarterly rainfall
rainfall.dt[,difference := (mean - avgqrain)/avgqrain] #compute the deviations from the quarterly means
rainfall.dt <- rainfall.dt[(year == 2017 & month %in% 7:12) | (year == 2018 & month %in% 1:6) |
                             (year == 2014),]
#######create district variable that matches with what we have in the NSS health and education surveys
rainfall.dt[,state := as.character(l1_code)]
rainfall.dt[str_length(state) == 1, state := paste("0", state, sep = "")]
rainfall.dt[,district.code := paste(state, l2_code, sep = ".")]

rainfall.dt <- rainfall.dt[,c("difference", "district.code", "quarter", "year")]
rainfall.dt[,diff.sq := difference^2]
ave.shock <- rainfall.dt[,list(mean(difference), mean(diff.sq)),by=.(quarter, year)]

colnames(rainfall.dt) <- c("district.code", "year", "rainfallQ1D", "rainfallQ2D", "rainfallQ3D", "rainfallQ4D")
rainfall.dt[,c("rainfallQ1sqD", "rainfallQ2sqD", "rainfallQ3sqD", "rainfallQ4sqD") := 
              list(rainfallQ1D^2, rainfallQ2D^2, rainfallQ3D^2, rainfallQ4D^2)]
#merge rainfall data into the health survey
rainfall.dt[R75250L2h, on = "district.code"]

### include all the time interactions as well 
R75250L2h[subround == 1 | subround == 2, time := 17]
R75250L2h[subround == 3 | subround == 4, time := 18]
##### interact with all the columns
R75250L2h[,Dhhsizecat2HT := Dhhsizecat2H*time]
R75250L2h[,Dhhsizecat3HT := Dhhsizecat3H*time]
R75250L2h[,Dhhsizecat4HT := Dhhsizecat4H*time]
R75250L2h[,Dhhsizecat5HT := Dhhsizecat5H*time]
R75250L2h[,Dhhsizecat2DT := Dhhsizecat2D*time]
R75250L2h[,Dhhsizecat3DT := Dhhsizecat3D*time]
R75250L2h[,Dhhsizecat4DT := Dhhsizecat4D*time]
R75250L2h[,Dhhsizecat5DT := Dhhsizecat5D*time]
R75250L2h[,Dagecat1HT := Dagecat1H*time]
R75250L2h[,Dagecat2HT := Dagecat2H*time]
R75250L2h[,Dagecat3HT := Dagecat3H*time]
R75250L2h[,Dagecat4HT := Dagecat4H*time]
R75250L2h[,Dagecat5HT := Dagecat5H*time]
R75250L2h[,Dagecat1DT := Dagecat1D*time]
R75250L2h[,Dagecat2DT := Dagecat2D*time]
R75250L2h[,Dagecat3DT := Dagecat3D*time]
R75250L2h[,Dagecat4DT := Dagecat4D*time]
R75250L2h[,Dagecat5DT := Dagecat5D*time]
R75250L2h[,hinduDT := hinduD*time]
R75250L2h[,hinduHT := hinduH*time]
R75250L2h[,scstHT := scstH*time]
R75250L2h[,scstDT := scstD*time]
R75250L2h[,selfemployedHT := selfemployedH*time]
R75250L2h[,casualurbanHT := casualurbanH*time]
R75250L2h[,regwageurbanHT := regwageurbanH*time]
R75250L2h[,selfemployedDT := selfemployedD*time]
R75250L2h[,casualurbanDT := casualurbanD*time]
R75250L2h[,regwageurbanDT := regwageurbanD*time]
R75250L2h[,lowskilloccHT := lowskilloccH*time]
R75250L2h[,middleskilloccHT := middleskilloccH*time]
R75250L2h[,highskilloccHT := highskilloccH*time]
R75250L2h[,lowskilloccDT := lowskilloccD*time]
R75250L2h[,middleskilloccDT := middleskilloccD*time]
R75250L2h[,highskilloccDT := highskilloccD*time]
R75250L2h[,agriDT := agriD*time]
R75250L2h[,agriHT := agriH*time]
R75250L2h[,indDT := indD*time]
R75250L2h[,indHT := indH*time]

### proceeding to the imputation stage
##### prepare the data to be imputed in a 2017/18 health file

###### load coefficient vectors for rural and urban models
rural.coef <- rbind(as.data.table(read.dta13("STATA/col1_rural.dta")), 
                    as.data.table(read.dta13("STATA/col2_rural.dta")))
urban.coef <- rbind(as.data.table(read.dta13("STATA/col1_urban.dta")),
                    as.data.table(read.dta13("STATA/col2_rural.dta")))
colnames(rural.coef) <- colnames(urban.coef) <- c("var.name", "coef")
#transform in the correct fileshape
rural.coef <- as.data.table(t(rural.coef))
colnames(rural.coef) <- as.character(as.data.frame(rural.coef[1,]))
rural.coef <- rural.coef[-1,]
rural.coef <- rural.coef[,lapply(.SD, as.numeric)]
urban.coef <- as.data.table(t(urban.coef))
colnames(urban.coef) <- as.character(as.data.frame(urban.coef[1,]))
urban.coef <- urban.coef[-1,]
urban.coef <- urban.coef[,lapply(.SD, as.numeric)]

### matrix multiplication of coefficients with columns available in our 2017/18 data.
##### in the urban sector
imp.vars <- colnames(urban.coef)[colnames(urban.coef) %in% colnames(R75250L2h)]
urban.coef <-urban.coef[,imp.vars,with=F]

urban.impute.dt <- R75250L2h[sector == "URBAN",imp.vars,with=FALSE][,Map("*",.SD,urban.coef)]
urban.impute.dt[, lnumpce := apply(urban.impute.dt, 1, sum, na.rm = TRUE)]
urban.impute.dt[,umpce := exp(lnumpce)]

urban.impute.dt[,poor := ifelse(umpce < 1.9, 1, 0)]

##### in the rural sector
imp.vars <- colnames(rural.coef)[colnames(rural.coef) %in% colnames(R75250L2h)]
rural.coef <-rural.coef[,imp.vars,with=F]

rural.impute.dt <- R75250L2h[sector == "RURAL",imp.vars,with=FALSE][,Map("*",.SD,rural.coef)]
rural.impute.dt[, lnumpce := apply(rural.impute.dt, 1, sum, na.rm = TRUE)]
rural.impute.dt[,umpce := exp(lnumpce)]

rural.impute.dt[,poor := ifelse(umpce < 1.9, 1, 0)]

######################################################################################################################













































