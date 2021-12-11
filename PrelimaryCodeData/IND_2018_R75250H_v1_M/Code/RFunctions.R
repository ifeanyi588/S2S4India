#### Load Libraries
library(readstata13)
library(data.table)
library(stringr)


################################### IMPORTANT NOTE!! #############################################
### All the scripts in this project are written to be run within the file structure created by 
### Laura in the OneDrive folder, "Laura Liliana Moreno Herrera - India". If this folder is 
### downloaded out of the drive, we suggest keep the same file structure and names while ensuring
### to set the working directory appropriately
##################################################################################################

#setwd("C:/Users/wb559885/WBG/Laura Liliana Moreno Herrera - India/IND_2018_R75252E_v1_M/Data/STATA")

### This function below is written to read in the DTA files on based a specific pattern upon 
### which the file ends. Only use this function when you want to load more than 1 file. 
### Otherwise it will break.

#### Select.dta args

Select.DTA <- function(find.pattern = "R75252L[0-9][0-9].dta"){
  
  #### read in the STATA datafiles
  dta.files <- list.files(pattern = find.pattern)
  
  df.list <- lapply(dta.files, read.dta13)
  df.list <- lapply(df.list, data.table)
  
  return(df.list)

}

## this function below computes means when we need to estimate hh and dist means for continuous numerics

HHdist.Mean <- function(hhdt = R75250L2h, var = "depratioh", 
                        tab.name = "DepRatio", pop.wgt = "pop_wgt"){
  hhdt <- hhdt[,c(var, "sector", "district.code", pop.wgt),with=F]
  colnames(hhdt) <- c("var", "sector", "district.code", "pop_wgt")
  mean.rates <- hhdt[,as.data.table(wtd.mean(var, weights = pop_wgt)), by = sector]
  colnames(mean.rates) <- c("sector", paste("Mean", tab.name, sep = "."))
  
  dist.wgts <- hhdt[,sum(pop_wgt),by=.(district.code, sector)]
  dist.mean.rates <- hhdt[,as.data.table(wtd.mean(var, weights = pop_wgt)),by=.(sector, district.code)]
  colnames(dist.mean.rates) <- c("sector", "district.code", paste("Mean",tab.name,sep="."))
  
  dist.mean.rates <- dist.mean.rates[dist.wgts, on = .(sector, district.code)]
  colnames(dist.mean.rates) <- c("sector", "district.code", "Mean", "wgts")
  
  dist.mean.rates <- dist.mean.rates[,wtd.mean(Mean, weights = wgts),by = .(sector)]
  colnames(dist.mean.rates) <- c("sector", paste("DistMean",tab.name,sep="."))
  
  return(list(mean.rates, dist.mean.rates))
  
}
## same as above except now it will be done with multiple by variables
HHdist.Mean2 <- function(hhdt = R75250L2h, var = "depratioh", 
                        tab.name = "DepRatio", pop.wgt = "pop_wgt", by.vars = c("year", "sector")){
  hhdt <- hhdt[,c(var, by.vars, "district.code", pop.wgt),with=F]
  colnames(hhdt) <- c("var", by.vars, "district.code", "pop_wgt")
  mean.rates <- hhdt[,as.data.table(wtd.mean(var, weights = pop_wgt)), by = by.vars]
  colnames(mean.rates) <- c(by.vars, paste("Mean", tab.name, sep = "."))
  
  dist.wgts <- hhdt[,sum(pop_wgt),by=c("district.code", by.vars)]
  dist.mean.rates <- hhdt[,as.data.table(wtd.mean(var, weights = pop_wgt)),by=c(by.vars, "district.code")]
  colnames(dist.mean.rates) <- c(by.vars, "district.code", paste("Mean",tab.name,sep="."))
  
  dist.mean.rates <- dist.mean.rates[dist.wgts, on = c(by.vars, "district.code")]
  colnames(dist.mean.rates) <- c(by.vars, "district.code", "Mean", "wgts")
  
  dist.mean.rates <- dist.mean.rates[,wtd.mean(Mean, weights = wgts),by = c(by.vars)]
  colnames(dist.mean.rates) <- c(by.vars, paste("DistMean",tab.name,sep="."))
  
  return(list(mean.rates, dist.mean.rates))
  
}

## this function below computes means when we need to estimate hh and dist means that require tabulations rather
## than means. This is useful for the data that is factors or character variables

HHdist.Tab <- function(hhdt = R75250L3h, var = "age.group", 
                       tab.name = "Age", pop.wgt = "hhwt"){
  
  hhdt <- hhdt[,c(var, "sector", "district.code", pop.wgt),with=F]
  colnames(hhdt) <- c("var", "sector", "district.code", "pop_wgt")
  mean.rates <- hhdt[,as.data.table(prop.table(wtd.table(var, weights = pop_wgt))), by = sector]
  colnames(mean.rates) <- c("sector", tab.name, paste(tab.name, "Rates", sep = "."))
  
  dist.wgts <- hhdt[,sum(pop_wgt),by=.(district.code, sector)]
  dist.mean.rates <- hhdt[,as.data.table(prop.table(wtd.table(var, weights = pop_wgt))),
                          by=.(sector, district.code)]
  colnames(dist.mean.rates) <- c("sector", "district.code", "Group", "Rates")
  
  dist.mean.rates <- dist.mean.rates[dist.wgts, on = .(sector, district.code)]
  colnames(dist.mean.rates) <- c("sector", "district.code", "Group", "Rates", "weights")
  
  dist.mean.rates <- dist.mean.rates[,as.data.table(wtd.mean(Rates, weights = weights)),
                                     by = .(Group, sector)]
  colnames(dist.mean.rates) <- c(tab.name, "sector", paste("DistMean",tab.name,sep="."))
  
  return(list(mean.rates, dist.mean.rates))

}

### same as HHdist.Tab2 except allows you to use multiple by variables
HHdist.Tab2 <- function(hhdt = R75250L3h, var = "age.group", 
                       tab.name = "Age", pop.wgt = "hhwt", by.vars = c("year", "sector")){
  
  hhdt <- hhdt[,c(var, by.vars, "district.code", pop.wgt),with=F]
  colnames(hhdt) <- c("var", by.vars, "district.code", "pop_wgt")
  mean.rates <- hhdt[,as.data.table(prop.table(wtd.table(var, weights = pop_wgt))), by = by.vars]
  colnames(mean.rates) <- c(by.vars, tab.name, paste(tab.name, "Rates", sep = "."))

  dist.wgts <- hhdt[,sum(pop_wgt),by=c("district.code", by.vars)]
  dist.mean.rates <- hhdt[,as.data.table(prop.table(wtd.table(var, weights = pop_wgt))),
                          by=c(by.vars, "district.code")]
  colnames(dist.mean.rates) <- c(by.vars, "district.code", "Group", "Rates")
  
  dist.mean.rates <- dist.mean.rates[dist.wgts, on = c(by.vars, "district.code")]
  colnames(dist.mean.rates) <- c(by.vars, "district.code", "Group", "Rates", "weights")
  
  dist.mean.rates <- dist.mean.rates[,as.data.table(wtd.mean(Rates, weights = weights)),
                                     by = c("Group", by.vars)]
  colnames(dist.mean.rates) <- c(tab.name, by.vars, paste("DistMean",tab.name,sep="."))
  
  return(list(mean.rates, dist.mean.rates))
  
}







