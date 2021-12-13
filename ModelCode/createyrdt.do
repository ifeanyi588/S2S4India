cap log close
cap log using createyrdt, log replace 
**** quick script to create 2004/5, 2009/10, 2011/12 from the newmaster.dta file

use "D:\Poverty_Estimates_India\data\finaldata\newmaster.dta", clear

keep if year == 2004.5
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae04.dta", replace
keep if urban == 1
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae04u.dta", replace
use "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae04.dta", clear
keep if urban == 0
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae04r.dta", replace

use "D:\Poverty_Estimates_India\data\finaldata\newmaster.dta", clear

keep if year == 2009.5
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae09.dta", replace
keep if urban == 1
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae09u.dta", replace
use "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae09.dta", clear
keep if urban == 0
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae09r.dta", replace

use "D:\Poverty_Estimates_India\data\finaldata\newmaster.dta", clear

keep if year == 2014.5
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae14.dta", replace
keep if urban == 1
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae14u.dta", replace
use "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae14.dta", clear
keep if urban == 0
save "D:\Poverty_Estimates_India\IndiaModelRuns\data\replication\sae14r.dta", replace

log close