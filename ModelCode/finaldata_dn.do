/*****************************************************************************************************
******************************************************************************************************
**                                                                                                  **
**                  SOUTH ASIA REGIONAL PROGRAM: INDIA SURVEY TO SURVEY IMPUTATION                  **
**                                                                                                  **
** COUNTRY			INDIA
** COUNTRY ISO CODE	IND
** YEAR				2011
** SURVEY NAME		NATIONAL SAMPLE SURVEY `y'TH ROUND 
*					HOUSEHOLD SCHEDULE 1.0 : CONSUMER EXPENDITURE
** SURVEY AGENCY	GOVERNMENT OF INDIA NATIONAL SAMPLE SURVEY ORGANISATION
** CREATED  BY Pallavi Vyas 
** MODIFIED BY Pallavi Vyas 
** Modified	 7/31/2017
** MOdified 2/20/18 by David Newhouse 
** 1. Express all expenditure variables in log per capita terms 
** 2. Include aggregate expenditure at cluster level 
** 3. Include more detailed measures of age and household size that allow for non-linearities 
** 4. 
                                                                                             
******************************************************************************************************
*****************************************************************************************************/

	local durexp transportr recreationr crockeryr furniturer jewelryr appliancesr kitchenr itr
	local dur transport recreation crockery furniture jewelry appliances kitchen it
	local duri transporti recreationi crockeryi furniturei jewelryi appliancesi kitcheni iti  	
	local desc rural hhsize depratio hindu islam otherreligion scsturban scstrural selfemployed casualurban casualrural	regwageurban regwagerural otherhhtype 
	local desc1 rural hindu islam otherreligion scsturban scstrural selfemployed casualurban casualrural regwageurban regwagerural otherhhtype 
	local all 61 66 68 72
	local no61 66 68 72
	local no72 61 66 68
		
/*****************************************************************************************************
*                                                                                                    *
                                Final Data
*                                                                                                    *
*****************************************************************************************************/

********************************************************* Datasets for Summary Statistics *******************************************

* Merging in the descriptive statistics, durable goods, consumer expenditure
foreach y in `all'{
clear
* Household Characteristics
use "$path\data\NSSO`y'\nss`y'_3.dta"
* Dependency Ratio 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_depratio.dta"
drop _merge
* Age categories 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_age_cat.dta"
drop _merge
 

* Durable Shares
merge 1:1 ID using "$path\data\NSSO`y'\shr_id_`y'.dta"
drop _merge
* Amount spent on miscellaneous services
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_10.dta"
drop _merge
* MPCE 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_3mpce.dta"
drop _merge
gen popwt=hhwt*hhsize 
* Creating occupation codes
save "$path\data\replication\NSSO`y'\descdata`y'.dta", replace
}

exit 

** processing rainfall data to create mean rainfall by quarter 
use "$path\data\raindata\SummarizedPrecipitationData_toSTata" in 1/35, replace 

drop y*2017* 
*egen avg_rain_Q1=rowmean(y*_01 y*_02 y*_03) 
*egen avg_rain_Q2=rowmean(y*_04 y*_05 y*_06) 
*egen avg_rain_Q3=rowmean(y*_07 y*_08 y*_09) 
*egen avg_rain_Q4=rowmean(y*_10 y*_11 y*_12) 

foreach month in 01 02 03 04 05 06 07 08 09 10 11 12 {
egen sd_M`month'=rowsd(y*_`month')
egen avg_M`month'=rowmean(y*_`month') 
}

foreach year of numlist 2004 2009 2011 2014  { 
	foreach month in 07 08 09 10 11 12 {  
		gen std_M`month'_`year'=(y`year'_`month'-avg_M`month')/sd_M`month' 
	} 
	local year=`year'+1 
	foreach month in 01 02 03 04 05 06 {  
		gen std_M`month'_`year'=(y`year'_`month'-avg_M`month')/sd_M`month' 
	} 
} 

egen rainfallY_201415=rowmean(std_*_2014 std_*_2015) 
egen rainfallY_201112=rowmean(std_*_2011 std_*_2012) 
egen rainfallY_200910=rowmean(std_*_2009 std_*_2010) 
egen rainfallY_200405=rowmean(std_*_2004 std_*_2005) 

foreach year in 2004 2009 2011 2014 { 
egen rainfall_`year'Q3=rowmean(std_M07_`year' std_M08_`year' std_M09_`year') 
egen rainfall_`year'Q4=rowmean(std_M10_`year' std_M11_`year' std_M12_`year') 
local year=`year'+1 
egen rainfall_`year'Q1=rowmean(std_M01_`year' std_M02_`year' std_M03_`year') 
egen rainfall_`year'Q2=rowmean(std_M04_`year' std_M05_`year' std_M06_`year') 
} 



 





recode gid (0=1) (1=35) (2=28) (3=18) (4=7) (5=30) (6=24) (7=6) (8=2) (9=29) (10=32) (11=31) (12=27) (13=14) (14=17) (15=15) (16=13) ///
            (17=21) (18=3) (19=8) (20=11) (21=33) (22=16) (23=19) (24=12) (25=10) (26=4) (27=22) (28=26) (29=25) (30=20) (31=23) (32=34) ///
			(33=9) (34=5) ,gen(state) 
* tostring state, replace format(%-02.0f) 
keep avg* sd*  state rainfallY* rainfall_*Q* 

save "$path\data\raindata\avg_rain", replace  



/* Merging in the poverty line for each household from the harmonized data sets
datalibweb, coun(IND) y(2004) t(SARMD) sur(IND_2004_NSS61-SCH1.0_v01_M_v03_A_SARMD) filen(IND_2004_NSS61-SCH1.0_v01_M_v03_A_SARMD_IND.dta)
save "$data61\povertyline61.dta", replace
*/

use "$data61\povertyline61.dta", clear

rename idh ID
merge m:1 ID using "$data61\descdata61.dta"
drop _merge
duplicates drop ID, force
gen Nhh=1 
replace year=2004 if year==.
* rainfall by month and state 
*merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2004* mean_rainfall_2005*) keep(1 3) 
*tab _m  
*drop _m 

merge n:1 state using "$path\data\raindata\avg_rain", keepusing(rainfallY_2004 rainfall_2004Q* rainfall_2005Q*)  
tab _m 
drop _m 
rename rainfallY_2004* rainfallY 
rename rainfall_2004* rainfall*
rename rainfall_2005* rainfall*

gen highskill=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskill = ((occup_e==4)| (occup_e==5))
gen lowskill=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskill "Bureaucrats, Professional and Technicians"
label var middleskill "Clerks and Service Related"
label var lowskill "Agri, Craft, Machine, Elementary"

save "$data61\finaldata61.dta", replace

/*datalibweb, coun(IND) y(2009) t(SARMD) sur(IND_2009_NSS66-SCH1.0-T1_v01_M_v03_A_SARMD) filen(IND_2009_NSS66-SCH1.0-T1_v01_M_v03_A_SARMD_IND.dta)
save "$data66\povertyline66.dta", replace
*/

use "$data66\povertyline66.dta", clear
rename idh ID 
destring ID, replace
merge m:1 ID using "$data66\descdata66.dta"
drop _merge
duplicates drop ID, force
gen Nhh=1 
replace year=2009 if year==.
* state rainfall 
* merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2009* mean_rainfall_2010*) keep(1 3) 
* tab _m  
* drop _m 
merge n:1 state using "$path\data\raindata\avg_rain", keepusing(rainfallY_2009 rainfall_2009Q* rainfall_2010Q*)  
tab _m 
drop _m 
rename rainfall_2009* rainfall*
rename rainfall_2010* rainfall*
rename rainfallY_2009* rainfallY
gen highskill=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskill = ((occup_e==4)| (occup_e==5))
gen lowskill=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskill "Bureaucrats, Professional and Technicians"
label var middleskill "Clerks and Service Related"
label var lowskill "Agri, Craft, Machine, Elementary"
save "$data66\finaldata66.dta", replace

/*datalibweb, coun(IND) y(2011) t(SARMD) sur(IND_2011_NSS68-SCH1.0-T1_v01_M_v03_A_SARMD) filen(IND_2011_NSS68-SCH1.0-T1_v01_M_v03_A_SARMD_IND.dta)
save "$data68\povertyline68.dta", replace
*/

use "$data68\povertyline68.dta", clear
rename idh ID
destring ID, replace
merge m:1 ID using "$data68\descdata68.dta"
drop _merge
duplicates drop ID, force
gen Nhh=1 
replace year=2011 if year==.
* state rainfall 
* merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2011* mean_rainfall_2012*) keep(1 3) 
* tab _m  
* drop _m 
merge n:1 state using "$path\data\raindata\avg_rain", keepusing(rainfallY_2011 rainfall_2011Q* rainfall_2012Q*)  
tab _m 
drop _m 

rename rainfall_2011* rainfall*
rename rainfall_2012* rainfall*
rename rainfallY_2011* rainfallY
gen highskill=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskill = ((occup_e==4)| (occup_e==5))
gen lowskill=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskill "Bureaucrats, Professional and Technicians"
label var middleskill "Clerks and Service Related"
label var lowskill "Agri, Craft, Machine, Elementary"
save "$data68\finaldata68.dta", replace

use "$data72\descdata72.dta", clear
gen urban=(sector==2)
gen Nhh=1 
replace year=2014 if year==.
** replace Telenaga with AP because Telenaga was split from AP in 2014 
replace state=28 if state==36 

* state rainfall 
merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2014* mean_rainfall_2015*) keep(1 3) 
tab _m  
drop _m 
merge n:1 state using "$path\data\raindata\avg_rain", keepusing(rainfallY_2014 rainfall_2014* rainfall_2015*)  
tab _m 
drop _m 

rename mean_rainfall_2014* rainfall* 
rename mean_rainfall_2015* rainfall* 
rename rainfallY_2014* rainfallY
gen highskill=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskill = ((occup_e==4)| (occup_e==5))
gen lowskill=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskill "Bureaucrats, Professional and Technicians"
label var middleskill "Clerks and Service Related"
label var lowskill "Agri, Craft, Machine, Elementary"
save "$data72\finaldata72.dta", replace


*************************************************** Creating Regional Level Variables ************************************************
********* Regional Stats *********** 
foreach y in `all'{
clear
use "$path\data\replication\NSSO`y'\finaldata`y'.dta"
cap gen poor_int=. 
* Consumption: Means by Region
collapse (mean) `duri'  mpcer poor_int hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion agri ind services scst transpr hhr communir crockeryr recr (rawsum) Nhh ///
(rawsum) hhwt popwt [pw=popwt], by(stat_reg) 
label var transporti "Prop Purchased Transport Equipment"
label var recreationi "Prop Purchased  Recreation Equipment"
label var crockeryi "Prop Purchased  Crockery and Utensils"
label var furniturei "Prop Purchased  Furniture and Fixtures"
label var jewelryi "Prop Purchased  Jewelry and Ornaments"
label var appliancesi "Prop Purchased  Household Appliances"
label var kitcheni "Prop Purchased  Kitchen Equipment"
label var iti "Prop Purchased  IT and Communication Devices"
save "$path\data\replication\NSSO`y'\varsmeans_reg`y'.dta", replace

clear
use "$path\data\replication\NSSO`y'\finaldata`y'.dta"
* Durables and HHs in different industries: Sums by Region
collapse (sum) `duri' agrihh=agri indhh=ind serviceshh=services Nhh  [pw=popwt], by(stat_reg) 
foreach var of local duri{
rename `var' `var'hh
gen prop`var'=`var'hh/Nhh
}
foreach v in agri ind services{
gen prop`v'=`v'hh/Nhh
drop `v'hh
}
save "$path\data\replication\NSSO`y'\varssums_reg`y'.dta", replace
}

*********** Regional Stats By Urban/Rural **************
foreach y in `all'{
use "$path\data\replication\NSSO`y'\finaldata`y'.dta", clear
cap gen poor_int=.  
* Consumption: Means by Region
collapse (mean) `duri'  mpcer poor_int hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion agri ind services scst transpr hhr communir crockeryr kitchenr recr (rawsum) Nhh ///
(rawsum) hhwt popwt [pw=popwt], by(stat_reg sector) 
label var transporti "Prop Purchased Transport Equipment"
label var recreationi "Prop Purchased  Recreation Equipment"
label var crockeryi "Prop Purchased  Crockery and Utensils"
label var furniturei "Prop Purchased  Furniture and Fixtures"
label var jewelryi "Prop Purchased  Jewelry and Ornaments"
label var appliancesi "Prop Purchased  Household Appliances"
label var kitcheni "Prop Purchased  Kitchen Equipment"
label var iti "Prop Purchased  IT and Communication Devices"
save "$path\data\replication\NSSO`y'\varsmeans_reg`y's.dta", replace

clear
use "$path\data\replication\NSSO`y'\finaldata`y'.dta", clear
* Durables  and individuals in different industries: Sums by Region
collapse (sum) `duri' agrihh=agri indhh=ind serviceshh=services Nhh [pw=popwt], by(stat_reg sector) 
foreach var of local duri{
rename `var' `var'hh
gen prop`var'=`var'hh/Nhh
}
foreach v in agri ind services{
gen prop`v'=`v'hh/Nhh
drop `v'hh
}
save "$path\data\replication\NSSO`y'\varssums_reg`y's.dta", replace
}

*************************************************** Creating Regional Level Datasets ************************************************
foreach y in `all'{
use "$path\data\replication\NSSO`y'\varsmeans_reg`y'.dta", clear
merge 1:1 stat_reg using "$path\data\replication\NSSO`y'\varssums_reg`y'.dta" 
drop _merge
gen round=`y'
save "$path\data\replication\NSSO`y'\final_reg`y'.dta", replace

use "$path\data\replication\NSSO`y'\varsmeans_reg`y's.dta", clear
merge 1:1 stat_reg sector using "$path\data\replication\NSSO`y'\varssums_reg`y's.dta"
drop _merge
gen round=`y'
save "$path\data\replication\NSSO`y'\final_reg`y's.dta", replace
}

*************************************************** Creating Regional Panel Data Sets ************************************************
clear
use "$data61\final_reg61.dta"
append using "$data66\final_reg66.dta"
append using "$data68\final_reg68.dta"
append using "$data72\final_reg72.dta"

sort stat_reg round
save "$path\data\replication\finaldata\panel_reg.dta", replace

* Urban/Rural 
use "$data61\final_reg61s.dta", clear
append using "$data66\final_reg66s.dta"
append using "$data68\final_reg68s.dta"
append using "$data72\final_reg72s.dta"

sort stat_reg round

save "$path\data\replication\finaldata\panel_regs.dta", replace

use "$path\data\replication\finaldata\panel_regs.dta", clear
keep if sector==1
save "$path\data\replication\finaldata\panelrural_reg.dta", replace

use "$path\data\replication\finaldata\panel_regs.dta", clear
keep if sector==2
save "$path\data\replication\finaldata\panelurban_reg.dta", replace
*/ 

************************************************** Creating a HH level Dataset with Regional Variables *********************
local all 61 66 68 72
foreach y in `all'{
use "$path\data\replication\NSSO`y'\finaldata`y'.dta", clear
* This code is to distinguish between means and sums of these variables * 
foreach var in `duri' agri ind services{
rename `var' `var'h
}

foreach v in Dhhsize_cat* Dagecat* agri ind services hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion scst transpr transp1r transp2r recr hhr transprpc recrpc hhrpc communirpc transprx transp2rx recrx hhrx communirx transprln recrln hhrln   lowskill middleskillocc highskillocc{
rename `v' `v'h
}

merge m:1 stat_reg using "$path\data\replication\NSSO`y'\varsmeans_reg`y'.dta"
drop _merge
tostring ID, replace
tostring principind, replace
tostring principocc, replace

* Creating IDs for the SAE analysis

gen qhweight = hhwt*hhsize 
la var qhweight "population weight"

labdtch state
tostring state, replace
replace state= "0" + state if strlen(state)==1

* convert expenditure variables to per capita 
foreach var of varlist transpr hhr communir crockeryr kitchenr recr { 
replace `var'=log(`var'/hhsize) 
label var `var' "log per capita `var'" 
} 



* Generating psus for years other than 2004 in which there is no psu variable. In 2004 the psu is the first 5 digits of the ID. 
gen qhclust=substr(ID,1,5)
replace qhclust=B1_v01 if year==2009

bys qhclust : gen hhcode=_n
tostring hhcode, gen (qhnum)
replace qhnum = "0" + qhnum if strlen(qhnum)==1
drop hhcode

labdtch sector
tostring sector, replace

gen id_sae="6"+sector+state+qhclust+qhnum 
gen id_sae_1="6"+sector+state+qhclust 

replace id_sae="7"+sector+state+qhclust+qhnum if year==2014
replace id_sae_1="7"+sector+state+qhclust if year==2014

la var id_sae "Unique HH ID"
la var id_sae_1 "Unique PSU ID"

destring id_sae id_sae_1 qhclust qhnum , replace

save "$path\data\replication\NSSO`y'\finaldata`y'_agg.dta", replace
}


** apprend together 

use "$data61\finaldata61_agg.dta", clear
append using "$data66\finaldata66_agg.dta"
append using "$data68\finaldata68_agg.dta"
append using "$data72\finaldata72_agg.dta"
replace year=2004.5 if year==2004 
replace year=2009.5 if year==2009
replace year=2011.5 if year==2011
replace year=2014.5 if year==2014

gen t=year-2004.5

** create detailed household size dummies 
recode hhsizeh (1=1 "1") (2=2 "2") (3=3 "3") (4=4 "4") (5=5 "5") (6=6 "6") (7=7 "7") (8/max=8 "8+"), gen (hhsizeh_cat) 
qui tab hhsizeh_cat, gen(Dhhsizeh_cat) 


local x "Dagecat* Dhhsizeh_cat* depratioh selfemployedh hinduh islamh otherreligionh scsth  transpr recr hhr agrih indh servicesh regwageruralh casualruralh regwageurbanh casualurbanh lowskillh middleskillh highskillh"
** create PSU level averages
preserve
collapse (mean) `x', by(psu)
foreach var of varlist `x' { 
	* rename `var' `var'_psu 
	} 
	
save psu_aggregates, replace 
restore 

merge n:1 psu using psu_aggregates
tab _m   
drop _m 


** merge average raindata 
* merge n:1 state using "$path\data\raindata\avg_rain", assert(3)



/*
forvalues i=1/4{
rename rainfall_Q`i' rainfallQ`i'
}
*/ 
label var rainfallQ1 "state mean rainfall Q1"
label var rainfallQ2 "state mean rainfall Q2"
label var rainfallQ3 "state mean rainfall Q3"
label var rainfallQ4 "state mean rainfall Q4"
/** standardize rainfall 
foreach q in 1 2 3 4 { 
qui replace rainfallQ`q'=(rainfallQ`q'-avg_rain_Q`q')/sd_rain_Q`q'
} 
*/ 

* egen rainfallY=rmean(rainfallQ?) 

* create squared rainfall
foreach var of varlist rainfallQ? rainfallY {
gen `var'sq=`var'^2 
} 

local x "Dagecat* Dhhsizeh_cat* depratioh selfemployedh hinduh islamh otherreligionh scsth  transpr recr hhr agrih indh servicesh regwageruralh casualruralh regwageurbanh casualurbanh lowskillh middleskillh highskillh rainfallQ* rainfallY*"

foreach var of varlist `x' {
gen `var't= c.`var'#c.t
} 
egen state_num=group(state) 

save "$data\finaldata\hh_reg.dta", replace
exit 

* Problems with the IDs for the 2009 data
* Checking that the psu variable is the same as the FSU variable
use "$data\finaldata\hh_reg.dta", clear
destring B1_v01, replace
gen check=qhclust - B1_v01
tab year if check~=0 

* Creating the datasets for the SAE analysis
use "$data\replication\finaldata\hh_reg.dta", clear
keep if year==2014.5
save "$data\finaldata\sae14.dta", replace

* Creating urban/rural for the SAE analysis
* Rural * 
use "$data\finaldata\sae14.dta", clear
keep if urban==0
save "$data\finaldata\sae14r.dta", replace

* Urban * 
use "$data\finaldata\sae14.dta", clear
keep if urban==1
save "$data\finaldata\sae14u.dta", replace


* Validation Test: Creating urban/rural for the SAE analysis: Forward Projection
use "$data\finaldata\hh_reg.dta", clear
keep if year==2011.5
save "$data\finaldata\sae11.dta", replace

* Rural * 
use "$data\finaldata\sae11.dta", clear
keep if urban==0
save "$data\finaldata\sae11r.dta", replace

* Urban * 
use "$data\finaldata\sae11.dta", clear
keep if urban==1
save "$data\finaldata\sae11u.dta", replace


* Validation Test: Creating urban/rural for the SAE analysis: Reverse Projection
use "$data\finaldata\hh_reg.dta", clear
keep if year==2004.5
save "$data\finaldata\sae04.dta", replace

* Rural * 
use "$data\finaldata\sae04.dta", clear
keep if urban==0
save "$data\finaldata\sae04r.dta", replace

* Urban * 
use "$data\finaldata\sae04.dta", clear
keep if urban==1
save "$data\finaldata\sae04u.dta", replace




