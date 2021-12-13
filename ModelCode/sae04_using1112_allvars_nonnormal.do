/*****************************************************************************************************
******************************************************************************************************
**                                                                                                  **
**                  IMPUTATION OF CONSUMPTION EXPD IN THE 2014 DATA                                  **
**                                                                                                  **
** COUNTRY			INDIA
** YEARS			2004-05; 2009-10; 2011-12
** SURVEY NAME		NATIONAL SAMPLE SURVEY 
** SURVEY AGENCY	NATIONAL SAMPLE SURVEY OFFICE
** CREATED  BY Pallavi Vyas 
** MODIFIED BY Pallavi Vyas 
** Modified	 11/15/2017
                                                                                             	  	
*****************************************************************************************************
*                                                                                                   *                          
***** --- ELL ESTIMATION: Uses the 2004.5, 2009.5 and 2011.5 surveys to impute into 2014-15 --- *****
*                                                                                                   *
*****************************************************************************************************/

*ATTENTION: DO NOT FORGET TO SAVE TO AN APPROPRIATELY NAMED FILE
cap log close
*cap log using sae18_log_test_matchvars, text replace 
*cap log using sae18_log_test_matchvars_notime, text replace
cap log using sae04_using1112_allvars_nonnormal, text replace

* locals for directories and variables

local datain "$data2\sae04.dta"
local datainu "$data2\sae04u.dta"
local datainr "$data2\sae04r.dta"

local dataout "$analysis\SAE_MATA04_using1112_allvars_nonnormal"
local dataoutu "$analysis\SAE_MATA04u_using1112_allvars_nonnormal"
local dataoutr "$analysis\SAE_MATA04r_using1112_allvars_nonnormal"


local ydump "$analysis\mata04_using1112_allvars_nonnormal"
local ydumpu "$analysis\mata04u_using1112_allvars_nonnormal"
local ydumpr "$analysis\mata04r_using1112_allvars_nonnormal"

glo res "$analysis\SAE_results"


********************************************************************************
************************************ MODEL ************************************
********************************************************************************

* First Stage
use "$path\data\finaldata\hhmaster.dta", clear
drop Dagecat6 Dagecat6h Dhhsizecat6 Dhhsizecat6h
unab xvar011t : $xvar011t 
unab xvar111t : $xvar111t 

gen lnwelf = ln(welfareintr)
*local xvar111t : subinstr local xvar111t "Dhhsizecat4ht" "" 
*local xvar111t : subinstr local xvar111t "Dhhsizecat5ht" "" 
local xvar111t : subinstr local xvar111t "t" ""
local xvar111t : subinstr local xvar111t "Dagecat4ht" "" 
local xvar111t : subinstr local xvar111t "casualurbanht" "" 
local xvar111t : subinstr local xvar111t "regwageurbanht" ""  
local xvar111t : subinstr local xvar111t "hhr" "" 
local xvar111t : subinstr local xvar111t "hhrh" "" 
local xvar111t : subinstr local xvar111t "recr" "" 
local xvar111t : subinstr local xvar111t "recrh" "" 
local xvar111t : subinstr local xvar111t "transpr" "" 
local xvar111t : subinstr local xvar111t "transprh" "" 
local xvar111t : subinstr local xvar111t "hhrt" "" 
local xvar111t : subinstr local xvar111t "hhrht" "" 
local xvar111t : subinstr local xvar111t "recrt" "" 
local xvar111t : subinstr local xvar111t "recrht" "" 
local xvar111t : subinstr local xvar111t "transprt" "" 
local xvar111t : subinstr local xvar111t "transprht" "" 
local xvar111t : subinstr local xvar111t "Dhhsizecat2ht" ""
local xvar111t : subinstr local xvar111t "Dhhsizecat3ht" ""
local xvar111t : subinstr local xvar111t "Dhhsizecat4ht" ""
local xvar111t : subinstr local xvar111t "Dhhsizecat5ht" ""
local xvar111t : subinstr local xvar111t "Dagecat2ht" ""
local xvar111t : subinstr local xvar111t "Dagecat3ht" ""
local xvar111t : subinstr local xvar111t "Dagecat5ht" ""
local xvar111t : subinstr local xvar111t "hinduht" ""
local xvar111t : subinstr local xvar111t "scstht" ""
local xvar111t : subinstr local xvar111t "casualurbant" ""
local xvar111t : subinstr local xvar111t "middleskilloccht" ""
local xvar111t : subinstr local xvar111t "middleskillocct" ""
local xvar111t : subinstr local xvar111t "highskilloccht" ""
local xvar111t : subinstr local xvar111t "agriht" ""
local xvar111t : subinstr local xvar111t "indht" ""
local xvar111t : subinstr local xvar111t "selfemployedht" ""
local xvar111t : subinstr local xvar111t "Dhhsizeca2" ""
local xvar111t : subinstr local xvar111t "Dhhsizeca3" ""  
local xvar111t : subinstr local xvar111t "regwageurbant" ""
local xvar111t : subinstr local xvar111t "regwageurbanht" ""

local xvar011t : subinstr local xvar011t "t" ""
local xvar011t : subinstr local xvar011t "Dagecat4ht" "" 
local xvar011t : subinstr local xvar011t "casualurbanht" "" 
local xvar011t : subinstr local xvar011t "regwageurbanht" ""  
local xvar011t : subinstr local xvar011t "hhr" "" 
local xvar011t : subinstr local xvar011t "hhrh" "" 
local xvar011t : subinstr local xvar011t "recr" "" 
local xvar011t : subinstr local xvar011t "recrh" "" 
local xvar011t : subinstr local xvar011t "transpr" "" 
local xvar011t : subinstr local xvar011t "transprh" "" 
local xvar011t : subinstr local xvar011t "hhrt" "" 
local xvar011t : subinstr local xvar011t "hhrht" "" 
local xvar011t : subinstr local xvar011t "recrt" "" 
local xvar011t : subinstr local xvar011t "recrht" "" 
local xvar011t : subinstr local xvar011t "transprt" "" 
local xvar011t : subinstr local xvar011t "transprht" ""
local xvar011t : subinstr local xvar011t "Dhhsizecat2ht" ""
local xvar011t : subinstr local xvar011t "Dhhsizecat3ht" ""
local xvar011t : subinstr local xvar011t "Dhhsizecat4ht" ""
local xvar011t : subinstr local xvar011t "Dhhsizecat5ht" ""
local xvar011t : subinstr local xvar011t "Dagecat2ht" ""
local xvar011t : subinstr local xvar011t "Dagecat3ht" ""
local xvar011t : subinstr local xvar011t "Dagecat5ht" ""
local xvar011t : subinstr local xvar011t "hinduht" ""
local xvar011t : subinstr local xvar011t "scstht" ""
local xvar011t : subinstr local xvar011t "casualurbant" ""
local xvar011t : subinstr local xvar011t "middleskilloccht" ""
local xvar011t : subinstr local xvar011t "middleskillocct" ""
local xvar011t : subinstr local xvar011t "highskilloccht" ""
local xvar011t : subinstr local xvar011t "agriht" ""
local xvar011t : subinstr local xvar011t "indht" ""
local xvar011t : subinstr local xvar011t "selfemployedht" ""
local xvar011t : subinstr local xvar011t "Dhhsizeca2" "" 
local xvar011t : subinstr local xvar011t "Dhhsizeca3" ""
local xvar011t : subinstr local xvar011t "regwageurbanht" ""
local xvar011t : subinstr local xvar011t "regwageurbant" ""

*add more variables to the model

local xvar111t `xvar111t' Dmarriedh Dmarried 
local xvar111t `xvar111t' Dmaleh Dmale Dfirewoodh Dfirewoodh 
local xvar111t `xvar111t' Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh
local xvar111t `xvar111t' Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh  

local xvar011t `xvar011t' Dmarriedh Dmarried 
local xvar011t `xvar011t' Dmaleh Dmale Dfirewoodh Dfirewoodh 
local xvar011t `xvar011t' Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh
local xvar011t `xvar011t' Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh 


gen intyear=year-0.5 

keep if year == 2011.5

save "$data2\sae12.dta", replace

* Urban
use "$data2\sae12.dta", clear
keep if urban==1

regress lnwelf `xvar111t' [aw=pop_wgt], vce(robust) 

sae model povmap lnwelf `xvar111t' [pw=pop_wgt], area(district_code) varest(ell) vce(ell) alfatest(robust) zvar(`xvar111t')
estimates table 

estimates store indu
estimates save "$estimates/stage1_04_indu_using1112_allvars_nonnormal", replace 


*qui: lassoregress robust_alfa `xvar111t' [aw=pop_wgt]

*local error_lasso_modelu "`e(varlist_nonzero)'"

* Rural
use "$data2\sae12.dta", clear
keep if urban==0
sae model povmap lnwelf `xvar011t' [pw=pop_wgt], area(district_code) varest(ell) vce(ell) alfatest(robust) zvar(`xvar011t')
estimates store indr

estimates save "$estimates/stage1_04_indr_using1112_allvars_nonnormal", replace 

*qui: lassoregress robust_alfa `xvar011t' [aw=pop_wgt]

*local error_lasso_modelr "`e(varlist_nonzero)'"


************************************************* Second Stage - Urban **********************************************
sae data import, varlist(id_sae qhweight qhclust `xvar111t' district_code) uniqid(id_sae) area(district_code) ///
datain(`datainu') dataout(`dataoutu')

use "$data2\sae12.dta", clear
keep if urban==1

/* assume normality 
sae sim povmap lnwelf `xvar111t' [pw=pop_wgt], area(district_code) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(5) seed(57678) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutu') indicators(fgt0) allmata zvar(`error_lasso_modelu') ///
pline(57.791668) addvars(id_sae qhweight qhclust `xvar111t' district_code) 
*/ 



global xvar111t "`xvar111t'" 
global error_lasso_modelu "`error_lasso_modelu'"

* non-paramertric
sae sim povmap lnwelf `xvar111t' [pw=pop_wgt], area(district_code) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(57678) lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutu') indicators(fgt0) /*allmata zvar(`error_lasso_modelu')*/ ///
pline(57.791668) addvars(id_sae qhclust `xvar111t' district_code) 
*/ 

* Export data
sae data export, matasource(`ydumpu') numfiles(1) datasave($analysis\final_data04u_using1112_allvars_nonnormal)

* Poverty numbers
sae proc indicator, matasource(`ydumpu') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impu04_using1112_allvars_nonnormal.dta", replace

************************************************* Second Stage - Rural **********************************************
sae data import, varlist(id_sae qhweight qhclust `xvar011t' district_code) uniqid(id_sae) area(district_code) ///
datain(`datainr') dataout(`dataoutr')

use "$data2\sae12.dta", clear
keep if urban==0
/* assume normality  
sae sim povmap lnwelf `xvar011t' [pw=pop_wgt], area(district_code) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(5) seed(16757) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutr') indicators(fgt0) allmata zvar(`error_lasso_modelr') ///
pline(57.791668) addvars(id_sae qhweight qhclust `xvar011t' district_code) 
*/ 

* non-paramertic estimation
sae sim povmap lnwelf `xvar011t' [pw=pop_wgt], area(district_code) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutr') indicators(fgt0) /*allmata zvar(`error_lasso_modelr')*/ ///
pline(57.791668) addvars(id_sae qhclust `xvar011t' district_code)  
*/ 

* Export data
sae data export, matasource(`ydumpr') numfiles(1) datasave($analysis\final_data04r_using1112_allvars_nonnormal)

* Poverty numbers
sae proc indicator, matasource(`ydumpr') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impr04_using1112_allvars_nonnormal.dta", replace

*************************************************************************************************************************
**** Predicted poverty

	* Importing dataset (National)
	use "$analysis\final_data04u_using1112_allvars_nonnormal.dta", clear
	gen urban=1
	append using "$analysis\final_data04r_using1112_allvars_nonnormal.dta"
	replace urban=0 if urban==.
	save "$analysis\final_data04ur_using1112_allvars_nonnormal.dta", replace
	
	gen welfare=. 

	* Importing dataset into MI format
	mi import wide, imputed(welfare=_YHAT*) clear
	drop _YHAT* 

	save "$analysis\mi_data04_using1112_allvars_nonnormal", replace 
	
	
	
use "$data\finaldata\hh_reg.dta", clear	
summ welfareintr [aw=pop_wgt] if urban==0 
global max_welfarer=r(max) 
summ welfareintr [aw=pop_wgt] if urban==1 
global max_welfareu=r(max) 
	
	*use "$analysis\mi_data04_using1112_allvars_nonnormal", clear
	
	
	use "$analysis\mi_data04_using1112_allvars_nonnormal", clear
	foreach pair in "190 1.90" "320 3.20" "550 5.50"  {
	local suffix : word 1 of `pair'
	local line : word 2 of `pair'
	dis "`suffix'" 
	qui mi passive: gen Dpoor`suffix'=(welfare<57.791668*`line'/1.9)
	qui mi passive: gen povgap`suffix'=max(1-welfare/(57.791668*`line'/1.9),0) 
	qui mi passive: gen povseverity`suffix'=max(1-welfare/(57.791668*`line'/1.9),0)^2 
	}

	/*
	preserve 
	egen Dpoor_190=rowmean(_*_Dpoor190) 
	collapse (mean) Dpoor_190 [pw=_WEIGHT], by(state) 
	save "$data/state_povrates_190_2014", replace 
	restore 
	*/ 
	mi describe
	mi svyset qhclust [pw= _WEIGHT]
	
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "$pathout\estimates\sae04_sectorpov_using1112_allvars_nonnormal", replace 
	
	mi estimate: svy: mean Dpoor* povgap* povseverity*
	estimates save "$pathout\estimates\sae04_overallpov_using1112_allvars_nonnormal", replace
	
	log close 
	exit 
	
	** replicate subsample of compete distrits for comparison with 
	preserve 
	*keep if Nyearsdistrict==4 
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "$pathout\estimates\sae18_sectorpov_using1112_allvars_nonnormal", replace 
	
	restore 
	
	log close 
	
	exit 
	
	
	
	
	preserve
	collapse (mean) Dpoor190, by(state) 
	
	restore 
	
	
	mat est_poverty=r(table)
	
	
	mat p14=J(2,3,.)
	mat p14[1,1]=est_poverty[1,2]*100 // % poverty headcount
	mat p14[1,2]=est_poverty[5,2]*100 // % poverty headcount
	mat p14[1,3]=est_poverty[6,2]*100 // % poverty headcount
	
	mat p14[2,1]=est_poverty[1,1]*100
	mat p14[2,2]=est_poverty[5,1]*100
	mat p14[2,3]=est_poverty[6,1]*100

	mat p14_320=J(2,3,.) 
	mat p14_320[1,1]=est_poverty[1,4]*100 // % poverty headcount
	mat p14_320[1,2]=est_poverty[5,4]*100 // % poverty headcount
	mat p14_320[1,3]=est_poverty[6,4]*100 // % poverty headcount
	
	mat p14_320[2,1]=est_poverty[1,3]*100
	mat p14_320[2,2]=est_poverty[5,3]*100
	mat p14_320[2,3]=est_poverty[6,3]*100
	
	mat p14_550=J(2,3,.) 
	mat p14_550[1,1]=est_poverty[1,6]*100 // % poverty headcount
	mat p14_550[1,2]=est_poverty[5,6]*100 // % poverty headcount
	mat p14_550[1,3]=est_poverty[6,6]*100 // % poverty headcount
	
	mat p14_550[2,1]=est_poverty[1,5]*100
	mat p14_550[2,2]=est_poverty[5,5]*100
	mat p14_550[2,3]=est_poverty[6,5]*100
	
	
	
	
svmat p14, names(poor14)
svmat p14_320, names(poor14_320) 
svmat p14_550, names(poor14_550) 		

keep poor14*
drop if poor141==. 
save "$analysis\pov14sae.dta", replace

use  "$data\finaldata\hh_reg.dta", clear
	preserve 
	keep if year==2011.5
	rename poor_int Dpoor190 
	collapse (mean) Dpoor190 [pw=pop_wgt], by(state)
	save "$data/state_povrates_190_2011", replace 
	restore 
	preserve 
	keep if year==2004.5
	rename poor_int Dpoor190 
	collapse (mean) Dpoor190 [pw=pop_wgt], by(state)
	save "$data/state_povrates_190_2004", replace 	
	restore 
	
	
	
	svyset qhclust [pw=pop_wgt]
	svy: mean poor_int, over (urban)
	mat r14 = r(table)'
	mat list r14
	
	forvalues y=1/2{
	foreach i in 1 5 6{
	mat r14`y'`i'=r14[`y',`i']*100
	}
	mat z`y'= r14`y'1, r14`y'5, r14`y'6
	}
	
	mat z14 = z2\ z1
	
	mat list z14
	mat k14 = p14, z14

	
	
mat rownames k14= "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (k14, fmt(%9.1fc)) using "$tables/sae14.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2009 and 2011 Surveys to project into 2014." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2014.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type

** 3.20 line 	
	gen poor_320=welfare*(12/365)/(cpi*ppp)<3.20
	svy: mean poor_320, over(urban) 
	mat r14_320=r(table)' 
	matrix z14_320=r14_320[1..2,1],r14_320[1..2,5..6] 
	matrix z14_320=z14_320*100 
	matrix k14_320=p14_320, z14_320
	mat rownames k14_320= "\textbf{Urban}" "\textbf{Rural}" 
esttab matrix (k14_320, fmt(%9.1fc)) using "$tables/sae14_320.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2009 and 2011 Surveys to project into 2014." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2014.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type

** 5.50 line 	
	gen poor_550=welfare*(12/365)/(cpi*ppp)<5.50
	svy: mean poor_550, over(urban) 
	mat r14_550=r(table)' 
	matrix z14_550=r14_550[1..2,1],r14_550[1..2,5..6] 
	matrix z14_550=z14_550*100 
	matrix k14_550=p14_550, z14_550
	mat rownames k14_550= "\textbf{Urban}" "\textbf{Rural}" 
esttab matrix (k14_550, fmt(%9.1fc)) using "$tables/sae14_550.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2009 and 2011 Surveys to project into 2014." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2014.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type


	
	

	
*Creating a Time Series Table of poverty Rates
use  "$data\finaldata\hh_reg.dta", clear
gen poor_320=welfare*(12/365)/(cpi*ppp)<3.20
gen poor_550=welfare*(12/365)/(cpi*ppp)<5.50


mean poor_int poor_320 poor_550 [pw=pop_wgt], over (urban year)
mat x=r(table)
matrix n=x[1,1..3]*(1-.316309)+x[1,4..6]*(.316309)
matrix n=n\x[1,4..6]
matrix n=n\x[1,1..3]
matrix n=n*100 

matrix n2=(p14[1,1]*(.316309)+p14[2,1]*(1-.316309))\p14[1..2,1]
matrix  n=n,n2
matrix list n 
matrix drop n2 



** 3.20 line 
matrix n_320=x[1,7..9]*(1-.316309)+x[1,10..12]*(.316309)
matrix n_320=n_320\x[1,10..12]
matrix n_320=n_320\x[1,7..9]
matrix n_320=n_320*100 

matrix n2=(p14_320[1,1]*(.316309)+p14_320[2,1]*(1-.316309))\p14_320[1..2,1]
matrix  n_320=n_320,n2
matrix list n_320 
matrix drop n2 


** 5.50 line 
matrix n_550=x[1,13..15]*(1-.316309)+x[1,14..16]*(.316309)
matrix n_550=n_550\x[1,14..16]
matrix n_550=n_550\x[1,13..15]
matrix n_550=n_550*100 

matrix n2=(p14_550[1,1]*(.316309)+p14_550[2,1]*(1-.316309))\p14_550[1..2,1]
matrix  n_550=n_550,n2
matrix list n_550 
matrix drop n2 


/* OLD CODE UNUSED Using 2004
use  "$data\finaldata\hh_reg.dta", clear
keep if year==2004.5



mean poor_int [pw=pop_wgt], over (urban)
mat x04=r(table)'
mat list x04

use  "$data\finaldata\hh_reg.dta", clear
keep if year==2009.5
mean poor_int [pw=pop_wgt], over (urban)
mat x09=r(table)'
mat list x09


* Creating a Time Series Table of poverty Rates
mat n = J(3,4,.)

mat n[2,1]=(x04[2,1])*100
mat n[3,1]=x04[1,1]*100
mat n[1,1] = (x04[2,1]*.316309 + x04[1,1]*(1-.316309))*100	

mat n[2,2]=(x09[2,1])*100
mat n[3,2]=(x09[1,1])*100
mat n[1,2]=(x09[2,1]*.316309 + x09[1,1]*(1-.316309))*100	

mat n[2,3]=(k14[1,4])
mat n[3,3]=(k14[2,4])
mat n[1,3]=(k14[1,4]*.316309 + k14[2,4]*(1-.316309))

mat n[2,4]=(k14[1,1])
mat n[3,4]=(k14[2,1])
mat n[1,4]=(k14[1,1]*.316309 + k14[2,1]*(1-.316309))
*/ 




mat rownames n= "\textbf{National}" "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (n_320, fmt(%9.1fc)) using "$tables/povrates_320.tex", /// 
nonumbers nomtitles nodepvars align(rrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys.") ///
posthead(& \multicolumn{1}{c}{\textbf{2004.5}} & {\textbf{2009.5}} & {\textbf{2011.5}} & {\color{red}\textbf{2014.5}}\\ "\hline") replace

mat rownames n= "\textbf{National}" "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (n_550, fmt(%9.1fc)) using "$tables/povrates_550.tex", /// 
nonumbers nomtitles nodepvars align(rrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys.") ///
posthead(& \multicolumn{1}{c}{\textbf{2004.5}} & {\textbf{2009.5}} & {\textbf{2011.5}} & {\color{red}\textbf{2014.5}}\\ "\hline") replace




svmat n, names(pov_190)
svmat n_320, names(pov_320)
svmat n_550, names(pov_550) 
keep pov_190* pov_320* pov_550* 
drop if pov_1901==.

gen sector="National" in 1 
replace sector="Urban" in 2
replace sector="Rural" in 3 


reshape long pov_190 pov_320 pov_550, i(sector) j(year) 
reshape wide pov_190 pov_320 pov_550, i(year) j(sector) string

replace year=2004.5 if year==1
replace year=2009.5 if year==2
replace year=2011.5 if year==3
replace year=2014.5 if year==4

save "$analysis\povtable.dta", replace

use "$analysis\povtable.dta" , clear
format pov_*National %3.1f 
gen labpos=6 
replace labpos=8 if year==2014.5 
replace labpos=5 if year==2004.5 



twoway  (line pov_320National year, lpattern(solid) lcolor(black) lwidth(thick)) (line pov_320Urban year, lpattern(dash) lcolor(purple) lwidth(thick)) (line pov_320Rural year, lpattern(longdash) lcolor(forest_green) lwidth(thick)) ///
		(scatter pov_320National year, mlabel(pov_320National) mlabv(labpos) mlabcolor(black) lpattern(solid) mcolor(black) lwidth(thick)), xtitle("Year")  legend(label(1 "National") label(2 "Urban")  ///
		label(3 "Rural") label(4 "Year of Survey")) xmticks(##2) xlabel(2004.5 (5) 2014.5) yscale(r(35 85))  ylabel(30(10)90, format(%2.0f)) ytitle(Percent) graphregion(color(white)) ///
		note("Sources: India National Sample Survey (NSSO) Surveys.")
graph export "$graphs\pov_320.pdf", replace	


twoway  (line pov_550National year, lpattern(solid) lcolor(black) lwidth(thick)) (line pov_550Urban year, lpattern(dash) lcolor(purple) lwidth(thick)) (line pov_550Rural year, lpattern(longdash) lcolor(forest_green) lwidth(thick)) ///
		(scatter pov_550National year, mlabel(pov_550National) mlabv(labpos) mlabcolor(black) lpattern(solid) mcolor(black) lwidth(thick)), xtitle("Year")  legend(label(1 "National") label(2 "Urban")  ///
		label(3 "Rural") label(4 "Year of Survey")) xmticks(##2) xlabel(2004.5 (5) 2014.5) yscale(r(70 100)) ylabel(70(5)100, format(%2.0f)) ytitle(Percent) graphregion(color(white)) ///
		note("Sources: India National Sample Survey (NSSO) Surveys.")
graph export "$graphs\pov_550.pdf", replace	
		
		



log close 
