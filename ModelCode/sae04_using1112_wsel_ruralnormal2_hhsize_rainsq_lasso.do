/*****************************************************************************************************
******************************************************************************************************
**                                                                                                  **
**                  IMPUTATION OF CONSUMPTION EXPD IN THE 2004 DATA                                  **
**                                                                                                  **
** COUNTRY			INDIA
** YEARS			2004-05; 2004-10; 2011-12
** SURVEY NAME		NATIONAL SAMPLE SURVEY 
** SURVEY AGENCY	NATIONAL SAMPLE SURVEY OFFICE
** CREATED  BY Pallavi Vyas 
** MODIFIED BY Pallavi Vyas 
** Modified	 11/15/2017
                                                                                             	  	
*****************************************************************************************************
*                                                                                                   *                          
***** --- ELL ESTIMATION: Uses the 2011.5 surveys to impute into 2004 --- *****
*                                                                                                   *
*****************************************************************************************************/

*ATTENTION: DO NOT FORGET TO SAVE TO AN APPROPRIATELY NAMED FILE
cap log close
*cap log using sae04_log_test_matchvars, text replace 
*cap log using sae04_log_test_matchvars_notime, text replace
*cap log using sae04_log_test_newvars, text replace
*cap log using sae04_log_test_newvars_withselection, text replace
cap log using sae04_log_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso, text replace


* locals for directories and variables

local datain "$data2\sae04.dta"
local datainu "$data2\sae04u.dta"
local datainr "$data2\sae04r.dta"

local dataout "$analysis\SAE_MATA04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"
local dataoutu "$analysis\SAE_MATA04u_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"
local dataoutr "$analysis\SAE_MATA04r_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"


local ydump "$analysis\mata04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"
local ydumpu "$analysis\mata04u_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"
local ydumpr "$analysis\mata04r_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso"

glo res "$analysis\SAE_results"


********************************************************************************
************************************ MODEL ************************************
********************************************************************************

* First Stage
use "$path\data\finaldata\newmaster.dta", clear

gen lnwelf = ln(welfareintr)

global ruralvars "Dagecat1h Dagecat2 Dagecat2h Dagecat3 Dagecat4h Dagecat5 Dagecat5h" 
global ruralvars "${ruralvars} hhsizeh hhsize highskillocc highskillocch" 
global ruralvars "${ruralvars} middleskillocc middleskillocch ind indh scst scsth selfemployed selfemployedh" 
global ruralvars "${ruralvars} depratio depratioh hhage"
global ruralvars "${ruralvars} Dmarriedh Dmarried" 
global ruralvars "${ruralvars} Dmaleh Dmale" 
global ruralvars "${ruralvars} Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh"
global ruralvars "${ruralvars} Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh"  
global ruralvars "${ruralvars} rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"
global ruralvars "${ruralvars} rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq"

global urbanvars "Dagecat1h Dagecat2 Dagecat2h Dagecat3 Dagecat4h Dagecat5 Dagecat5h" 
global urbanvars "${urbanvars} hhsizeh hhsize highskillocc highskillocch" 
global urbanvars "${urbanvars} middleskillocc middleskillocch ind indh scst scsth selfemployed selfemployedh" 
global urbanvars "${urbanvars} regwageurban regwageurbanh depratio depratioh"
global urbanvars "${urbanvars} Dmarriedh Dmarried" 
global urbanvars "${urbanvars} Dmaleh Dmale hhage" 
global urbanvars "${urbanvars} Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh"
global urbanvars "${urbanvars} Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh"  
global urbanvars "${urbanvars} rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"
global urbanvars "${urbanvars} rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq"

gen intyear=year-0.5 

keep if year == 2011.5

save "$data2\sae12.dta", replace


/*
**** the next step is to do a model with selection using a stepwise process
*stepwise, pr(0.05): reg lnwelf $urbanvars [pw = wgt] if urban == 1

local temp : colfullnames e(b) 
local b _cons
local xvars : list temp - b
glo urbanvars2 `xvars'

stepwise, pr(0.05): reg lnwelf $ruralvars [pw = wgt] if urban == 0
local temp : colfullnames e(b) 
local b _cons
local xvars : list temp - b
glo ruralvars2 `xvars'
*/

*** using a lasso regression to do variable selection
qui: lassoregress lnwelf $urbanvars [aw=pop_wgt] if urban == 1
glo urbanvars2 "`e(varlist_nonzero)'"

qui: lassoregress lnwelf $ruralvars [aw=pop_wgt] if urban == 0
glo ruralvars2 "`e(varlist_nonzero)'"



* Urban
use "$data2\sae12.dta", clear
keep if urban==1


regress lnwelf $urbanvars2 [aw=pop_wgt], vce(robust) 



sae model povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(h3) alfatest(_res) zvar($urbanvars2)
estimates table 

estimates store indu
estimates save "$estimates/stage1u_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", replace 


*qui: lassoregress robust_alfa $urbanvars2 [aw=pop_wgt]

*local error_lasso_modelu "`e(varlist_nonzero)'"

* Rural
use "$data2\sae12.dta", clear
keep if urban==0
sae model povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(h3) alfatest(_res) zvar($ruralvars2)
estimates store indr

estimates save "$estimates/stage1r_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", replace 

*qui: lassoregress robust_alfa $ruralvars2 [aw=pop_wgt]

*local error_lasso_modelr "`e(varlist_nonzero)'"


 

************************************************* Second Stage - Urban **********************************************
sae data import, varlist(id_sae qhweight qhclust $urbanvars2 district_code) uniqid(id_sae) area(district_code) ///
datain(`datainu') dataout(`dataoutu')

use "$data2\sae12.dta", clear
keep if urban==1

/* assume normality 
sae sim povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(5) seed(57678) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutu') indicators(fgt0) allmata zvar(`error_lasso_modelu') ///
pline(57.791668) addvars(id_sae qhweight qhclust $urbanvars2 district_code) 
*/ 

global urbanvars "$urbanvars" 
global error_lasso_modelu "`error_lasso_modelu'"

* non-paramertric
sae sim povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(h3) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(57678) lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutu') indicators(fgt0) /*allmata zvar(`error_lasso_modelu')*/ ///
pline(57.791668) addvars(id_sae qhweight qhclust $urbanvars2) 
*/ 

* Export data
sae data export, matasource(`ydumpu') numfiles(1) datasave($analysis\final_datau_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso)

* Poverty numbers
sae proc indicator, matasource(`ydumpu') aggids(7 8) indicators(fgt0 gini) plines(57.791668) 

save "$res\impu_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso.dta", replace

************************************************* Second Stage - Rural **********************************************
sae data import, varlist(id_sae qhweight qhclust $ruralvars2 district_code) uniqid(id_sae) area(district_code) ///
datain(`datainr') dataout(`dataoutr')

use "$data2\sae12.dta", clear
keep if urban==0
* assume nonnormality  
sae sim povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(h3) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutr') indicators(fgt0 gini) allmata zvar(`error_lasso_modelr') ///
pline(57.791668) addvars(id_sae qhweight qhclust $ruralvars2 district_code) 
*/ 

/* non-parametric estimation
sae sim povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(h3) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutr') indicators(fgt0) /*allmata zvar(`error_lasso_modelr')*/ ///
pline(57.791668) addvars(id_sae qhclust $ruralvars2 district_code)  
*/ 

* Export data
sae data export, matasource(`ydumpr') numfiles(1) datasave($analysis\final_datar_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso)

* Poverty numbers
sae proc indicator, matasource(`ydumpr') aggids(7 8) indicators(fgt0 gini) plines(57.791668) 

save "$res\impr_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso.dta", replace

*************************************************************************************************************************
**** Predicted poverty

	* Importing dataset (National)
	use "$analysis\final_datau_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso.dta", clear
	gen urban=1
	append using "$analysis\final_datar_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso.dta"
	replace urban=0 if urban==.
	save "$analysis\final_dataur_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso.dta", replace
	
	gen welfare=. 

	* Importing dataset into MI format
	mi import wide, imputed(welfare=_YHAT*) clear
	drop _YHAT* 

	save "$analysis\mi_data_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", replace 
	
		
use "$path\data\finaldata\newmaster.dta", clear	
summ welfareintr [aw=pop_wgt] if urban==0 
global max_welfarer=r(max) 
summ welfareintr [aw=pop_wgt] if urban==1 
global max_welfareu=r(max) 
	
	use "$analysis\mi_data_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", clear
	
	
	
	 
	
	
	


	use "$analysis\mi_data_04_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", clear
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
	save "$data/state_povrates_190_2004", replace 
	restore 
	*/ 
	mi describe
	mi svyset qhclust [pw= _WEIGHT]
	
	

	
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "$pathout\estimates\sae04_sectorpov_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", replace 
	
	**estimates for the overall national
	mi estimate: svy: mean Dpoor* povgap* povseverity*
	estimates save "$pathout\estimates\sae04_overallpov_using1112_wsel_ruralnormal2_hhsize_rainsq_lasso", replace
	
	log close
	exit
	
	
	** replicate subsample of compete distrits for comparison with 
	preserve 
	*keep if Nyearsdistrict==4 
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "D:\Poverty_Estimates_India\estimates\sae04_completedistrictsubsample_newvars_withselection_nonnormal", replace 
	restore 
	
	log close 
	
	exit 
	
	
	
	
	preserve
	collapse (mean) Dpoor190, by(state) 
	
	restore 
	
	
	mat est_poverty=r(table)
	
	
	mat p04=J(2,3,.)
	mat p04[1,1]=est_poverty[1,2]*100 // % poverty headcount
	mat p04[1,2]=est_poverty[5,2]*100 // % poverty headcount
	mat p04[1,3]=est_poverty[6,2]*100 // % poverty headcount
	
	mat p04[2,1]=est_poverty[1,1]*100
	mat p04[2,2]=est_poverty[5,1]*100
	mat p04[2,3]=est_poverty[6,1]*100

	mat p04_320=J(2,3,.) 
	mat p04_320[1,1]=est_poverty[1,4]*100 // % poverty headcount
	mat p04_320[1,2]=est_poverty[5,4]*100 // % poverty headcount
	mat p04_320[1,3]=est_poverty[6,4]*100 // % poverty headcount
	
	mat p04_320[2,1]=est_poverty[1,3]*100
	mat p04_320[2,2]=est_poverty[5,3]*100
	mat p04_320[2,3]=est_poverty[6,3]*100
	
	mat p04_550=J(2,3,.) 
	mat p04_550[1,1]=est_poverty[1,6]*100 // % poverty headcount
	mat p04_550[1,2]=est_poverty[5,6]*100 // % poverty headcount
	mat p04_550[1,3]=est_poverty[6,6]*100 // % poverty headcount
	
	mat p04_550[2,1]=est_poverty[1,5]*100
	mat p04_550[2,2]=est_poverty[5,5]*100
	mat p04_550[2,3]=est_poverty[6,5]*100
	
	
	
	
svmat p04, names(poor04)
svmat p04_320, names(poor04_320) 
svmat p04_550, names(poor04_550) 		

keep poor04*
drop if poor041==. 
save "$analysis\pov04sae.dta", replace

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
	mat r04 = r(table)'
	mat list r04
	
	forvalues y=1/2{
	foreach i in 1 5 6{
	mat r04`y'`i'=r04[`y',`i']*100
	}
	mat z`y'= r04`y'1, r04`y'5, r04`y'6
	}
	
	mat z04 = z2\ z1
	
	mat list z04
	mat k04 = p04, z04

	
	
mat rownames k04= "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (k04, fmt(%9.1fc)) using "$tables/sae04.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2004 and 2011 Surveys to project into 2004." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2004.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type

** 3.20 line 	
	gen poor_320=welfare*(12/365)/(cpi*ppp)<3.20
	svy: mean poor_320, over(urban) 
	mat r04_320=r(table)' 
	matrix z04_320=r04_320[1..2,1],r04_320[1..2,5..6] 
	matrix z04_320=z04_320*100 
	matrix k04_320=p04_320, z04_320
	mat rownames k04_320= "\textbf{Urban}" "\textbf{Rural}" 
esttab matrix (k04_320, fmt(%9.1fc)) using "$tables/sae04_320.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2004 and 2011 Surveys to project into 2004." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2004.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type

** 5.50 line 	
	gen poor_550=welfare*(12/365)/(cpi*ppp)<5.50
	svy: mean poor_550, over(urban) 
	mat r04_550=r(table)' 
	matrix z04_550=r04_550[1..2,1],r04_550[1..2,5..6] 
	matrix z04_550=z04_550*100 
	matrix k04_550=p04_550, z04_550
	mat rownames k04_550= "\textbf{Urban}" "\textbf{Rural}" 
esttab matrix (k04_550, fmt(%9.1fc)) using "$tables/sae04_550.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys." "Using 2004, 2004 and 2011 Surveys to project into 2004." "a. Simulated" "b. Actual") ///
posthead(& \multicolumn{1}{c}{\color{red}\textbf{2004.5$^a$}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & \multicolumn{1}{c}{\textbf{2011.5$^b$}} & \multicolumn{2}{c}{\textbf{95\% C.I}}\\ "\hline") replace type


	
	

	
*Creating a Time Series Table of poverty Rates
use  "$data\finaldata\hh_reg.dta", clear
gen poor_320=welfare*(12/365)/(cpi*ppp)<3.20
gen poor_550=welfare*(12/365)/(cpi*ppp)<5.50


mean poor_int poor_320 poor_550 [pw=pop_wgt], over (urban year)
mat x=r(table)
matrix n=x[1,1..3]*(1-.316304)+x[1,4..6]*(.316304)
matrix n=n\x[1,4..6]
matrix n=n\x[1,1..3]
matrix n=n*100 

matrix n2=(p04[1,1]*(.316304)+p04[2,1]*(1-.316304))\p04[1..2,1]
matrix  n=n,n2
matrix list n 
matrix drop n2 



** 3.20 line 
matrix n_320=x[1,7..9]*(1-.316304)+x[1,10..12]*(.316304)
matrix n_320=n_320\x[1,10..12]
matrix n_320=n_320\x[1,7..9]
matrix n_320=n_320*100 

matrix n2=(p04_320[1,1]*(.316304)+p04_320[2,1]*(1-.316304))\p04_320[1..2,1]
matrix  n_320=n_320,n2
matrix list n_320 
matrix drop n2 


** 5.50 line 
matrix n_550=x[1,13..15]*(1-.316304)+x[1,04..16]*(.316304)
matrix n_550=n_550\x[1,04..16]
matrix n_550=n_550\x[1,13..15]
matrix n_550=n_550*100 

matrix n2=(p04_550[1,1]*(.316304)+p04_550[2,1]*(1-.316304))\p04_550[1..2,1]
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
keep if year==2004.5
mean poor_int [pw=pop_wgt], over (urban)
mat x04=r(table)'
mat list x04


* Creating a Time Series Table of poverty Rates
mat n = J(3,4,.)

mat n[2,1]=(x04[2,1])*100
mat n[3,1]=x04[1,1]*100
mat n[1,1] = (x04[2,1]*.316304 + x04[1,1]*(1-.316304))*100	

mat n[2,2]=(x04[2,1])*100
mat n[3,2]=(x04[1,1])*100
mat n[1,2]=(x04[2,1]*.316304 + x04[1,1]*(1-.316304))*100	

mat n[2,3]=(k04[1,4])
mat n[3,3]=(k04[2,4])
mat n[1,3]=(k04[1,4]*.316304 + k04[2,4]*(1-.316304))

mat n[2,4]=(k04[1,1])
mat n[3,4]=(k04[2,1])
mat n[1,4]=(k04[1,1]*.316304 + k04[2,1]*(1-.316304))
*/ 




mat rownames n= "\textbf{National}" "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (n_320, fmt(%9.1fc)) using "$tables/povrates_320.tex", /// 
nonumbers nomtitles nodepvars align(rrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys.") ///
posthead(& \multicolumn{1}{c}{\textbf{2004.5}} & {\textbf{2004.5}} & {\textbf{2011.5}} & {\color{red}\textbf{2004.5}}\\ "\hline") replace

mat rownames n= "\textbf{National}" "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (n_550, fmt(%9.1fc)) using "$tables/povrates_550.tex", /// 
nonumbers nomtitles nodepvars align(rrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys.") ///
posthead(& \multicolumn{1}{c}{\textbf{2004.5}} & {\textbf{2004.5}} & {\textbf{2011.5}} & {\color{red}\textbf{2004.5}}\\ "\hline") replace




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
replace year=2004.5 if year==2
replace year=2011.5 if year==3
replace year=2004.5 if year==4

save "$analysis\povtable.dta", replace

use "$analysis\povtable.dta" , clear
format pov_*National %3.1f 
gen labpos=6 
replace labpos=8 if year==2004.5 
replace labpos=5 if year==2004.5 



twoway  (line pov_320National year, lpattern(solid) lcolor(black) lwidth(thick)) (line pov_320Urban year, lpattern(dash) lcolor(purple) lwidth(thick)) (line pov_320Rural year, lpattern(longdash) lcolor(forest_green) lwidth(thick)) ///
		(scatter pov_320National year, mlabel(pov_320National) mlabv(labpos) mlabcolor(black) lpattern(solid) mcolor(black) lwidth(thick)), xtitle("Year")  legend(label(1 "National") label(2 "Urban")  ///
		label(3 "Rural") label(4 "Year of Survey")) xmticks(##2) xlabel(2004.5 (5) 2004.5) yscale(r(35 85))  ylabel(30(10)90, format(%2.0f)) ytitle(Percent) graphregion(color(white)) ///
		note("Sources: India National Sample Survey (NSSO) Surveys.")
graph export "$graphs\pov_320.pdf", replace	


twoway  (line pov_550National year, lpattern(solid) lcolor(black) lwidth(thick)) (line pov_550Urban year, lpattern(dash) lcolor(purple) lwidth(thick)) (line pov_550Rural year, lpattern(longdash) lcolor(forest_green) lwidth(thick)) ///
		(scatter pov_550National year, mlabel(pov_550National) mlabv(labpos) mlabcolor(black) lpattern(solid) mcolor(black) lwidth(thick)), xtitle("Year")  legend(label(1 "National") label(2 "Urban")  ///
		label(3 "Rural") label(4 "Year of Survey")) xmticks(##2) xlabel(2004.5 (5) 2004.5) yscale(r(70 100)) ylabel(70(5)100, format(%2.0f)) ytitle(Percent) graphregion(color(white)) ///
		note("Sources: India National Sample Survey (NSSO) Surveys.")
graph export "$graphs\pov_550.pdf", replace	
		
		



log close 
