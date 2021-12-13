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
*cap log using sae18_log_test_newvars, text replace
*cap log using sae18_log_test_newvars_withselection, text replace
cap log using sae18_log_test_newvars_withselection_nonnormal_lessvars, text replace


* locals for directories and variables

local datain "$data2\sae18.dta"
local datainu "$data2\sae18u.dta"
local datainr "$data2\sae18r.dta"

local dataout "$analysis\SAE_MATA_wd18_newvars_withselection_nonnormal_lessvar"
local dataoutu "$analysis\SAE_MATAu_wd18_newvars_withselection_nonnormal_lessvars"
local dataoutr "$analysis\SAE_MATAr_wd18_newvars_withselection_nonnormal_lessvars"


local ydump "$analysis\mata_wd18_newvars_withselection_nonnormal_lessvars"
local ydumpu "$analysis\matau_wd18_newvars_withselection_nonnormal_lessvars"
local ydumpr "$analysis\matar_wd18_newvars_withselection_nonnormal_lessvars"

glo res "$analysis\SAE_results"


********************************************************************************
************************************ MODEL ************************************
********************************************************************************

* First Stage
use "$path\data\finaldata\hhmaster.dta", clear

gen lnwelf = ln(welfareintr)

global ruralvars "Dagecat1h Dagecat2h Dagecat3 Dagecat4h Dagecat5 Dagecat5h Dhhsizecat2h Dhhsizecat3" 
global ruralvars "${ruralvars} Dhhsizecat4 Dhhsizecat4h Dhhsizecat5 Dhhsizecat5h highskillocc highskillocch" 
global ruralvars "${ruralvars} middleskillocc middleskillocch ind indh scsth selfemployed selfemployedh" 
global ruralvars "${ruralvars} depratioh Dhhsizecat3h hhage"
global ruralvars "${ruralvars} Dmarriedh Dmarried" 
global ruralvars "${ruralvars} Dmaleh Dmale" 
global ruralvars "${ruralvars} Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh"
global ruralvars "${ruralvars} Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh"  
global ruralvars "${ruralvars} rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"


global urbanvars "Dagecat1h Dagecat2 Dagecat2h Dagecat3 Dagecat4h Dagecat5 Dagecat5h Dhhsizecat2h Dhhsizecat3" 
global urbanvars "${urbanvars} Dhhsizecat4 Dhhsizecat4h Dhhsizecat5 Dhhsizecat5h highskillocc highskillocch" 
global urbanvars "${urbanvars} middleskillocc middleskillocch ind indh scst scsth selfemployed selfemployedh" 
global urbanvars "${urbanvars} regwageurban regwageurbanh depratio depratioh"
global urbanvars "${urbanvars} Dmarriedh Dmarried" 
global urbanvars "${urbanvars} Dmaleh Dmale hhage" 
global urbanvars "${urbanvars} Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dcokecoal Dkeroseneh"
global urbanvars "${urbanvars} Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg Dlpgh"  
global urbanvars "${urbanvars} rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"


gen intyear=year-0.5 


**** the next step is to do a model with selection using a stepwise process
stepwise, pr(0.05): reg lnwelf $urbanvars [pw = wgt] if urban == 1
local temp : colfullnames e(b) 
local b _cons
local xvars : list temp - b
glo urbanvars2 `xvars'

stepwise, pr(0.05): reg lnwelf $ruralvars [pw = wgt] if urban == 0
local temp : colfullnames e(b) 
local b _cons
local xvars : list temp - b
glo ruralvars2 `xvars'


keep if year<2014






save "$data2\sae0411.dta", replace

* Urban
use "$data2\sae0411.dta", clear
keep if urban==1


regress lnwelf $urbanvars2 [aw=pop_wgt] 

reg lnwelf $urbanvars2 [aw = pop_wgt]
predict _yhat if e(sample), xb
sum lnwelf _yhat

sae model lmm lnwelf $urbanvars2[aw=pop_wgt], area(id_sae_1)  varest(h3)

* Create variables for the alpha model
global vary
global vary2
foreach var of global urbanvars2 {
	gen double `var'_y = `var'*_yhat
	gen double `var'_y2 = `var'*_yhat*_yhat
	global vary "$vary `var'_y"
	global vary2 "$vary2 `var'_y2"
}

 * Alpha model selection - here you can do the different model selections until you think that is good
stepwise, pr(0.05): reg _res_alfa $urbanvars2 $vary $vary2 [pw=wgt]
mat b = e(b)'
local mm= rowsof(b)
local nn = `mm'-1
mat c = b[1..`nn',.]
local rname : rowfullnames c
global B = "`rname'"
di "$B"
global alphalist "$B"

reg _res_alfa $alphalist [pw=wgt]
reg _res_alfa $alphalist [pw=wgt], robust
global zvar
global yhat
global yhat2
foreach var of global alphalist {
	if strpos("`var'","_y2")>0 {
		global yhat2 "$yhat2 `=substr("`var'", 1, `=strlen("`var'")-3')'"
	}
	else {
		if strpos("`var'","_y")>0 global yhat "$yhat `=substr("`var'", 1, `=strlen("`var'")-2')'"
		else global zvar "$zvar `var'"
	}
}

sae model povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(h3) alfatest(_res) zvar($urbanvars2) yhat($yhat) yhat2($yhat2)
estimates table 

estimates store indu
estimates save "$estimates/stage1_18_indu_newvars_withselection_nonnormal", replace 


qui: lassoregress robust_alfa $urbanvars2 [aw=pop_wgt]

local error_lasso_modelu "`e(varlist_nonzero)'"

* Rural
use "$data2\sae0411.dta", clear
keep if urban==0
* Create variables for the alpha model
global vary
global vary2
foreach var of global ruralvars2 {
	gen double `var'_y = `var'*_yhat
	gen double `var'_y2 = `var'*_yhat*_yhat
	global vary "$vary `var'_y"
	global vary2 "$vary2 `var'_y2"
}

 * Alpha model selection - here you can do the different model selections until you think that is good
stepwise, pr(0.05): reg _res_alfa $ruralvars2 $vary $vary2 [pw=wgt]
mat b = e(b)'
local mm= rowsof(b)
local nn = `mm'-1
mat c = b[1..`nn',.]
local rname : rowfullnames c
global B = "`rname'"
di "$B"
global alphalist "$B"

reg _res_alfa $alphalist [pw=wgt]
reg _res_alfa $alphalist [pw=wgt], robust
global zvar
global yhat
global yhat2
foreach var of global alphalist {
	if strpos("`var'","_y2")>0 {
		global yhat2 "$yhat2 `=substr("`var'", 1, `=strlen("`var'")-3')'"
	}
	else {
		if strpos("`var'","_y")>0 global yhat "$yhat `=substr("`var'", 1, `=strlen("`var'")-2')'"
		else global zvar "$zvar `var'"
	}
}

sae model povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(h3) alfatest(_res) zvar($ruralvars2) yhat($yhat) yhat2($yhat2) 
estimates store indr

estimates save "$estimates/stage1_18_indr_newvars_withselection_nonnormal", replace 

qui: lassoregress robust_alfa $ruralvars2 [aw=pop_wgt]

local error_lasso_modelr "`e(varlist_nonzero)'"


************************************************* Second Stage - Urban **********************************************
sae data import, varlist(id_sae qhweight qhclust $urbanvars2 district_code) uniqid(id_sae) area(district_code) ///
datain(`datainu') dataout(`dataoutu')

use "$data2\sae0411.dta", clear
keep if urban==1

/* assume normality 
sae sim povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(5) seed(57678) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutu') indicators(fgt0) allmata zvar(`error_lasso_modelu') ///
pline(57.791668) addvars(id_sae qhweight qhclust $urbanvars2 district_code) 
*/ 



global urbanvars "$urbanvars" 
global error_lasso_modelu "`error_lasso_modelu'"

* non-parametric
sae sim povmap lnwelf $urbanvars2 [pw=pop_wgt], area(district_code) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(57678) lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutu') indicators(fgt0) /*allmata zvar(`error_lasso_modelu')*/ ///
pline(57.791668) addvars(id_sae qhweight qhclust $urbanvars2) 
*/ 

* Export data
sae data export, matasource(`ydumpu') numfiles(1) datasave($analysis\final_datau_wd18_newvars_withselection_nonnormal)

* Poverty numbers
sae proc indicator, matasource(`ydumpu') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impu_wd18_newvars_withselection_nonnormal.dta", replace

************************************************* Second Stage - Rural **********************************************
sae data import, varlist(id_sae qhweight qhclust $ruralvars2 district_code) uniqid(id_sae) area(district_code) ///
datain(`datainr') dataout(`dataoutr')

use "$data2\sae0411.dta", clear
keep if urban==0
/* assume normality  
sae sim povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(5) seed(16757) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutr') indicators(fgt0) allmata zvar(`error_lasso_modelr') ///
pline(57.791668) addvars(id_sae qhweight qhclust $ruralvars2 district_code) 
*/ 

* non-paramertic estimation
sae sim povmap lnwelf $ruralvars2 [pw=pop_wgt], area(district_code) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutr') indicators(fgt0) /*allmata zvar(`error_lasso_modelr')*/ ///
pline(57.791668) addvars(id_sae qhclust $ruralvars2 district_code)  
*/ 

* Export data
sae data export, matasource(`ydumpr') numfiles(1) datasave($analysis\final_datar_wd18_newvars_withselection_nonnormal)

* Poverty numbers
sae proc indicator, matasource(`ydumpr') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impr_wd18_newvars_withselection_nonnormal.dta", replace

*************************************************************************************************************************
**** Predicted poverty

	* Importing dataset (National)
	use "$analysis\final_datau_wd18_newvars_withselection_nonnormal_lessvars.dta", clear
	gen urban=1
	append using "$analysis\final_datar_wd18_newvars_withselection_nonnormal_lessvars.dta"
	replace urban=0 if urban==.
	save "$analysis\final_dataur_wd18_newvars_withselection_nonnormal_lessvars.dta", replace
	
	gen welfare=. 

	* Importing dataset into MI format
	mi import wide, imputed(welfare=_YHAT*) clear
	drop _YHAT* 

	save "$analysis\mi_data_wd18_newvars_withselection_nonnormal_lessvars", replace 
	
		
use "$path\data\finaldata\hhmaster.dta", clear	
summ welfareintr [aw=pop_wgt] if urban==0 
global max_welfarer=r(max) 
summ welfareintr [aw=pop_wgt] if urban==1 
global max_welfareu=r(max) 
	
	use "$analysis\mi_data_wd18_newvars_withselection_nonnormal_lessvars", clear
	
	
	
	 
	
	
	
	/** Calculate gini coefficient and variance 
	preserve 
	* drop outliers across all imputations 
	egen welfare_mean=rowmean(_*_welfare)
	* trim if above max survey value, separately for urban rural 
	drop if welfare_mean>=$max_welfarer & urban==0  
	drop if welfare_mean>=$max_welfareu & urban==1
	
	gen ginir=. 
	gen ginir_var=. 
	gen giniu=. 
	gen giniu_var=. 
	foreach imputation of numlist 1/100 { 
	if mod(`imputation',10)==0 { 
	dis "`imputation'" _c 
	} 
	else {
	dis "." _c 
	} 
	
	* drop if _`imputation'_welfare>=r(r1) 
	qui fastgini _`imputation'_welfare if urban==0 [pw=_WEIGHT], jk
	qui replace ginir=r(gini) in `imputation' 
	qui replace ginir_var=r(se)^2 in `imputation' 
	qui fastgini _`imputation'_welfare if urban==1 [pw=_WEIGHT], jk
	qui replace giniu=r(gini) in `imputation' 
	qui replace giniu_var=r(se)^2 in `imputation' 
	* ainequal _`imputation'_welfare [aw=_WEIGHT]
	* qui replace gini=real(r(gini_1)) in `imputation' 
	} 
	** apply rubin's rule: V = W + (1+1/M)B
	** W = average jackknife varianece, across imputations 
	** B = 1/(m-1) Sum (Ghat - Gbar)^2 where Ghat is gini estimate and Gbar is average across imputations 
	qui summ giniu_var 
	local W=r(mean) 
	qui summ giniu 
	local B=r(sd)^2 
	scalar giniu_14=r(mean) 
	dis 
	dis "Gini estimate is " %5.3f `giniu_14' " In urban areas" 
	scalar gini_14_sdu=(`W'+(1.01)*`B')^0.5 
	dis "Standard deviation is: " %5.3f `gini_14_sd' 
	
	qui summ ginir_var 
	local W=r(mean) 
	qui summ ginir 
	local B=r(sd)^2 
	scalar ginir_14=r(mean) 
	dis "Gini estimate is " %5.3f `ginir_14' " In rural areas" 
	scalar gini_14_sdr=(`W'+(1.01)*`B')^0.5 
	dis "Standard deviation is: " %5.3f `gini_14_sd' 
	
	mat a=J(2,4,.)
	mat a[1,1]=giniu_14*100 
	mat a[2,1]=ginir_14*100
	mat a[1,2]= (giniu_14 - 1.96*(gini_14_sdu))*100
	mat a[1,3]=(giniu_14 + 1.96*(gini_14_sdu))*100
	mat a[2,2]=(ginir_14 - 1.96*(gini_14_sdr))*100
	mat a[2,3]=(ginir_14 + 1.96*(gini_14_sdr))*100
	mat a[1,4]=39.0144
	mat a[2,4]=31.1275
	
mat rownames a= "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (a, fmt(%9.1fc)) using "$tables/gini.tex", /// 
nonumbers nomtitles nodepvars align(rrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Note: Trimmed outliers above maximum" "per capita consumption in NSS surveys.""a. PovcalNet") ///
posthead(& {\textbf{2014.5}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & {\textbf{2011.5$^a$}} \\ "\hline") replace
	
	
	** Calculate mean and median 
mi estimate,  dots: mean welfare [pw=_WEIGHT], over(urban) 
	mat mean=J(2,4,.)
	mat xx=r(table)'
	mat mean[1,1]=xx[2,1]
	mat mean[2,1]=xx[1,1]
	mat mean[1,2]=xx[2,5]
	mat mean[1,3]=xx[2,6]
	mat mean[2,2]=xx[1,5]
	mat mean[2,3]=xx[1,6]
	mat mean[1,4]=158.63
	mat mean[2,4]=103.66
	
	mi estimate,  dots: qreg welfare if urban==0 [pw=_WEIGHT]
	mat yy = r(table)'
	mat medr=J(1,1,.)
    mat medr[1,1]=yy[1,1]

	mi estimate,  dots: qreg welfare if urban==1 [pw=_WEIGHT]
	mat zz = r(table)'
	mat medu=J(1,1,.)
    mat medu[1,1]=zz[1,1]
	
	mat med=J(2,2,.)
	mat med[1,1]= medu[1,1]
	mat med[2,1]=medr[1,1]
	mat med[1,2]=116.23
	mat med[2,2]=83.93
	
	mat sumnonp=mean, med
	mat list sumnonp
	
mat rownames sumnonp= "\textbf{Urban}" "\textbf{Rural}"
esttab matrix (sumnonp, fmt(%9.1fc)) using "$tables/summ_sim.tex", /// 
nonumbers nomtitles nodepvars align(rrrrrr) style(tab) ///
collabel(none) modelwidth(10) ///
addnotes("Note: Trimmed outliers above maximum" "per capita consumption in NSS surveys." "a. 2011.5") ///
posthead(& {\textbf{Mean}} & \multicolumn{2}{c}{\textbf{95\% C.I}} & {\textbf{PovcalNet$^a$}} & {\textbf{Median}} & {\textbf{PovcalNet$^a$}}\\ "\hline") replace


	

	restore 
*/ 


	use "$analysis\mi_data_wd18_newvars_withselection_nonnormal_lessvars", clear
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
	estimates save "D:\Poverty_Estimates_India\estimates\replication\sae18_newvars_withselection_nonnormal", replace 
	
	**estimates for the overall national
	mi estimate: svy: mean Dpoor* povgap* povseverity*
	
	log close
	exit
	
	** replicate subsample of compete distrits for comparison with 
	preserve 
	*keep if Nyearsdistrict==4 
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "D:\Poverty_Estimates_India\estimates\sae18_completedistrictsubsample_newvars_withselection_nonnormal", replace 
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
