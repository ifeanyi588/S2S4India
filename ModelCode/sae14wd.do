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
cap log close
cap log using sae14_log_replicate, text replace 

* locals for directories and variables

*local datain "$data\finaldata\sae14.dta"
local datainu "$data\finaldata\sae14u.dta"
local datainr "$data\finaldata\sae14r.dta"

local dataout "$analysis\SAE_MATA_wd"
local dataoutu "$analysis\SAE_MATAu_wd"
local dataoutr "$analysis\SAE_MATAr_wd"


local ydump "$analysis\mata_wd"
local ydumpu "$analysis\matau_wd"
local ydumpr "$analysis\matar_wd"

glo res "$analysis\SAE_results"

/*
* Constant coefficient: per capita expd, psu level means, different age composition
local xvar010	"agri agrih	Dagecat1 Dagecat1h Dagecat2	Dagecat2h Dagecat3 Dagecat3h Dagecat4 Dagecat4h	Dagecat6 Dagecat6h Dhhsizecat1 Dhhsizecat1h	Dhhsizecat2	Dhhsizecat2h Dhhsizecat3 Dhhsizecat3h Dhhsizecat4 Dhhsizecat4h Dhhsizecat6 Dhhsizecat6h	hhr	hhrh highskillocc highskillocch	hindu hinduh ind indh middleskillocc middleskillocch rainfallQ1	rainfallQ2	rainfallQ3	rainfallQ4	recr recrh scst	scsth selfemployed selfemployedh transpr transprh"																						
* With Time Trends on all hhlevel hhsize and age category variables																																																																		
local xvar010t	"agri agrih	Dagecat1 Dagecat1h Dagecat1ht Dagecat2 Dagecat2h Dagecat2ht	Dagecat3 Dagecat3h Dagecat3ht Dagecat4 Dagecat4h Dagecat4ht	Dagecat6 Dagecat6h	Dagecat6ht	Dhhsizecat1	Dhhsizecat1h Dhhsizecat1ht	Dhhsizecat2	Dhhsizecat2h Dhhsizecat2ht	Dhhsizecat3	Dhhsizecat3h Dhhsizecat3ht	Dhhsizecat4	Dhhsizecat4h Dhhsizecat4ht	Dhhsizecat6	Dhhsizecat6h Dhhsizecat6ht hhr	hhrh hhrht highskillocc	highskillocch hindu	hinduh	hindut	ind	indh indht middleskillocc middleskillocch rainfallQ1 rainfallQ2	rainfallQ3 rainfallQ4 recr recrh recrt scst	scsth selfemployed selfemployedh selfemployedht	t transpr transprh"						
* With Time Trends on only some hh and age category that have time trends																																																																		
local xvar011t	"agri agrih	Dagecat1 Dagecat1h Dagecat1ht Dagecat2 Dagecat2h Dagecat3 Dagecat3h	Dagecat4 Dagecat4h Dagecat6	Dagecat6h Dhhsizecat1 Dhhsizecat1h Dhhsizecat1ht Dhhsizecat2 Dhhsizecat2h Dhhsizecat2t Dhhsizecat3 Dhhsizecat3h	Dhhsizecat4	Dhhsizecat4h Dhhsizecat6 Dhhsizecat6h hhr hhrh hhrht highskillocc highskillocch	hindu hinduh hindut	ind	indh indht middleskillocc middleskillocch rainfallQ1 rainfallQ2	rainfallQ3	rainfallQ4 recr	recrh recrt scst scsth selfemployed	selfemployedh selfemployedht t transpr transprh"													

* rural 
** specification is copied and pasted from modelselection.do 
local xvar011t "Dhhsizecat? Dhhsizecat?h Dagecat? Dagecat?h hindu hinduh scst scsth " 
local xvar011t  "`xvar011t' selfemployed selfemployedh casualurban regwageurban highskillocc highskillocch " 
local xvar011t  "`xvar011t'	middleskillocc	middleskillocch hhr hhrh recr recrh transpr	transprh rainfallQ? rainfallQ?sq t"

 foreach var in Dhhsizecat2h Dhhsizecat3h  Dhhsizecat4h Dhhsizecat6h  ///
				Dagecat1h Dagecat4h Dagecat6h scsth selfemployedh ///
				middleskillocch highskillocch recrh agrih  {
	local xvar011t "`xvar011t' `var't" 
	} 


* urban 
* Constant coefficient: per capita expd, psu level means, different age composition, hhsize categories;  casualurban	casualurbanh  removed due to multicollinearity
local xvar110 "agri	agrih	Dagecat1	Dagecat1h	Dagecat2	Dagecat2h	Dagecat3	Dagecat3h	Dagecat4	Dagecat4h	Dagecat6	Dagecat6h	Dhhsizecat1	Dhhsizecat1h	Dhhsizecat2	Dhhsizecat2h	Dhhsizecat3	Dhhsizecat3h	Dhhsizecat4	Dhhsizecat4h	Dhhsizecat6	Dhhsizecat6h	hhr	hhrh	highskillocc	highskillocch	hindu	hinduh	ind	indh	middleskillocc	middleskillocch	rainfallQ1	rainfallQ2	rainfallQ3	rainfallQ4	recr	recrh	regwageurban	regwageurbanh	scst	scsth	selfemployed	selfemployedh	transpr	transprh"																
* With Time Trends on all hhlevel hhsize and age category variables;  casualurban	casualurbanh  removed due to multicollinearity																																																																
local xvar110t "agri	agrih	Dagecat1	Dagecat1h	Dagecat1ht	Dagecat2	Dagecat2h	Dagecat2ht	Dagecat3	Dagecat3h	Dagecat3ht	Dagecat4	Dagecat4h	Dagecat4ht	Dagecat6	Dagecat6h	Dagecat6ht	Dhhsizecat1	Dhhsizecat1h	Dhhsizecat1ht	Dhhsizecat2	Dhhsizecat2h	Dhhsizecat2ht	Dhhsizecat3	Dhhsizecat3h	Dhhsizecat3ht	Dhhsizecat4	Dhhsizecat4h	Dhhsizecat4ht	Dhhsizecat6	Dhhsizecat6h	Dhhsizecat6ht	hhr	hhrh	highskillocc	highskillocch	highskilloccht	hindu	hinduh	hindut	ind	indh	middleskillocc	middleskillocch	rainfallQ1	rainfallQ2	rainfallQ3	rainfallQ4	recr	recrh	regwageurbanh	regwageurbanht	scst	scsth	scstht	selfemployedh	t	transpr	transprh"
* With Time Trends on only some hh and age category that have time trends;  casualurban	casualurbanh  removed due to multicollinearity																																																																
local xvar111t "agri	agrih	agriht		Dagecat1	Dagecat1h Dagecat1t	Dagecat2	Dagecat2h	Dagecat2t	Dagecat3	Dagecat3h	Dagecat4	Dagecat4h	Dagecat4ht	Dagecat6	Dagecat6h	Dhhsizecat1	Dhhsizecat1h	Dhhsizecat2	Dhhsizecat2h	Dhhsizecat2t	Dhhsizecat3	Dhhsizecat3h	Dhhsizecat4	Dhhsizecat4h	Dhhsizecat6	Dhhsizecat6h	Dhhsizecat6t	hhr	hhrh	highskillocc	highskillocch	highskilloccht	hindu	hinduh	ind	indh	indht	indt	middleskillocc	middleskillocch	rainfallQ1	rainfallQ2	rainfallQ3	rainfallQ4	recr	recrh	recrht	regwageurban regwageurbanh	regwageurbanht	scst	scsth	selfemployed selfemployedh	t	transpr	transprh"			


local xvar111t "Dhhsizecat? Dhhsizecat?h Dagecat? Dagecat?h"
local xvar111t "`xvar111t' hindu hinduh scst scsth	regwageurban regwageurbanh selfemployed selfemployedh"
local xvar111t	"`xvar111t'	 casualurban casualurbanh highskillocc	highskillocch middleskillocc"
local xvar111t "`xvar111t' middleskillocch hhr hhrh recr recrh transpr	transprh rainfallQ? rainfallQ?sq t" 

 foreach var in Dhhsizecat3h Dhhsizecat6h Dagecat4h ///
				regwageurbanh casualurbanh casualurban  ///
                     highskillocch middleskillocch middleskillocc hhrh  agrih indh {
		local xvar111t "`xvar111t' `var't" 
		} 
*/
********************************************************************************
************************************ MODEL ************************************
********************************************************************************

* First Stage
use "$data\finaldata\hh_reg.dta", clear
drop Dagecat6 Dagecat6h Dhhsizecat6 Dhhsizecat6h
unab xvar011t : $xvar011t 
unab xvar111t : $xvar111t 

gen lnwelf = ln(welfareintr)
*local xvar111t : subinstr local xvar111t "Dhhsizecat4ht" "" 
*local xvar111t : subinstr local xvar111t "Dhhsizecat5ht" "" 
local xvar111t : subinstr local xvar111t "Dagecat4ht" "" 
local xvar111t : subinstr local xvar111t "casualurbanht" "" 
local xvar111t : subinstr local xvar111t "regwageurbanht" ""  


gen intyear=year-0.5 

mean recr [aw=pop_wgt]
mean recr [aw=pop_wgt] if year==2014.5 & urban==1 


summ `xvar111t' if urban==1 & year==2014.5 [aw=pop_wgt]

keep if year<2014

save "$data2\sae0411.dta", replace



* Urban
use "$data2\sae0411.dta", clear
keep if urban==1
save "$data\finaldata\sae14u.dta", replace


regress lnwelf `xvar111t' [aw=pop_wgt], vce(robust) 



sae model povmap lnwelf `xvar111t' [pw=pop_wgt], area(id_sae_1) varest(ell) vce(ell) alfatest(robust) zvar(`xvar111t')
estimates table 

estimates store indu
estimates save "$estimates/stage1_14_indu_wd", replace 


qui: lassoregress robust_alfa `xvar111t' [aw=pop_wgt]

local error_lasso_modelu "`e(varlist_nonzero)'"

* Rural
use "$data2\sae0411.dta", clear
keep if urban==0
save "$data\finaldata\sae14r.dta", replace

sae model povmap lnwelf `xvar011t' [pw=pop_wgt], area(id_sae_1) varest(ell) vce(ell) alfatest(robust) zvar(`xvar011t')
estimates store indr

estimates save "$estimates/stage1_14_indr_wd", replace 

qui: lassoregress robust_alfa `xvar011t' [aw=pop_wgt]

local error_lasso_modelr "`e(varlist_nonzero)'"

estout indu indr using "$tables/indfirsta_wd.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2_beta N_beta F_beta, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (Dhhsizecat2h Dhhsizecat2ht Dhhsizecat3h	Dhhsizecat3ht	Dhhsizecat4h	Dhhsizecat4ht Dhhsizecat5h Dhhsizecat5ht	Dagecat1h	Dagecat2h	Dagecat2ht	Dagecat3h Dagecat3ht	Dagecat4h Dagecat4ht	Dagecat5h Dagecat5ht ) ///
order ( Dhhsizecat2h Dhhsizecat2ht Dhhsizecat3h	Dhhsizecat3ht	Dhhsizecat4h	Dhhsizecat4ht Dhhsizecat5h Dhhsizecat5ht Dagecat1h	Dagecat2h Dagecat2ht	Dagecat3h	Dagecat3ht Dagecat4h Dagecat4ht	Dagecat5h Dagecat5ht) ///
varlabels(Dagecat1h "Prop HH 0-15 yrs" Dagecat2h "Prop HH 16-24 yrs" Dagecat3h "Prop HH 25-34 yrs" Dagecat4h "Prop HH 35-49" Dagecat6h "Prop HH 65+ yrs" Dagecat1ht "Prop HH 0-15 yrs*t" Dagecat2ht "Prop HH 16-24 yrs*t" Dagecat3ht "Prop HH 25-34 yrs*t" Dagecat4ht "Prop HH 35-49 yrs*t" Dagecat5h "Prop HH 50-64 yrs" Dagecat5ht "Prop HH 50-64 yrs*t" Dhhsizecat1h "HH size 1"  Dhhsizecat2h "HH size 1 or 2" Dhhsizecat3h "HH size 3" Dhhsizecat4h "HH size 4" Dhhsizecat5h "HH size 5"  Dhhsizecat1ht "HH size 1*t" Dhhsizecat2ht "HH size 1 or 2*t" Dhhsizecat3ht "HH size 3*t" Dhhsizecat4ht "HH size 4*t" Dhhsizecat5ht "HH size 5*t" Dhhsizecat6ht "HH size 6+*t") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead( & \textbf{Urban} & \textbf{Rural} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout indu indr using "$tables/indfirstb_wd.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2_beta N_beta F_beta, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (hinduh hinduht   scsth scstht  selfemployedh selfemployedht casualurbanh casualurbanht  regwageurbanh regwageurbanht agrih agriht indh  indht highskillocch highskilloccht middleskillocch middleskilloccht transprh transprht ) ///
order(hinduh  hinduht scsth scstht selfemployedh selfemployedht casualurbanh casualurbanht regwageurbanh regwageurbanht agrih agriht indh  indht highskillocch highskilloccht middleskillocch middleskilloccht transprh transprht   ) ///
varlabels(hinduh "Hindu" hinduht "Hindu*t" scsth "Low caste" scstht "Low caste*t" selfemployedh "HH type: Self Employed" selfemployedht "HH type: Self Employed*t" regwageurbanh "HH type: Regular Wage Worker" regwageurbanht "HH type: Regular Wage Worker*t" casualurbanh "HH type: Casual Laborer" casualurbanht "HH type: Casual Laborer*t" agrih "Princip Ind: Agri" agriht "Princip Ind: Agri*t" indh "Princip Ind: Industry" indht "Princip Ind: Industry*t" highskillocch "High Skill Occupation" middleskillocch "Middle Skill Occupation" highskilloccht "High Skill Occupation*t" middleskilloccht "Middle Skill Occupation*t" transprh "Transport services expenses" transprht "Transport services expenses*t" ) ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead( & \textbf{Urban} & \textbf{Rural} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  indu indr using "$tables/indfirstc_wd.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2_beta N_beta F_beta, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (recrh recrht hhrh hhrht selfemployed casualurban casualurbant regwageurban regwageurbant hindu  ind highskillocc  middleskillocc middleskillocct transpr   recr recrt  hhr) ///
order (recrh recrht hhrh hhrht  selfemployed casualurban casualurbant regwageurban regwageurbant hindu ind highskillocc  middleskillocc middleskillocct transpr  recr recrt  hhr) ///
varlabels(recrh "Recreation services expenses" recrht "Recreation services expenses*t" hhrh "Household services expenses" hhrht "Household services expenses*t" regwageurban "HH type: Regular Wage Worker (Dist)" regwageurbant "HH type: Regular Wage Worker (Dist)*t" selfemployed "HH type: Self Employed (Dist)" casualurban "HH type: Casual Laborer (Dist)" casualurbant "HH type: Casual Laborer (Dist)*t"  hindu "Hindu (Dist)" hindut "Hindu (Dist)*t" scst "Low Caste (Dist)"  agri "Princip Ind: Agri (Dist)" ind "Princip Ind: Industry (Dist)" indt "Princip Ind: Industry (Dist)*t" highskillocc "High Skill Occupation (Dist)" middleskillocc "Middle Skill Occupation(Dist)" middleskillocct "Middle Skill Occupation(Dist)*t" transpr "Transport services expenses (Dist)" recr "Recreation services expenses (Dist)" recrt "Recreation services expenses (Dist)*t" hhr "Household services expenses (Dist)") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead( & \textbf{Urban} & \textbf{Rural} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  indu indr using "$tables/indfirstd_wd.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2_beta N_beta F_beta, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (Dagecat2  Dagecat3 Dagecat4 Dagecat5  Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5  rainfallQ1  rainfallQ2 rainfallQ3  rainfallQ4  rainfallQ1sq  rainfallQ2sq  rainfallQ3sq  rainfallQ4sq  t ) ///
order (Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5   Dagecat2 Dagecat2t Dagecat3 Dagecat4 Dagecat5 rainfallQ1 rainfallQ1sq rainfallQ2 rainfallQ2sq rainfallQ3  rainfallQ3sq rainfallQ4 rainfallQ4sq  t  ) ///
varlabels(Dagecat1 "Prop HH 0-15 yrs (Dist)" Dagecat1t "Prop HH 0-15 yrs (Dist)*t" Dagecat2 "Prop HH 16-24 yrs (Dist)" Dagecat2t "Prop HH 16-24 yrs (Dist)*t" Dagecat3 "Prop HH 25-34 yrs (Dist)" Dagecat4 "Prop HH 35-49 (Dist)" Dagecat5 "Prop HH 50-64 yrs (Dist)" agecat6 "Prop HH 65+ yrs (Dist)" Dhhsizecat1 "HH size 1 (Dist)" Dhhsizecat2 "HH size 1 or 2 (Dist)" Dhhsizecat2t "HH size 2 (Dist)*t" Dhhsizecat3 "HH size 3 (Dist)" Dhhsizecat4 "HH size 4 (Dist)" Dhhsizecat5 "HH size 5 (Dist)" Dhhsizecat6 "HH size 6 (Dist)" Dhhsizecat6t "HH size 6 (Dist)*t" rainfallQ1 "Rainfall Q1" rainfallQ2 "Rainfall Q2" rainfallQ3 "Rainfall Q3" rainfallQ4 "Rainfall Q4" rainfallQ1sq "Rainfall Q1$^2$" rainfallQ2sq "Rainfall Q2$^2$" rainfallQ3sq "Rainfall Q3$^2$" rainfallQ4sq "Rainfall Q4$^2$" t "Time trend") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Urban} & \textbf{Rural} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

 

************************************************* Second Stage - Urban **********************************************
sae data import, varlist( `xvar111t' qhweight qhclust psu qhnum state id_sae_1) uniqid(id_sae) area(id_sae_1) ///
datain(`datainu') dataout(`dataoutu')

use "$data2\sae0411.dta", clear
keep if urban==1

* assume normality 
sae sim povmap lnwelf `xvar111t' [pw=pop_wgt], area(id_sae_1) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(100) seed(57678) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutu') indicators(fgt0) allmata zvar(`error_lasso_modelu') ///
pline(57.791668) addvars(id_sae qhclust psu qhnum `xvar111t' state id_sae_1) 
*/ 



global xvar111t "`xvar111t'" 
global error_lasso_modelu "`error_lasso_modelu'"

/* non-paramertric
sae sim povmap lnwelf `xvar111t' [pw=pop_wgt], area(id_sae_1) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(5) seed(57678) lny aggids(7) col(1) stage(second) ydump(`ydumpu') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutu') indicators(fgt0) allmata zvar(`error_lasso_modelu') ///
pline(57.791668) addvars(id_sae qhclust psu qhnum `xvar111t') 
*/ 

* Export data
sae data export, matasource(`ydumpu') numfiles(1) datasave($analysis\final_datau_wd)

* Poverty numbers
sae proc indicator, matasource(`ydumpu') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impu_wd.dta", replace

************************************************* Second Stage - Rural **********************************************
sae data import, varlist( `xvar011t' qhweight qhclust psu qhnum state id_sae_1) uniqid(id_sae) area(id_sae_1) ///
datain(`datainr') dataout(`dataoutr')

use "$data2\sae0411.dta", clear
keep if urban==0
* assume normality  
sae sim povmap lnwelf `xvar011t' [pw=pop_wgt], area(id_sae_1) varest(ell) eta(normal) epsilon(normal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) bootstrap lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) ebest matin(`dataoutr') indicators(fgt0) allmata zvar(`error_lasso_modelr') ///
pline(57.791668) addvars(id_sae qhclust psu qhnum `xvar011t' state id_sae_1) 
*/ 

/* non-paramertic estimation
sae sim povmap lnwelf `xvar011t' [pw=pop_wgt], area(id_sae_1) varest(ell) eta(nonnormal) epsilon(nonnormal) ///
uniqid(id_sae) vce(ell) rep(100) seed(16757) lny aggids(7) col(1) stage(second) ydump(`ydumpr') ///
res(`resr') pwcensus(qhweight) psu(qhclust) matin(`dataoutr') indicators(fgt0) allmata zvar(`error_lasso_modelr') ///
pline(57.791668) addvars(id_sae qhclust psu qhnum `xvar011t')  
*/ 

* Export data
sae data export, matasource(`ydumpr') numfiles(1) datasave($analysis\final_datar_wd)

* Poverty numbers
sae proc indicator, matasource(`ydumpr') aggids(7 8) indicators(fgt0) plines(57.791668) 

save "$res\impr_wd.dta", replace

*************************************************************************************************************************
**** Predicted poverty

	* Importing dataset (National)
	use "$analysis\final_datau_wd.dta", clear
	gen urban=1
	append using "$analysis\final_datar_wd.dta"
	replace urban=0 if urban==.
	save "$analysis\final_dataur_wd.dta", replace
	
	gen welfare=. 

	* Importing dataset into MI format
	mi import wide, imputed(welfare=_YHAT*) clear
	drop _YHAT* 

	save "$analysis\mi_data_wd", replace 
	
	
	
use "$data\finaldata\hhmaster.dta", clear	
summ welfareintr [aw=pop_wgt] if urban==0 
global max_welfarer=r(max) 
summ welfareintr [aw=pop_wgt] if urban==1 
global max_welfareu=r(max) 
	
	use "$analysis\mi_data_wd", clear
	
	
	
	 
	
	
	
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


	use "$analysis\mi_data_wd", clear
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
	estimates save "D:\Poverty_Estimates_India\estimates\replication\sae14_wd", replace 
	
	** replicate subsample of compete distrits for comparison with 
	preserve 
	*keep if Nyearsdistrict==4 
	mi estimate: svy: mean Dpoor* povgap* povseverity*, over(urban)
	estimates save "D:\Poverty_Estimates_India\estimates\sae14_completedistrictsubsample_wd", replace 
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
