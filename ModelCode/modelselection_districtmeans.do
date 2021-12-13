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
******  Checking for Trends in the Coefficients: Using the 2004.5, 2009.5 and 2011.5 surveys.  ******
*                                                                                                   *
*****************************************************************************************************/
cap log close 
log using "modelselection_districtmeans", text replace 



** Rural model 

dis "`trends_rural'" 
 
 local model1_rur "$candidate_xvars_rur" 
* do not interact rainfall with time, given the draught it is just asking too much of the data 
 local interactions_rur "`model1_rur'" 
local interactions_rur : subinstr local interactions_rur "rainfallQ?" ""
local interactions_rur : subinstr local interactions_rur "rainfallQ?sq" ""
 
 foreach var in `interactions_rur' {
		local model1_rur "`model1_rur' `var't" 
		} 
local model1_rur "`model1_rur' t"  
		
dis "`model1_rur'" 

use "$data\finaldata\hh_reg.dta" if urban==0, clear 
drop Dagecat6 Dagecat6h Dagecat6t Dagecat6ht Dhhsizecat6 Dhhsizecat6h Dhhsizecat6t Dhhsizecat6ht
gen lnwelf = ln(welfareintr)

unab model1_rur : `model1_rur' 
local round=1
local maxvif=10 


while `maxvif'>=10 { 
	regress lnwelf `model`round'_rur' [aw=qhweight], vce(cluster psu) 
	estimates save ../estimates/districtmeans/replicate/model`round'_rur, replace  
	estat vif 
	local var_remove_pos=1
	local thisvar=r(name_`var_remove_pos') 
	while "`thisvar'"=="t" { 
		local var_remove_pos=`var_remove_pos'+1 
		local thisvar=r(name_`var_remove_pos') 
		} 	
	local maxvif=r(vif_`var_remove_pos')  	
	local var_remove_name=r(name_`var_remove_pos') 	
	local round=`round'+1 
	local lastround=`round'-1
	local model`round'_rur : subinstr local model`lastround'_rur "`var_remove_name'" "" 		
	} 
 	
dis "final model is round `round'" 
 
 
** Urban model 
 local model1_urb "$candidate_xvars_urb" 
 local interactions_urb "`model1_urb'" 
local interactions_urb : subinstr local interactions_urb "rainfallQ?" ""
local interactions_urb : subinstr local interactions_urb "rainfallQ?sq" ""

 
 foreach var in `interactions_urb' {
		local model1_urb "`model1_urb' `var't" 
		} 
local model1_urb "`model1_urb' t" 
dis "`model1_urb'" 

use "$data\finaldata\hh_reg.dta" if urban==1, clear 
drop Dagecat6 Dagecat6h Dagecat6t Dagecat6ht Dhhsizecat6 Dhhsizecat6h Dhhsizecat6t Dhhsizecat6ht
gen lnwelf = ln(welfareintr)
unab model1_urb : `model1_urb' 
local round=1
local maxvif=10 


while `maxvif'>=10 { 
	regress lnwelf `model`round'_urb' [aw=qhweight], vce(cluster psu) 
	estimates save ../estimates/districtmeans/replicate/model`round'_urb, replace  
	estat vif 
	local var_remove_pos=1
	local thisvar=r(name_`var_remove_pos') 
	while "`thisvar'"=="t" { 
		local var_remove_pos=`var_remove_pos'+1 
		local thisvar=r(name_`var_remove_pos')
		} 	
	local maxvif=r(vif_`var_remove_pos')  	
	local var_remove_name=r(name_`var_remove_pos') 	
	local round=`round'+1 
	local lastround=`round'-1
	local model`round'_urb : subinstr local model`lastround'_urb "`var_remove_name'" "" 		
	} 
 	
dis "final model is round `round'" 


*/
** urban model 

exit 










********************************************************************************
************* DIFFERENT MODELS: Tested with the 2011.5 Data ********************
********************************************************************************

reg lnwelf `xvar`i'11t' [pw=qhweight], vce(cluster psu)
estimates store est`i'11
estat vif
estadd vif
mat v`i'=e(vif)'
svmat2 v`i', rnames(label)
keep v`i'1 label
drop if v`i'1==.
order label v`i'1
gsort - v`i'1
egen max=max(v`i'1)
egen median=median(v`i'1)
egen min =min(v`i'1)
egen mean =mean(v`i'1)
drop if label== "_cons"
save "v`i'.dta", replace
}

forvalues i=0/1{
use v`i'.dta, clear
keep if label=="t"
keep label max min mean
order label mean max 
replace label="`i'"
save v`i'_1.dta, replace
}

use v1_1.dta
append using v0_1.dta
save vifs.dta, replace

* Creating a Table with the Mean and Max VIFs
mkmat mean max, mat(vif)
mat rownames vif = "Urban Model"  "Rural Model"
esttab matrix (vif, fmt("%9.2f")) using "$path/tables/vif.tex", /// 
nonumbers nomtitles nodepvars align(rr) style(tab) ///
collabel(none) modelwidth(10) ///
posthead(& {\textbf{Mean}} & {\textbf{Max}} \\ "\hline") prefoot("\hline") replace


*** Creating the VIFs tables ***
* Rural *
use v0.dta, clear
keep if v01>median
local n1=_N
foreach i of numlist 1/`n1'{
local names01=label[`i']
local rows01 "`rows01' `names01'"
}


mkmat v01, mat(vif0_1)
mat rownames vif0_1 = `rows01'

esttab matrix (vif0_1, fmt("%9.2f")) using "$path/tables/vif0_1.tex", /// 
nonumbers nomtitles nodepvars align(r) style(tab) ///
collabel(none) modelwidth(10) ///
posthead(& {\textbf{VIF}} \\ "\hline") prefoot("\hline") replace

use v0.dta, clear
keep if v01<=median
local n2=_N
foreach i of numlist 1/`n2'{
local names02=label[`i']
local rows02 "`rows02' `names02'"
}


mkmat v01, mat(vif0_2)
mat rownames vif0_2 = `rows02'
mat list vif0_2
esttab matrix (vif0_2, fmt("%9.2f")) using "$path/tables/vif0_2.tex", /// 
nonumbers nomtitles nodepvars align(r) style(tab) ///
collabel(none) modelwidth(10) ///
posthead(& \multicolumn{1}{c}{\textbf{VIF}} \\ "\hline") prefoot("\hline") replace

* Urban *
use v1.dta, clear
keep if v11>median
local n1=_N
foreach i of numlist 1/`n1'{
local names11=label[`i']
local rows11 "`rows11' `names11'"
}

mkmat v11, mat(vif1_1)
mat rownames vif1_1 =`rows11'

esttab matrix (vif1_1, fmt("%9.2f")) using "$path/tables/vif1_1.tex", /// 
nonumbers nomtitles nodepvars align(r) style(tab) ///
collabel(none) modelwidth(10) ///
posthead(& {\textbf{VIF}} \\ "\hline") prefoot("\hline") replace

use v1.dta, clear
keep if v11<=median
local n2=_N
foreach i of numlist 1/`n2'{
local names12=label[`i']
local rows12 "`rows12' `names12'"
}

mkmat v11, mat(vif1_2)
mat rownames vif1_2 = `rows12'
mat list vif1_2
esttab matrix (vif1_2, fmt("%9.2f")) using "$path/tables/vif1_2.tex", /// 
nonumbers nomtitles nodepvars align(r) style(tab) ///
collabel(none) modelwidth(10) ///
posthead(& \multicolumn{1}{c}{\textbf{VIF}} \\ "\hline") prefoot("\hline") replace

ddd

estout  est03 est04 est05 est06  est08 est09 est07 est010 using "$tables/modelsrural1.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (hhsizeh depratioh  selfemployedh  hinduh  scsth agrih  indh  highskillocch middleskillocch ) ///
order(hhsizeh depratioh selfemployedh  hinduh  scsth  agrih  indh  highskillocch middleskillocch) ///
varlabels(_cons "constant" agehh "Age of HH head" hhsizeh "Household Size" depratioh "Dependency Ratio"  selfemployedh "HH type: Self Employed"  hinduh "Hindu" scsth "Low Caste"  agrih "Princip Ind: Agri" indh "Princip Ind: Industry" highskillocch "High Skill Occupation" middleskillocch "Middle Skill Occupation") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  est03 est04 est05 est06  est08 est09 est07 est010 using "$tables/modelsrural2.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (transprh  recrh  hhrh  rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4 ) ///
order (rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4 transprh  recrh  hhrh  ) ///
varlabels(transprh "Transport services expenses" recrh "Recreation services expenses" hhrh "Household services expenses" rainfallQ1 "Rainfall Q1" rainfallQ2 "Rainfall Q2" rainfallQ3 "Rainfall Q3" rainfallQ4 "Rainfall Q4") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  est03 est04 est05 est06  est08 est09 est07 est010 using "$tables/modelsrural3.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (transprpch  recrpch  hhrpch transprxh  recrxh  hhrxh  transprlnh   hhrlnh ) ///
order (transprpch  recrpch  hhrpch transprxh  recrxh  hhrxh  transprlnh   hhrlnh) ///
varlabels(transprlnh "Log Transport services expenses"   hhrlnh "Log Household services expenses" transprxh "Transport services expenses Y/N)" recrxh "Recreation services expenses Y/N)" hhrxh "Household services expenses Y/N)" transprpch "Transport services expenses per capita" recrpch "Recreation services expenses per capita" hhrpch "Household services expenses per capita") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  est03 est04 est05 est06  est08 est09 est07 est010 using "$tables/modelsrural4.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transpr  recr  hhr ) ///
order (hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transpr  recr  hhr ) ///
varlabels(hhsize "Household Size (PSU)" depratio  "Dependency Ratio (PSU)"  selfemployed "HH type: Self Employed (PSU)"  hindu "Hindu (PSU)" scst "Low Caste (PSU)"  agri "Princip Ind: Agri (PSU)" ind "Princip Ind: Industry (PSU)" highskill "High Skill Occupation (PSU)" middleskill "Middle Skill Occupation(PSU)" transpr "Transport services expenses (PSU)" recr "Recreation services expenses (PSU)" hhr "Household services expenses (PSU)") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout  est03 est04 est05 est06  est08 est09 est07 est010 using "$tables/modelsrural5.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5  Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 ) ///
order (Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5  Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 ) ///
varlabels(Dagecat1 "Prop HH 0-15 yrs" Dagecat2 "Prop HH 16-24 yrs" Dagecat3 "Prop HH 25-34 yrs" Dagecat4 "Prop HH 35-49" Dagecat5 "Prop HH 50-64 yrs" Dhhsizecat2 "HH size 2" Dhhsizecat3 "HH size 3" Dhhsizecat4 "HH size 4" Dhhsizecat5 "HH size 5" Dhhsizecat6 "HH size 6") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	


estout  est13 est14 est15 est16  est18 est19 est17 est110 using "$tables/modelsurban1.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (hhsizeh depratioh  selfemployedh  hinduh  scsth agrih  indh  highskillocch middleskillocch ) ///
order(hhsizeh depratioh selfemployedh  hinduh  scsth  agrih  indh  highskillocch middleskillocch) ///
varlabels(_cons "constant" agehh "Age of HH head" hhsizeh "Household Size" depratioh "Dependency Ratio"  selfemployedh "HH type: Self Employed"  hinduh "Hindu" scsth "Low Caste"  agrih "Princip Ind: Agri" indh "Princip Ind: Industry" highskillocch "High Skill Occupation" middleskillocch "Middle Skill Occupation") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout   est13 est14 est15 est16  est18 est19 est17 est110 using "$tables/modelsurban2.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (transprh  recrh  hhrh  rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4 ) ///
order (rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4 transprh  recrh  hhrh  ) ///
varlabels(transprh "Transport services expenses" recrh "Recreation services expenses" hhrh "Household services expenses" rainfallQ1 "Rainfall Q1" rainfallQ2 "Rainfall Q2" rainfallQ3 "Rainfall Q3" rainfallQ4 "Rainfall Q4") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout   est13 est14 est15 est16  est18 est19 est17 est110 using "$tables/modelsurban3.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (transprpch  recrpch  hhrpch transprxh  recrxh  hhrxh  transprlnh   hhrlnh ) ///
order (transprpch  recrpch  hhrpch transprxh  recrxh  hhrxh  transprlnh   hhrlnh) ///
varlabels(transprlnh "Log Transport services expenses"   hhrlnh "Log Household services expenses" transprxh "Transport services expenses Y/N)" recrxh "Recreation services expenses Y/N)" hhrxh "Household services expenses Y/N)" transprpch "Transport services expenses per capita" recrpch "Recreation services expenses per capita" hhrpch "Household services expenses per capita") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout   est13 est14 est15 est16  est18 est19 est17 est110 using "$tables/modelsurban4.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transpr  recr  hhr ) ///
order (hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transpr  recr  hhr ) ///
varlabels(hhsize "Household Size (PSU)" depratio  "Dependency Ratio (PSU)"  selfemployed "HH type: Self Employed (PSU)"  hindu "Hindu (PSU)" scst "Low Caste (PSU)"  agri "Princip Ind: Agri (PSU)" ind "Princip Ind: Industry (PSU)" highskill "High Skill Occupation (PSU)" middleskill "Middle Skill Occupation(PSU)" transpr "Transport services expenses (PSU)" recr "Recreation services expenses (PSU)" hhr "Household services expenses (PSU)") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	

estout   est13 est14 est15 est16  est18 est19 est17 est110 using "$tables/modelsurban5.tex", ///
cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N F, fmt(%9.2f %9.0f %9.0f) labels("\$R^2\$" "N" "F-Stat" )) collabels(none) style(tex) starlevels(* .10 ** .05 *** .01) mlabels(none) ///
keep (Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5  Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 ) ///
order (Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5  Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 ) ///
varlabels(Dagecat1 "Prop HH 0-15 yrs" Dagecat2 "Prop HH 16-24 yrs" Dagecat3 "Prop HH 25-34 yrs" Dagecat4 "Prop HH 35-49" Dagecat5 "Prop HH 50-64 yrs" Dhhsizecat2 "HH size 2" Dhhsizecat3 "HH size 3" Dhhsizecat4 "HH size 4" Dhhsizecat5 "HH size 5" Dhhsizecat6 "HH size 6") ///
prehead("\resizebox{.7\textheight}{!}{" "\begin{tabular}{{l}*{@M}{c}}" "\hline") posthead(& \textbf{Baseline} & \textbf{PC Exp} & \textbf{Y/N Exp} & \textbf{LnExp}  & \textbf{AgeCat} & \textbf{HHCat} & \textbf{PSUMeans} & \textbf{Final} \\ "\hline" \\) prefoot(\hline)  postfoot("\hline" "\end{tabular}" "}") replace	


