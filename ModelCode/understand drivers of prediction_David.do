*fversion 15 
clear 
matrix drop _all 
cap log close
cap log using understanding_drivers, text replace 

* locals for directories and variables

local datain "$data\finaldata\sae14.dta"
local datainu "$data\finaldata\sae14u.dta"
local datainr "$data\finaldata\sae14r.dta"

local dataout "$analysis\SAE_MATA"
local dataoutu "$analysis\SAE_MATAu"
local dataoutr "$analysis\SAE_MATAr"

local ydump "$analysis\mata"
local ydumpu "$analysis\matau"
local ydumpr "$analysis\matar"

glo res "$analysis\SAE_results"


********************************************************************************
************************************ MODEL ************************************
********************************************************************************


use "$data\finaldata\hh_reg.dta", replace
keep $xvar011t  $xvars111t $xvar111t pop_wgt welfareintr urban year poor_int ID 

unab xvar011t : $xvar011t 
unab xvar111t : $xvar111t 



_rmcoll `xvar011t', forcedrop
local xvar011t=r(varlist) 
_rmcoll `xvar111t', forcedrop
local xvar111t=r(varlist) 
gen lnwelf = ln(welfareintr)

local xvar011t : subinstr local xvar011t "rainfallQ3 rainfallQ4 rainfallQ1 rainfallQ2 rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq" "rainfallQ1 rainfallQ1sq rainfallQ2 rainfallQ2sq rainfallQ3 rainfallQ3sq rainfallQ4 rainfallQ4sq" 
local xvar111t : subinstr local xvar111t "rainfallQ3 rainfallQ4 rainfallQ1 rainfallQ2 rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq" "rainfallQ1 rainfallQ1sq rainfallQ2 rainfallQ2sq rainfallQ3 rainfallQ3sq rainfallQ4 rainfallQ4sq" 
gen year2=int(year) if year>=2011  


save "$data\finaldata\sae0411.dta", replace
*/ 
* Urban
use "$data\finaldata\sae0411.dta", clear
keep if urban==1

gen subpop=(year>=2011) 



summ poor_int if year==2011.5 [aw=pop_wgt]
local pov_2011=r(mean) 
dis "$xvar111t" 

** order regression vaiables appropriately 
preserve 
foreach var of varlist `xvar111t' { 
	if substr("`var'",-1,.)=="h" {
		local newvar = substr("`var'",1,strlen("`var'")-1)+"H" 
		}
		else {
		local newvar "`var'D"
		}
	if substr("`var'",-2,.)=="ht" {
		local newvar = substr("`var'",1,strlen("`var'")-2)+"HT" 
		}
	else if substr("`var'",-1,.)=="t" {
			local newvar = substr("`var'",1,strlen("`var'")-1)+"DT" 
	}
	
		rename `var' `newvar'
		local xvar111t_reg "`xvar111t_reg' `newvar'"
}
	
rename ID id 



* regress poor_int `xvar111t' [aw=pop_wgt] 
keep lnwelf `xvar111t_reg'  pop_wgt urban year2
order casualurbanDT middleskilloccDT recrDT
regress lnwelf `xvar111t_reg' [pw=pop_wgt] 


estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*D *H _cons DT) order(*D *H _cons DT) stats(r2 N, fmt(%9.3f %9.0fc)) 
matrix col1_urban=r(coefs) \ r(stats) 
matrix list col1_urban 
estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*DT *HT) drop(DT) order(*DT *HT) 
matrix col2_urban=r(coefs) 

* means 
mean `xvar111t_reg' if urban==1 [aw=pop_wgt], over(year2)  
estimates save "${estimates}/meanvars_urb", replace 

estout ,keep(*D:2011)
matrix means_D_urb=r(coefs) 
estout ,keep(*D:2014)
matrix means_D_urb=means_D_urb,r(coefs)
estout ,keep(*H:2011)
matrix means_H_urb=r(coefs)
estout ,keep(*H:2014)
matrix means_H_urb=means_H_urb,r(coefs)

matrix colnames means_D_urb = "mean_D_2011 mean_D_2014"
matrix colnames means_H_urb = "mean_H_2011 mean_H_2014"

matsave means_D_urb, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave means_H_urb, replace saving path(D:\Poverty_Estimates_India\estimates) 


restore 



regress lnwelf `xvar111t' [pw=pop_wgt] 
estimates store urban
predict yhat 
table year [aw=pop_wgt], c(m yhat) 

* "table year [aw=pop_wgt], c(m poor_int) 

* margins, subpop(subpop) noesample 

/* 
cap matrix drop marginal_effects_urban
foreach vars in  "Dhhsizecat?h" "Dhhsizecat?" "Dagecat?h"  "Dagecat?" "hinduh" "hindu"   "scsth" "scst"  "regwageurbanh selfemployedh casualurbanh" "regwageurban selfemployed casualurban" ///
                 "highskillocch middleskillocch" "highskillocc middleskillocc" "hhrh"  "hhr" "recrh" "recr" "transprh"  "transpr"	"rainfallQ? rainfallQ?sq" "t" { 
local groupname "`vars'" 
unab vars : `vars' 
local at "" 
foreach var of varlist `vars' { 
qui summ `var' if year==2014.5 
local value_2014=r(mean) 
local at "`at' `var'=`r(mean)' "  
} 
margins, over(year2) at(`at') noesample subpop(if year==2011.5) nose 
matrix marginal_effects_urban = nullmat(marginal_effects_urban) \ r(b) 
}
matrix marginal_effects_urban=marginal_effects_urban-J(rowsof(marginal_effects_urban),1,`pov_2011')
matrix marginal_effects_urban=marginal_effects_urban \ J(1,rowsof(marginal_effects_urban),1)*marginal_effects_urban
matrix rownames marginal_effects_urban =  "HH size" "Dist HH size"  "Head age" "Dist Head age"  "Hindu" "Dist % Hindu" "Sched" "Dist % Sched"  "Employment type" "Dist Employment type"  /// 
   "HH Occ" "Dist Occ"  "HH service exp" "Dist HH service exp"  "HH rec exp" "Dist hh rec exp"  "Transport exp" "District Transport exp"  "Rainfall" "Time trend" "Total"  
 


margins, over(year2) at(t=10) subpop(subpop)  
matrix marginal_effects_timetrend_urban=r(b)-`pov_2011'

*/ 


* replace yhat=poor_int if year<=2011.5 
dis "`xvar111t'"



local oax_specu "(hhsizeD: Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5)"
local oax_specu "`oax_specu' (hhsizeH: Dhhsizecat2h Dhhsizecat3h Dhhsizecat4h Dhhsizecat5h Dhhsizecat2ht Dhhsizecat3ht Dhhsizecat4ht Dhhsizecat5ht)" 
local oax_specu "`oax_specu'  (agecatD: Dagecat2 Dagecat3 Dagecat4 Dagecat5 )" 
local oax_specu "`oax_specu'  (agecatH: Dagecat1h Dagecat2h Dagecat3h Dagecat4h Dagecat5h Dagecat2ht Dagecat3ht Dagecat5ht)" 
* local oax_specu "`oax_specu'  (HinduD: hindu) (HinduH: hinduh hinduht) (SchedD: scst) (SchedH: scsth scstht)" 
local oax_specu "`oax_specu'  (HinduD: hindu) (HinduH: hinduh hinduht) (SchedH: scsth scstht)" 
local oax_specu "`oax_specu'  (jobtypeD: regwageurban selfemployed casualurban  casualurbant)" 
local oax_specu "`oax_specu'  (jobtypeH: regwageurbanh selfemployedh casualurbanh regwageurbanht casualurbanht)" 
local oax_specu "`oax_specu'  (OccD: highskillocc middleskillocc middleskillocct)" 
local oax_specu "`oax_specu'  (OccH: highskillocch  middleskillocch highskilloccht middleskilloccht)" 
local oax_specu "`oax_specu'  (SectorD: ind)" 
local oax_specu "`oax_specu'  (SectorH: agrih indh agriht indht)" 
local oax_specu "`oax_specu'  (Dservice_exp: hhr)" 
local oax_specu "`oax_specu'  (Hservice_exp: hhrh hhrht)"
local oax_specu "`oax_specu'  (Drec_exp: recr recrt) (Hrec_exp: recrh recrht) "
local oax_specu "`oax_specu'  (Dtrans_exp: transpr) (Htrans_exp: transprh transprht) "
* local oax_specu "`oax_specu'  (Drainfall: rainfallQ3 rainfallQ4 rainfallQ1 rainfallQ2 rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq)"
local oax_specu "`oax_specu'  (Drainfall: rainfallQ3 rainfallQ4 rainfallQ1 rainfallQ2 rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq)"
local oax_specu "`oax_specu'   t"
replace year2=. if year<2011 
set trace off


oaxaca yhat `oax_specu' [pw=pop_wgt], by(year2) swap reference(urban) relax nose 
estimates save "${estimates}/oaxaca_urb", replace 

matrix b=e(b)
matrix resultsu=b[1,"explained:"]
matrix resultsu=resultsu,b[1,"overall:difference"]
matrix resultsu=resultsu,b[1,"overall:group_2"] 
matrix resultsu=resultsu,b[1,"overall:group_1"] 
matrix resultsu=resultsu*100 

matrix resultsu=resultsu'
matrix list resultsu 
drop yhat 




*/
 


*/
* Rural
use "$data\finaldata\sae0411.dta", clear
keep if urban==0
gen subpop=(year>=2011) 
summ poor_int if year==2011.5 [aw=pop_wgt]
local pov_2011=r(mean) 



preserve 
foreach var of varlist `xvar011t' { 
	if substr("`var'",-1,.)=="h" {
		local newvar = substr("`var'",1,strlen("`var'")-1)+"H" 
		}
		else {
		local newvar "`var'D"
		}
	if substr("`var'",-2,.)=="ht" {
		local newvar = substr("`var'",1,strlen("`var'")-2)+"HT" 
		}
	else if substr("`var'",-1,.)=="t" {
			local newvar = substr("`var'",1,strlen("`var'")-1)+"DT" 
	}
	
		rename `var' `newvar'
		local xvar011t_reg "`xvar011t_reg' `newvar'"
}
	
rename ID id 


* regress poor_int `xvar111t' [aw=pop_wgt] 
regress lnwelf `xvar011t_reg' [pw=pop_wgt] 

matrix drivers_reg_rural=e(b)' 

keep lnwelf `xvar011t_reg' pop_wgt urban year2 
order rainfallQ1D rainfallQ1sqD rainfallQ2D rainfallQ2sqD rainfallQ3D rainfallQ3sqD rainfallQ4D rainfallQ4sqD, last 
order casualurbanDT middleskilloccDT recrDT

* outreg, stats(b) keep(*D) varlabels rtitles("Household size 1 or 2") blankrows nocenter

estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*D *H _cons DT) order(*D *H _cons DT) stats(r2 N, fmt(%9.3f %9.0fc)) 
matrix col1_rural=r(coefs) \ r(stats)

estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*DT *HT) drop(DT) order(*DT *HT) 
matrix col2_rural=r(coefs) 
* means 
mean `xvar011t_reg' if urban==0 [aw=pop_wgt], over(year2)  
estimates save "${estimates}/meanvars_rur", replace 

estout ,keep(*D:2011)
matrix means_D_rur=r(coefs) 
estout ,keep(*D:2014)
matrix means_D_rur=means_D_rur,r(coefs)
estout ,keep(*H:2011)
matrix means_H_rur=r(coefs)
estout ,keep(*H:2014)
matrix means_H_rur=means_H_rur,r(coefs)

matrix colnames means_D_rur = "mean_D_2011 mean_D_2014"
matrix colnames means_H_rur = "mean_H_2011 mean_H_2014"

matsave means_D_rur, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave means_H_rur, replace saving path(D:\Poverty_Estimates_India\estimates)



restore 
regress lnwelf `xvar011t' [pw=pop_wgt] 
predict yhat 
estimates store rural 

 




gen D2014=(year==2014.5)




/*
cap matrix drop marginal_effects_rural
foreach vars in "Dhhsizecat?h" "Dhhsizecat?" "Dagecat?h" "Dagecat?" "hinduh"  "hindu" "scsth"  "scst" "selfemployedh" "selfemployed casualurban regwageurban"  ///
                 "highskillocch middleskillocch" "highskillocc middleskillocc"  "hhrh"  "hhr" "recrh" "recr"  "transprh" "transpr"	 "rainfallQ? rainfallQ?sq" "t" { 
local groupname "`vars'" 
unab vars : `vars' 
local at "" 
foreach var of varlist `vars' { 
qui summ `var' if year==2014.5 
local value_2014=r(mean) 
local at "`at' `var'=`r(mean)' "  
} 
margins, over(year2) at(`at') noesample subpop(if year==2011.5) nose 
matrix marginal_effects_rural = nullmat(marginal_effects_rural) \ r(b) 
}
matrix marginal_effects_rural=marginal_effects_rural-J(rowsof(marginal_effects_rural),1,`pov_2011')
matrix marginal_effects_rural=marginal_effects_rural \ J(1,rowsof(marginal_effects_rural),1)*marginal_effects_rural
matrix rownames marginal_effects_rural = "HH size" "Dist HH size" "Head age" "Dist Head age" "Hindu"  "Dist % Hindu"   "Sched" "Dist % Sched" "Employment type" "Dist Employment type"  /// 
    "HH Occ" "Dist Occ" "HH service exp" "Dist HH service exp"  "HH rec exp" "Dist hh rec exp"   "Transport exp" "District Transport exp" "Rainfall" "Time trend" "Total"  
 
*/

dis "`xvar011t'"

local oax_specr "(hhsizeD: Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5)"
local oax_specr "`oax_specr' (hhsizeH: Dhhsizecat2h Dhhsizecat3h Dhhsizecat4h Dhhsizecat5h Dhhsizecat2ht Dhhsizecat3ht Dhhsizecat4ht Dhhsizecat5ht)" 
local oax_specr "`oax_specr'  (agecatD: Dagecat2 Dagecat3 Dagecat4 Dagecat5 )" 
local oax_specr "`oax_specr'  (agecatH: Dagecat1h Dagecat2h Dagecat3h Dagecat4h Dagecat5h Dagecat2ht Dagecat3ht Dagecat4ht Dagecat5ht)" 
* local oax_specr "`oax_specr'  (HinduD: hindu) (HinduH: hinduh hinduht) (SchedD: scst) (SchedH: scsth scstht)" 
local oax_specr "`oax_specr'  (HinduD: hindu) (HinduH: hinduh hinduht) (SchedH: scsth scstht)" 
local oax_specr "`oax_specr'  (jobtypeD: regwageurban selfemployed casualurban casualurbant regwageurbant)" 
local oax_specr "`oax_specr'  (jobtypeH: selfemployedh selfemployedht)" 
local oax_specr "`oax_specr'  (OccD: highskillocc middleskillocc middleskillocct)" 
local oax_specr "`oax_specr'  (OccH: highskillocch  middleskillocch highskilloccht middleskilloccht )" 
local oax_specr "`oax_specr'  (SectorD: ind)" 
local oax_specr "`oax_specr'  (SectorH: agrih indh indht)" 
local oax_specr "`oax_specr'  (Dservice_exp: hhr)" 
local oax_specr "`oax_specr'  (Hservice_exp: hhrh hhrht)"
local oax_specr "`oax_specr'  (Drec_exp: recr recrt) (Hrec_exp: recrh recrht) "
local oax_specr "`oax_specr'  (Dtrans_exp: transpr) (Htrans_exp: transprh  transprht) "
local oax_specr "`oax_specr'  (Drainfall: rainfallQ3 rainfallQ4 rainfallQ1 rainfallQ2 rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq)"
local oax_specr "`oax_specr'   t"

oaxaca yhat `oax_specr' [aw=pop_wgt], by(year2) swap reference(rural) relax nose
estimates save "${estimates}/oaxaca_rur", replace 
ereturn list 

matrix b=e(b)
matrix resultsr=b[1,"explained:"]
matrix resultsr=resultsr,b[1,"overall:difference"]
matrix resultsr=resultsr,b[1,"overall:group_2"] 
matrix resultsr=resultsr,b[1,"overall:group_1"] 
matrix resultsr=resultsr*100 

matrix resultsr=resultsr'
matrix list resultsr 
drop yhat 

matrix results=resultsu,resultsr
* matrix results=results*100
matrix colnames results="Urban Rural" 

/*
putdocx begin 
putdocx table drivers = matrix(results), nformat(%02.1f) rownames 
putdocx save "D:\Poverty_Estimates_India\tables/drivers_table", replace 
esttab matrix(results, fmt(%03.1f)) using "D:\Poverty_Estimates_India\tables/drivers_table", replace
matrix drivers=results 
matsave drivers, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave col1_urban, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave col2_urban, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave col1_rural, replace saving path(D:\Poverty_Estimates_India\estimates) 
matsave col2_rural, replace saving path(D:\Poverty_Estimates_India\estimates) 
*/

mat rownames results = "HH Size (Dist)" "HH Size" "Age Category (Dist)" ///
"Age Category" "Hindu (Dist)" "Hindu" "Low Caste" "HH Type (Dist)" "HH Type" ///
"Occupation (Dist)" "Occupation" "Sector (Dist)" "Sector" ///
"Expd on Services (Dist)" "Expd on Services" ///
"Expd on Recreation (Dist)" "Expd on Recreation" /// 
"Expd on Transportation (Dist)" "Expd on Transportation" ///
"Rainfall" "Time Trend" "Difference" "Mean Log Welfare (2011-12)*100" "Mean Pred Log Welfare (2014-15)*100" 
esttab matrix (results, fmt(%9.1fc)) using "$tables/drivers.tex", /// 
nonumbers nomtitles nodepvars align(rr) style(tab) ///
collabel(none) modelwidth(10) ///
varlabels(hinduh "Hindu" scsth "Low caste" selfemployedh "HH type: Self Employed" regwageurbanh "HH type: Regular Wage Worker" casualurbanh "HH type: Casual Laborer" agrih "Princip Ind: Agri" indh "Princip Ind: Industry" highskillocch "High Skill Occupation" middleskillocch "Middle Skill Occupation" transprh "Transport services expenses" recrh "Recreation services expenses" hhrh "Household services expenses") ///
addnotes("Sources: India National Sample Survey Office (NSSO) Surveys.") ///
posthead(& \multicolumn{1}{c}{\textbf{Urban}} & \multicolumn{1}{c}{\textbf{Rural}}\\ "\hline") replace


cap log close 



