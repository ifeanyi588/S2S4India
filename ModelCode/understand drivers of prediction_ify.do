*fversion 15 
clear 
matrix drop _all 
cap log close
cap log using understanding_drivers, text replace 

*** To modify these files I create a folder to take new outputs


glo path "D:\Poverty_Estimates_India"
glo pathout "D:\Poverty_Estimates_India\IndiaModelRuns"

*Change next path to your current directory
* 61 = 2004/05 
* 66 = 2009/10 
* 68 = 2011/12
* 72 = 2014/15 

    glo data  "$path\data\replication" //contains input data
	glo data2  "$pathout\data\replication" //contains input data
	glo data61  "$data\NSSO61" //these next 4 are input datasets
	glo data66  "$data\NSSO66" 
	glo data68  "$data\NSSO68"
	glo data72  "$data\NSSO72"
	
	
	glo analysis  "$pathout\analysis" //all these are output datasets, the result of "analysis"
	glo analysis61  "$analysis\NSSO61"
    glo analysis66  "$analysis\NSSO66"
	glo analysis68  "$analysis\NSSO68"
	glo analysis72  "$analysis\NSSO72"
	glo analysislasso  "$pathout\analysis\lasso"

	glo desc_stats "$analysis\desc_stats" 
	
	glo graphs "$pathout\graphs"  //output graphs
	glo tables "$pathout\tables"  //output tables
	glo resleasso "$analysislasso\lasso\SAE_results" //output lasso results

* locals for directories and variables

local datain "$data2\sae18.dta"
local datainu "$data2\sae18u.dta"
local datainr "$data2\sae18r.dta"

local dataout "$analysis\SAE_MATA_oaxaca"
local dataoutu "$analysis\SAE_MATAu_oaxaca"
local dataoutr "$analysis\SAE_MATAr_oaxaca"

local ydump "$analysis\mata_oaxaca"
local ydumpu "$analysis\matau_oaxaca"
local ydumpr "$analysis\matar_oaxaca"

glo res "$analysis\SAE_results_oaxaca"


********************************************************************************
************************************ MODEL *************************************
********************************************************************************


use "$path\data\finaldata\newmaster.dta", replace

global urbanvars "Dagecat1 Dagecat1h Dagecat2 Dagecat3 Dagecat3h Dagecat4 Dagecat4h"
global urbanvars "${urbanvars} Dcharcoal Dcokecoal Dcokecoalh Ddungcake Ddungcakeh Delectricityh"
global urbanvars "${urbanvars} Dfirewood Dfirewoodh Dkerosene Dkeroseneh Dlpg Dlpgh Dmaleh Dnocooking Dnocookingh Dagri Dagrih"
global urbanvars "${urbanvars} depratioh hhsize hhsizeh highskillocc highskillocch indh middleskillocc middleskillocch"
global urbanvars "${urbanvars} rainfallQ1 rainfallQ1sq rainfallQ2 rainfallQ2sq rainfallQ3 rainfallQ3sq rainfallQ4"
global urbanvars "${urbanvars} scst scsth selfemployed selfemployedh"

global ruralvars "Dagecat1 Dagecat1h Dagecat2 Dagecat3 Dagecat3h Dagecat4 Dcharcoal Dcokecoalh Ddungcake Ddungcakeh"
global ruralvars "${ruralvars} Delectricityh Dfirewood Dfirewoodh Dkerosene Dkeroseneh Dlpg Dlpgh Dmaleh Dnocooking"
global ruralvars "${ruralvars} Dnocookingh agri agrih depratioh hhsize hhsizeh highskillocc highskillocch indh middleskillocc middleskillocch"
global ruralvars "${ruralvars} rainfallQ1 rainfallsq rainfallQ2 rainfallQ2sq rainfallQ3sq rainfallQ4 scst scsth selfemployed selfemployedh"

save "$data2\sae12.dta", replace

* Urban
use "$data2\sae12.dta", clear
keep if urban==1

gen subpop=(year>=2011) 

exit

summ poor_int if year==2011.5 [aw=pop_wgt]
local pov_2011=r(mean) 
dis "$urbanvars2" 

** order regression vaiables appropriately 
preserve 
foreach var of varlist `urbanvars2' { 
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
		local urbanvars2_reg "`urbanvars2_reg' `newvar'"
}
	
rename ID id 



* regress poor_int `urbanvars2' [aw=pop_wgt] 
keep lnwelf `urbanvars2_reg'  pop_wgt urban year2
*order casualurbanDT middleskilloccDT recrDT
regress lnwelf `urbanvars2_reg' [pw=pop_wgt] 


estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*D *H _cons DT) order(*D *H _cons DT) stats(r2 N, fmt(%9.3f %9.0fc)) 
matrix col1_urban=r(coefs) \ r(stats) 
matrix list col1_urban 
estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*DT *HT) drop(DT) order(*DT *HT) 
matrix col2_urban=r(coefs) 

* means 
mean `urbanvars2_reg' if urban==1 [aw=pop_wgt], over(year2)  
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



regress lnwelf `urbanvars2' [pw=pop_wgt] 
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
dis "`urbanvars2'"






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
local oax_specu "`oax_specu'  (Dtrans_exp: transp2r transp2rt) (Htrans_exp: transp2rh transp2rht) "
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
foreach var of varlist `ruralvars2' { 
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
		local ruralvars2_reg "`ruralvars2_reg' `newvar'"
}
	
rename ID id 


* regress poor_int `urbanvars2' [aw=pop_wgt] 
regress lnwelf `ruralvars2_reg' [pw=pop_wgt] 

matrix drivers_reg_rural=e(b)' 

keep lnwelf `ruralvars2_reg' pop_wgt urban year2 
order rainfallQ1D rainfallQ1sqD rainfallQ2D rainfallQ2sqD rainfallQ3D rainfallQ3sqD rainfallQ4D rainfallQ4sqD, last 
order casualurbanDT middleskilloccDT recrDT

* outreg, stats(b) keep(*D) varlabels rtitles("Household size 1 or 2") blankrows nocenter

estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*D *H _cons DT) order(*D *H _cons DT) stats(r2 N, fmt(%9.3f %9.0fc)) 
matrix col1_rural=r(coefs) \ r(stats)

estout, label varwidth(50) cells(b(fmt(%9.2f))) keep(*DT *HT) drop(DT) order(*DT *HT) 
matrix col2_rural=r(coefs) 
* means 
mean `ruralvars2_reg' if urban==0 [aw=pop_wgt], over(year2)  
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
regress lnwelf `ruralvars2' [pw=pop_wgt] 
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

dis "`ruralvars2'"

/*Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat2h Dhhsizecat3h Dhhsizecat4h Dhhsi
> zecat5h Dagecat2 Dagecat3 Dagecat4 Dagecat5 Dagecat1h Dagecat2h Dagecat3h Dagecat4h Dageca
> t5h hindu hinduh scsth selfemployed selfemployedh casualurban regwageurban middleskillocc 
> middleskillocch highskillocc highskillocch agrih ind indh hhr hhrh recr recrh transp2r tra
> nsp2rh rainfallQ1 rainfallQ1sq rainfallQ2 rainfallQ2sq rainfallQ3 rainfallQ3sq rainfallQ4 
> rainfallQ4sq Dhhsizecat2ht Dhhsizecat3ht Dhhsizecat4ht Dhhsizecat5ht Dagecat2ht Dagecat3ht
>  Dagecat4ht Dagecat5ht hinduht scstht selfemployedht casualurbant regwageurbant middleskil
> locct middleskilloccht highskilloccht indht hhrht recrt recrht transp2rt transp2rht t
*/


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
local oax_specr "`oax_specr'  (Dtrans_exp: transp2r transp2rt) (Htrans_exp: transp2rh  transp2rht) "
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
*/
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



