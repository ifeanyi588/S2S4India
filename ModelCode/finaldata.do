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
** MODIFIED BY David newhouse
** Modified	 03/18
                                                                                             
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
use "$path\data\replication\NSSO`y'\nss`y'_3.dta"

set trace off
set traced 1


** create detailed household size dummies 
recode hhsize (1/2=2 "1 or 2") (3=3 "3") (4=4 "4") (5=5 "5") (6/max=6 "6+"), gen (hhsize_cat) 
foreach val of numlist 2/6 { 
gen Dhhsizecat`val'=(hhsize_cat==`val') 
} 

* Dependency Ratio 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_depratio.dta"
drop _merge

* Age categories 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_age_cat.dta"
drop _merge

* Amount spent on miscellaneous services
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_10.dta"
drop _merge
sort year
replace year=year[_n-1] if year==.




* MPCE 
merge 1:1 ID using "$path\data\NSSO`y'\nss`y'_3mpce.dta"
drop _merge

** merge harmonized district codes for 61st round, putting merge results in dist_merge`round' 
preserve
use NSS`y'* NSS61dist_code NSS61state_id using "D:\Poverty_Estimates_India\data\district concordances from Sutirtha\NSS05_10_11_14_concordance_processed", replace 
bys NSS`y'dist_code NSS`y'state_id: keep if _n==1 
save "D:\Poverty_Estimates_India\data\district concordances from Sutirtha\NSS05_10_11_14_concordance_processed_`y'", replace 
restore 

gen NSS`y'dist_code=district 
gen NSS`y'state_id=state

dis "`y'" 
merge n:1 NSS`y'state_id NSS`y'dist_code  using "D:\Poverty_Estimates_India\data\district concordances from Sutirtha\NSS05_10_11_14_concordance_processed_`y'",  ///
	keepusing(NSS61state_id NSS61dist_code) keep(1 3) gen(dist_merge`y') 
if `y'>61 { 
	drop NSS`y'state_id NSS`y'dist_code 
} 

* Replacing the dependency ratio with hhsize as the denominator
replace depratio=sumdep/hhsize

* Creating Per Capita Expenditure variables; log of expenses; extensive margin variables on expd.
foreach v in transpr transp1r transp2r recr hhr communir {
gen `v'pc=`v'/hhsize
replace `v'pc=. if `v'pc==.
gen `v'x=(`v'>0)
replace `v'x=. if `v'==.
}

* Creating log of expenditures for transportation and household expenses. Recreation has too many zeros
foreach v in transpr transp1r transp2r recr hhr {
gen `v'ln=ln(`v')
}

* For 2004.5, there are districts in Delhi with codes=98 and 99. Changing them to 1 which is New Delhi. This is for rainfall. Since the geographic area is small, it shouldn't vary much across districts of Delhi. 
replace district=1 if state==7 & (district==98 | district==99) 

* Creating IDs for the SAE analysis

gen qhweight = hhwt*hhsize 
la var qhweight "population weight"

labdtch state
tostring state, replace
replace state= "0" + state if strlen(state)==1

tostring ID, replace

* Generating psus for years other than 2004 in which there is no psu variable. In 2004 the psu is the first 5 digits of the ID. 
gen qhclust=substr(ID,1,5)
replace qhclust=B1_v01 if year==2009

bys qhclust : gen hhcode=_n
tostring hhcode, gen (qhnum)
replace qhnum = "0" + qhnum if strlen(qhnum)==1
drop hhcode

labdtch sector
tostring sector, replace

tostring district, format(%-02.0f) replace 

gen id_sae_1="1"+sector+state+qhclust 
gen id_sae="4"+sector+state+qhclust+qhnum if year==2004
replace id_sae="5"+sector+state+qhclust+qhnum if year==2009
replace id_sae="6"+sector+state+qhclust+qhnum if year==2011
replace id_sae="7"+sector+state+qhclust+qhnum if year==2014


gen id_sae_2="1"+sector+state+district
gen id_sae_3="4"+sector+state+district if year==2004 
replace id_sae_3="5"+sector+state+district if year==2009 
replace id_sae_3="6"+sector+state+district if year==2011 
replace id_sae_3="7"+sector+state+district if year==2014 


la var id_sae "Unique HH ID"
la var id_sae_1 "Unique PSU ID"
la var id_sae_2 "Unique District ID"
la var id_sae_2 "Unique District-year ID"


destring id_sae id_sae_1 id_sae_2 id_sae_3 qhclust qhnum state district, replace

save "$path\data\replication\NSSO`y'\descdata`y'.dta", replace
}
exit 
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
/* rainfall by month and state 
merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2004* mean_rainfall_2005*) keep(1 3) 
tab _m  
drop _m 
rename mean_rainfall_2004* rainfall* 
rename mean_rainfall_2005* rainfall*
*/

merge n:1 state district using "$path\data\raindata\district_rain_std", keepusing(rainfallY_2004 rainfall_2004Q* rainfall_2005Q*)  
drop if _m==2
tab _m 
drop _m 
rename rainfallY_2004* rainfallY 
rename rainfall_2004* rainfall*
rename rainfall_2005* rainfall*


gen highskillocc=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskillocc = ((occup_e==4)| (occup_e==5))
gen lowskillocc=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskillocc "Bureaucrats, Professional and Technicians"
label var middleskillocc "Clerks and Service Related"
label var lowskillocc "Agri, Craft, Machine, Elementary"

* Calculating welfare in Real Terms (2011 Rs.)
merge m:1 year using "$path\data\CPI2011\cpi2011.dta"
keep if _merge==3
drop _merge
* gen welfarenatr = welfarenat/(cpiurban*ppp) if urban==1
* replace welfarenatr= welfarenat/(cpirural*ppp) if urban==0
* la var welfarenatr "Welfare aggregate for national poverty (2011 Rs.)"
gen welfareintr = welfare/(cpiurban*ppp) if urban==1
replace welfareintr= welfare/(cpirural*ppp) if urban==0
la var welfareintr "Welfare aggregate for international poverty (2011 Rs.)"

drop cpirural cpiurban 


save "$data61\finaldata61.dta", replace

/*datalibweb, coun(IND) y(2009) t(SARMD) sur(IND_2009_NSS66-SCH1.0-T1_v01_M_v03_A_SARMD) filen(IND_2009_NSS66-SCH1.0-T1_v01_M_v03_A_SARMD_IND.dta)
save "$data66\povertyline66.dta", replace
*/

use "$data66\povertyline66.dta", clear
rename idh ID 
*destring ID, replace
merge m:1 ID using "$data66\descdata66.dta"
drop _merge
duplicates drop ID, force
gen Nhh=1 
replace year=2009 if year==.
/* state rainfall 
merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2009* mean_rainfall_2010*) keep(1 3) 
tab _m  
drop _m 
rename mean_rainfall_2009* rainfall*
rename mean_rainfall_2010* rainfall*
*/

merge n:1 state district using "$path\data\raindata\district_rain_std", keepusing(rainfallY_2009 rainfall_2009Q* rainfall_2010Q*)    
drop if _m==2

tab _m 
drop _m 
rename rainfall_2009* rainfall*
rename rainfall_2010* rainfall*
rename rainfallY_2009* rainfallY

gen highskillocc=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskillocc = ((occup_e==4)| (occup_e==5))
gen lowskillocc=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskillocc "Bureaucrats, Professional and Technicians"
label var middleskillocc "Clerks and Service Related"
label var lowskillocc "Agri, Craft, Machine, Elementary"

* Calculating welfare in Real Terms (2011 Rs.)
merge m:1 year using "$path\data\CPI2011\cpi2011.dta"
keep if _merge==3
drop _merge
* gen welfarenatr = welfarenat/(cpiurban*ppp) if urban==1
* replace welfarenatr= welfarenat/(cpirural*ppp) if urban==0
* la var welfarenatr "Welfare aggregate for national poverty (2011 Rs.)"
gen welfareintr = welfare/(cpiurban*ppp) if urban==1
replace welfareintr= welfare/(cpirural*ppp) if urban==0
la var welfareintr "Welfare aggregate for international poverty (2011 Rs.)"

drop cpirural cpiurban 

save "$data66\finaldata66.dta", replace

/*datalibweb, coun(IND) y(2011) t(SARMD) sur(IND_2011_NSS68-SCH1.0-T1_v01_M_v03_A_SARMD) filen(IND_2011_NSS68-SCH1.0-T1_v01_M_v03_A_SARMD_IND.dta)
save "$data68\povertyline68.dta", replace
*/

use "$data68\povertyline68.dta", clear
rename idh ID
*destring ID, replace
merge m:1 ID using "$data68\descdata68.dta"
drop _merge
duplicates drop ID, force
gen Nhh=1 
replace year=2011 if year==.
/* state rainfall 
merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2011* mean_rainfall_2012*) keep(1 3) 
tab _m  
drop _m 
rename mean_rainfall_2011* rainfall*
rename mean_rainfall_2012* rainfall*
*/

merge n:1 state district using "$path\data\raindata\district_rain_std", keepusing(rainfallY_2011 rainfall_2011Q* rainfall_2012Q*)
drop if _m==2

tab _m 
drop _m 

rename rainfall_2011* rainfall*
rename rainfall_2012* rainfall*
rename rainfallY_2011* rainfallY

gen highskillocc=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskillocc = ((occup_e==4)| (occup_e==5))
gen lowskillocc=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskillocc "Bureaucrats, Professional and Technicians"
label var middleskillocc "Clerks and Service Related"
label var lowskillocc "Agri, Craft, Machine, Elementary"
* Calculating welfare in Real Terms (2011 Rs.)
merge m:1 year using "$path\data\CPI2011\cpi2011.dta"
keep if _merge==3
drop _merge
*gen welfarenatr = welfarenat/(cpiurban*ppp) if urban==1
*replace welfarenatr= welfarenat/(cpirural*ppp) if urban==0
*la var welfarenatr "Welfare aggregate for national poverty (2011 Rs.)"
gen welfareintr = welfare/(cpiurban*ppp) if urban==1
replace welfareintr= welfare/(cpirural*ppp) if urban==0
la var welfareintr "Welfare aggregate for international poverty (2011 Rs.)"

drop cpirural cpiurban 

save "$data68\finaldata68.dta", replace

use "$data72\descdata72.dta", clear
gen urban=(sector=="2")
gen Nhh=1 
replace year=2014 if year==.
/* state rainfall 
merge m:1 state using "$path\data\raindata\raindata", keepusing(state state_name mean_rainfall_2014* mean_rainfall_2015*) keep(1 3) 
tab _m  
drop _m 
rename mean_rainfall_2014* rainfall* 
rename mean_rainfall_2015* rainfall* 
*/

merge n:1 state district using "$path\data\raindata\district_rain_std", keepusing(rainfallY_2014 rainfall_2014* rainfall_2015*) 
tab _m 
drop if _m==2
drop _m 

rename rainfall_2014* rainfall* 
rename rainfall_2015* rainfall* 
rename rainfallY_2014* rainfallY

gen highskillocc=((occup_e==1) | (occup_e==2) | (occup_e==3))
gen middleskillocc = ((occup_e==4)| (occup_e==5))
gen lowskillocc=((occup_e==6) | (occup_e==7) | (occup_e==8) | (occup_e==9))
label var highskillocc "Bureaucrats, Professional and Technicians"
label var middleskillocc "Clerks and Service Related"
label var lowskillocc "Agri, Craft, Machine, Elementary"

* Generating a pop weight. For all years other than 2014-15, pop_wgt calculated in this way.
gen pop_wgt=hhwt*hhsize 

save "$data72\finaldata72.dta", replace

*************************************************** Creating PSU Level Variables ************************************************
********* Regional Stats *********** 
local all 61 66 68 72
foreach y in `all'{
use "$path\data\NSSO`y'\finaldata`y'.dta", clear
cap gen poor_int=. 
* Consumption: Means by District
/*
collapse (mean) Dhhsizecat* Dagecat* hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion agri ind services scst highskillocc middleskillocc lowskill transprln recrln hhrln transpr hhr communir recr transprx hhrx communirx recrx transprpc hhrpc communirpc recrpc (rawsum) Nhh ///
(rawsum) hhwt pop_wgt [pw=pop_wgt], by(id_sae_1) 
*/ 
collapse (mean) Dhhsizecat* Dagecat* hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion agri ind services scst highskillocc middleskillocc lowskill transprln recrln hhrln transpr transp1r transp2r  hhr communir recr transprx transp2rx hhrx communirx recrx transprpc hhrpc communirpc recrpc (rawsum) Nhh ///
(rawsum) hhwt pop_wgt [pw=pop_wgt], by(state district) 
save "$path\data\replication\NSSO`y'\varsmeans_reg`y'.dta", replace
gen round=`y'
save "$path\data\replication\NSSO`y'\final_reg`y'.dta", replace
}

* Creating a panel data set at the District level *
use "$data61\final_reg61.dta", clear
append using "$data66\final_reg66.dta"
append using "$data68\final_reg68.dta"
append using "$data72\final_reg72.dta"

* sort id_sae_1 round
sort state district round
save "$path\data\replication\finaldata\panel_reg.dta", replace

************************************************** Creating a HH level Dataset with District Means *********************
local all 61 66 68 72
foreach y in `all'{
use "$path\data\replication\NSSO`y'\finaldata`y'.dta", clear
* This code is to distinguish between means and HH level variables * 

foreach v in Dhhsizecat* Dagecat* agri ind services hhsize depratio rural selfemployed casualrural casualurban regwageurban regwagerural otherhhtype hindu islam otherreligion scst transpr transp1r transp2r recr hhr transprpc recrpc hhrpc communirpc transprx transp2rx recrx hhrx communirx transprln recrln hhrln   lowskill middleskillocc highskillocc{
rename `v' `v'h
}


merge m:1 state district using "$path\data\replication\NSSO`y'\varsmeans_reg`y'.dta"
drop _merge

tostring principind, replace
tostring principocc, replace

save "$path\data\replication\NSSO`y'\finaldata`y'_agg.dta", replace
}

use "$data61\finaldata61_agg.dta", clear
append using "$data66\finaldata66_agg.dta"
append using "$data68\finaldata68_agg.dta"
append using "$data72\finaldata72_agg.dta"
replace year=2004.5 if year==2004 
replace year=2009.5 if year==2009
replace year=2011.5 if year==2011
replace year=2014.5 if year==2014

gen t=year-2004.5
* local x  "Dhhsizecat1h Dhhsizecat2h Dhhsizecat3h Dhhsizecat4h Dhhsizecat5h Dhhsizecat6h Dhhsizecat1 Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 regwageurbanh regwageurban selfemployedh  hinduh  scsth  agrih  indh  highskillocch middleskillocch transprh recrh  hhrh transprlnh recrlnh  hhrlnh  transprln recrln  hhrln transprxh recrxh  hhrxh communirxh transprpch recrpch  hhrpch communirpch Dagecat1h Dagecat2h Dagecat3h Dagecat4h Dagecat5h Dagecat6h Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5 Dagecat6   hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transprpc  recrpc  hhrpc transprx  recrx  hhrx  transpr  recr  hhr rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"
local x  "Dhhsizecat2h Dhhsizecat3h Dhhsizecat4h Dhhsizecat5h Dhhsizecat6h Dhhsizecat2 Dhhsizecat3 Dhhsizecat4 Dhhsizecat5 Dhhsizecat6 regwageurbanh" 
local x "`x' regwageurban casualurban casualurbanh selfemployedh  hinduh  scsth  agrih  indh  highskillocch middleskillocch transprh   hhrh transprlnh recrlnh  hhrlnh  transprln recrln  hhrln"
local x "`x' transprxh  communirxh transprpch recrpch  hhrpch communirpch Dagecat1h Dagecat2h Dagecat3h Dagecat4h Dagecat5h Dagecat6h Dagecat1 Dagecat2 Dagecat3 Dagecat4 Dagecat5 Dagecat6"
local x "`x' hhsize depratio  selfemployed  hindu  scst  agri  ind  highskillocc middleskillocc transprpc  recrpc  hhrpc transpr transp1r transp2r transp2rh transprx transp2rx transp2rxh recr  recrh recrxh recrx"
local x "`x' hhr hhrx hhrxh rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"


foreach var of local x {
gen `var't= c.`var'#c.t
} 

foreach q in 1 2 3 4 { 
gen rainfallQ`q'sq=rainfallQ`q'^2
gen rainfallQ`q'sqt=rainfallQ`q't^2
 
} 

* egen district_id=group(state district) 

egen district_id=group(NSS61state_id NSS61dist_code), missing
separate district_id, by(urb) 
rename district_id0 district_idr
rename district_id1 district_idu
egen state_id=group(NSS61state_id) 
separate state_id, by(urb) 
rename state_id0 state_idr 
rename state_id1 state_idu 


* xi, pre(DDR) noomit: i.district_idr*year 
* xi, pre(DDU) noomit: i.district_idu*year 
qui tab district_idr, gen(DDR_) 
qui tab district_idu, gen(DDU_) 
qui tab state_idr, gen(DSR_)
qui tab state_idu, gen(DSU_)
 
 
foreach var of varlist DDR* DDU* DSR* DSU* { 
qui summ `var'
if r(N)>0 & r(sd)>0 { 
qui gen `var't=`var'*t 
}
else if r(N)==0 | r(sd)==0 { 
drop `var'
} 
} 
 

la var Dagecat1h "Prop HH 0-15 yrs" 
la var Dagecat2h "Prop HH 16-24 yrs" 
la var Dagecat3h "Prop HH 25-34 yrs"
la var Dagecat1h "Prop HH 0-15 yrs" 
la var Dagecat2h "Prop HH 16-24 yrs" 
la var Dagecat3h "Prop HH 25-34 yrs" 
la var Dagecat4h "Prop HH 35-49" 
la var Dagecat6h "Prop HH 65+ yrs" 
la var Dagecat1ht "Prop HH 0-15 yrs*t" 
la var Dagecat2ht "Prop HH 16-24 yr*t" 
la var Dagecat3ht "Prop HH 25-34 yrs*t" 
la var Dagecat4ht "Prop HH 35-49*t" 
la var Dagecat5ht "Prop HH 50-64 yrs*t" 
la var hinduh "Hindu" 
la var hinduht "Hindu*t" 
la var scsth "Low caste" 
la var scstht "Low caste*t" 
la var selfemployedh "HH type: Self Employed" 
la var selfemployedht "HH type: Self Employed*t" 
la var regwageurbanh "HH type: Regular Wage Worker" 
la var regwageurbanht "HH type: Regular Wage Worker*t" 
la var agrih "Princip Ind: Agri" 
la var agriht "Princip Ind: Agri*t" 
la var indh "Princip Ind: Industry" 
la var indht "Princip Ind: Industry*t"
la var highskillocch "High Skill Occupation" 
la var middleskillocch "Middle Skill Occupation" 
la var highskilloccht "High Skill Occupation*t" 
la var middleskilloccht "Middle Skill Occupation*t" 
la var transprh "Transport services expenses" 
la var transp1rh "Transport services expenses no fuel" 
la var transp2rh "Transport services expenses no fuel and bus or tram" 
la var recrh "Recreation services expenses" 
la var recrht "Recreation services expenses*t" 
la var hhrh "Household services expenses" 
la var hhrht "Household services expenses*t" 
la var rainfallQ1 "Rainfall Q1" 
la var rainfallQ2 "Rainfall Q2" 
la var rainfallQ3 "Rainfall Q3" 
la var rainfallQ4 "Rainfall Q4"
la var rainfallQ1sq "Rainfall Q1 squared" 
la var rainfallQ2sq "Rainfall Q2 squared" 
la var rainfallQ3sq "Rainfall Q3 squared" 
la var rainfallQ4sq "Rainfall Q4 squared"

la var selfemployed "HH type: Self Employed (District)"  
la var hindu "Hindu (District)" 
la var scst "Low Caste (District)"  
la var agri "Princip Ind: Agri (District)" 
la var ind "Princip Ind: Industry (District)" 
la var indt "Princip Ind: Industry (District)*t" 
la var highskillocc "High Skill Occupation (Distritc)" 
la var middleskillocc "Middle Skill Occupation(District)" 
la var transpr "Transport services expenses (District)" 
la var transp1r "Transp services exp no fuel (District)"
la var transp2r "Transp services exp no fuel and bus(District)"
la var recr "Recreation services expenses (District)" 
la var recrt "Recreation services expenses (District)*t" 
la var hhr "Household services expenses (District)"
la var Dagecat1 "Prop HH 0-15 yrs (District)" 
la var Dagecat1t "Prop HH 0-15 yrs (District)*t" 
la var Dagecat2 "Prop HH 16-24 yrs (District)" 
la var Dagecat2t "Prop HH 16-24 yrs (District)*t" 
la var Dagecat3 "Prop HH 25-34 yrs (District)" 
la var Dagecat4 "Prop HH 35-49 (District)" 
la var Dagecat6 "Prop HH 65+ yrs (District)" 
* la var Dhhsizecat1 "HH size 1 (District)" 
la var Dhhsizecat2 "HH size 1 or 2 (District)" 
la var Dhhsizecat2t "HH size 2 (District)*t" 
la var Dhhsizecat3 "HH size 3 (District)" 
la var Dhhsizecat4 "HH size 4 (District)" 
la var Dhhsizecat5 "HH size 5 (District)" 
la var Dhhsizecat6 "HH size 6 (District)" 
la var Dhhsizecat6t "HH size 6 (District)*t"
save "$data\finaldata\hh_reg.dta", replace


* Problems with the IDs for the 2009 data
* Checking that the psu variable is the same as the FSU variable
use "$path\data\replication\finaldata\hh_reg.dta", clear
destring B1_v01, replace
gen check=qhclust - B1_v01
tab year if check~=0 

* Creating the datasets for the SAE analysis
use "$data\finaldata\hh_reg.dta", clear
gen lnwelf = ln(welfareintr)
egen district_year_tag=tag(state district year) 
egen Nyearsdistrict=sum(district_year_tag), by(state district) 


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

* Validation Test: Creating urban/rural for the SAE analysis: Forward Projection from 2004.5 into 2009.5
use "$data\finaldata\hh_reg.dta", clear
keep if year==2009.5
save "$data\finaldata\sae09.dta", replace

* Rural * 
use "$data\finaldata\sae09.dta", clear
keep if urban==0
save "$data\finaldata\sae09r.dta", replace

* Urban * 
use "$data\finaldata\sae09.dta", clear
keep if urban==1
save "$data\finaldata\sae09u.dta", replace

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

* Calculating Poverty Lines for the Different years
use "$path\data\CPI2011\cpi2011.dta", clear	
replace year=year+0.5
merge 1:m year using "$data\finaldata\hh_reg.dta"
keep if _m==3
duplicates drop year sector, force
sort sector year
foreach v in nat int{
gen pline_`v'r=pline_`v'/(cpi*ppp) 
replace pline_`v'r=pline_`v'r[_n-1] if year==2014.5
}
keep year sector urban cpiurban cpirural pline_int pline_intr pline_nat pline_natr ppp
save "$path\data\plines.dta", replace




