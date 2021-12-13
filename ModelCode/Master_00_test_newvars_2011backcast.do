/*****************************************************************************************************
*                                                                                                    *
                                   INITIAL COMMANDS
*                                                                                                    *
*****************************************************************************************************/

** INITIAL COMMANDS
   clear
   set more off, perm
   set matsize 5000 
   
** DIRECTORY


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

	
glo candidate_xvars_rur "Dhhsizecat? Dhhsizecat?h Dagecat? Dagecat?h hindu hinduh scst scsth" 
glo candidate_xvars_rur "${candidate_xvars_rur} selfemployed selfemployedh casualurban regwageurban" 
glo candidate_xvars_rur "${candidate_xvars_rur} middleskillocc	middleskillocch highskillocc highskillocch agri agrih ind indh " 
glo candidate_xvars_rur "${candidate_xvars_rur} rainfallQ? rainfallQ?sq" 
*/ 

glo candidate_xvars_rur_tt "Dhhsizecat?h Dagecat?h hinduh scsth" 
glo candidate_xvars_rur_tt "${candidate_xvars_rur_tt} selfemployedh" 
glo candidate_xvars_rur_tt "${candidate_xvars_rur_tt} middleskillocch highskillocch agrih indh " 

 
 
glo candidate_xvars_urb  "Dhhsizecat? Dhhsizecat?h Dagecat? Dagecat?h hindu hinduh scst scsth"
glo candidate_xvars_urb "${candidate_xvars_urb} 	selfemployed selfemployedh casualurban casualurbanh regwageurban regwageurbanh "
glo candidate_xvars_urb	"${candidate_xvars_urb} middleskillocc	middleskillocch highskillocc highskillocch agri agrih ind indh  "
glo candidate_xvars_urb "${candidate_xvars_urb} rainfallQ? rainfallQ?sq" 	
 

glo candidate_xvars_urb_tt  "Dhhsizecat?h Dagecat?h hinduh scsth"
glo candidate_xvars_urb_tt "${candidate_xvars_urb_tt} selfemployedh casualurbanh regwageurbanh"
glo candidate_xvars_urb_tt	"${candidate_xvars_urb_tt}  middleskillocch highskillocch agrih indh  "


glo estimates "$pathout\estimates" 
glo estimateslasso "$estimates\new_transport\lasso" 




**here is a list of variables we drop 

*from the candidate_xvars_rur = hhr hhrh recr recrh transpr transprh
*from the candidate_xvars_rur_tt = hhrh recrh transprh
*from the candidate_xvars_urb = hhr hhrh recr recrh transpr transprh
*from the candidate_xvars_urb_tt = hhrh recrh transprh



*exit 
*glo candidate_xvars_urb "${candidate_xvars_urb}  hhrh recrh transprh rainfallQ? rainfallQ?sq" 	

	
/* I. Each file below gets the data for all 4 rounds.

a) Household Characteristics
data_hh_charac.do

b) Durable Expenditures 
data_durables.do

c) Expenses for miscellaneous consumer services
data_misc_services.do

d) Total Consumer Expenditure Data
data_consump_exp.do

e) process district concordance file 

do process_districts.do 



* II. Merging all of the above datasets together
do finaldata.do

* III. Creating Descriptive Statistics
desc.do
desc_durables.do

* IV. Regressions
regressions.do
regressions_regions.do
*/ 
 
* V. Select models, separately for district time trends and aggregates 
* time trends program takes a LONG time (3 days) to run 
* do modelselection_timetrends 
* do modelselection_districtmeans 

**here is a list of variables we drop 

*from the candidate_xvars_rur = hhr hhrh recr recrh transpr transprh
*from the candidate_xvars_rur_tt = hhrh recrh transprh
*from the candidate_xvars_urb = hhr hhrh recr recrh transpr transprh
*from the candidate_xvars_urb_tt = hhrh recrh transprh


** Set baseline model 

cd "D:\Poverty_Estimates_India\dofiles" 
estimates use ..\estimates\districtmeans\replicate\model24_urb 
local cmd `=e(cmdline)' 
gettoken cmd : cmd, parse("[")

global xvar111t : subinstr local cmd "regress lnwelf " "" 
* drop scst (scheduled caste interacted with time trend) because main effect is not included in both urban and rural models 
** drop all expenditure variables
global xvar111t : subinstr global xvar111t " scst " " " 
	
** rural 
estimates use ..\estimates\districtmeans\replicate\model22_rur 
local cmd `=e(cmdline)' 
gettoken cmd : cmd, parse("[")

global xvar011t : subinstr local cmd "regress lnwelf " "" 
	
	

* District time trend model 

*1. set global models xvars011t and xvars111t for main models based on final models estimated in modelselection_dn.do. This is model 6 for the urban model and model 10 for the rural model. 
	** urban 
	cd "D:\Poverty_Estimates_India\dofiles" 
	estimates use ..\estimates\timetrends\model4_urb 
	local cmd `=e(cmdline)' 
	gettoken cmd : cmd, parse("[")
	global xvar111t_mr2 : subinstr local cmd "regress lnwelf " "" 
	** rural 
	estimates use ..\estimates\timetrends\model4_rur 
	local cmd `=e(cmdline)' 
	gettoken cmd : cmd, parse("[")
	global xvar011t_mr2 : subinstr local cmd "regress lnwelf " "" 
	
	
 
 exit
 * 4. Set models for etsimating with augmented dataset using admin data provided by Yue Li. 
 * global cc_xvar011t_yue "$cc_xvar011t lg_dens-lg_m_dec_tmp"
 * global cc_xvar111t_yue "$cc_xvar111t lg_dens-lg_m_dec_tmp"




/*2. Set constant coefficent models  
* keep only level variables for constant coefficient model, so remove all time trends 
	
 global cc_xvar011t "$xvar011t"		
 global cc_xvar111t "$xvar011t"		
 foreach var in $cc_xvar011t { 
  global cc_xvar011t : subinstr global cc_xvar011t "`var't " "", all 
 } 

  foreach var in $cc_xvar111t { 
  global cc_xvar111t : subinstr global cc_xvar111t "`var't " "", all 
 } 
*/ 

** core results based on district means model
do sae14 

* Uses the 2004.5 and 2009.5 data to predict into 2011.
do sae11 

*Uses the 2009.5 and 2011.5 data to predict into 2004.
do sae04.do

*Results using the ELL method. Uses 2004.5, 2009.5 and 2011.5 to predict poverty in 2014.5.
do sae14_martin2.do
do sae11_martin2.do
do sae04_martin2.do

** extensive margin for consumption variables 
do sae14_model2.do 
do sae11_model2.do 
do sae04_model2.do 

** constant coefficient model 
do sae14_model3.do 
do sae11_model3.do 
do sae04_model3.do 



* do robustness checks 
* 1. check against state GDP growth 
do crosscheck_against_state_gdp_growth 

* do oaxaca-blinder and output regression coefficients 
do "understand drivers of prediction" 

* plot welfare distribution 
do "plot welfare distribution on same graph" 

* look at lineup estimates
do "do_lineup_tests" 

* explore proper trimming rule 
do gini_and_mean
do gini_and_mean_2011 
do gini_and_mean_2004 
do gini_and_mean_table 

exit 


** using district level time trends 
* do sae14_martin1.do


* Excludes mean expenditures on miscellaneous consumer expenditures and services at the household and psu level
do sae14_model2.do 

* Includes Per Capita Expenditures on miscellaneous consumer expenditures and services at the household and psu level
do sae14_model3.do 

* sae14 without time trends
do sae14_model4.do

* Using only 2011.5 for Prediction
do sae14_model5.do 

* Includes Ln expenses of miscellaneous consumer expenditures and services
do sae14_model6.do 



* Uses the 2004.5 data to predict into 2009.5. Excludes variables with time trends.
do sae09_notrend.do

* using district level time trends 
do sae11_martin1.do


* robustness checks predicting into 2004 and 2011: Exclde mean expenditures, only include dummies for expendiyire  
do sae11_model2 
do sae04_model2 

* robustness checks predicting into 2004 and 2011: Include per capita mean expenditures, only include dummies for expendiyire  
do sae11_model3 
do sae04_model3 

* Uses the 2009.5 data to predict into 2011.5. Excludes variables with time trends.
do sae11_nottrend.do 

* Uses the 2009.5 data to predict into 2004.5. Excludes variables with time trends.
do sae04_notrend.do 

* Uses 2004.5 data to predict into 2009.5 
do sae09_nottrend.do 



* 2. Understand what is driving poverty changes in linear probability model 
do "understand drivers of prediction" 

* 3. Plot CDF of welfare on same graph 
do "plot welfare distributions on same graph" 

*4. Estimate model with district time trends 
do sae14_martin1 

*5. Test model with district time trends 
do sae11_martin1 

*6. Estimate constsnta coefficient with 2004 data, without additional variables 
do sae14_martin2 

*6. Estimate constsnta coefficient with 2004 data, with additional variables 
do sae14_martin3 




*** Other files created
V. CPI 
CPI2011.do

VI. Rainfall
rainfall.do

VI. For NSSO61, Block 13 was created. 
check\blk13_created.do

VII. Data Dictionary for NSSO72 
NSS72.do


