glo path "D:\Poverty_Estimates_India"
glo pathout "D:\Poverty_Estimates_India\IndiaModelRuns_pooleddata"


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
cd "D:\Poverty_Estimates_India\dofiles" 

** Set baseline model 


estimates use ..\estimates\districtmeans\replicate\model24_urb 
local cmd `=e(cmdline)' 
gettoken cmd : cmd, parse("[")

global xvar111t : subinstr local cmd "regress lnwelf " "" 
* drop scst (scheduled caste interacted with time trend) because main effect is not included in both urban and rural models 
** drop all expenditure variables
global xvar111t : subinstr global xvar111t " scst " " " 
	
dis "$xvar111t"	
	
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
