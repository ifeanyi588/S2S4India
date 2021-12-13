*fversion 15 
clear 
matrix drop _all 
cap log close
cap log using oaxacablinder_urban, text replace 

glo path "D:\Poverty_Estimates_India"
glo pathout "D:\Poverty_Estimates_India\IndiaModelRuns"

glo data  "$path\data\replication" //contains input data
glo data2  "$pathout\data\replication" //contains input data

glo analysis  "$pathout\analysis" //all these are output datasets, the result of "analysis"

global urbanvars "Dagecat1h Dagecat1 Dagecat2 Dagecat2h Dagecat3 Dagecat3h Dagecat4h Dagecat5 Dagecat5h" 
global urbanvars "${urbanvars} hhsizeh hhsize highskillocc highskillocch" 
global urbanvars "${urbanvars} middleskillocch indh agrih agri scsth selfemployedh" 
global urbanvars "${urbanvars} regwageurban regwageurbanh depratio"
global urbanvars "${urbanvars} Dmarriedh Dmaleh hhage" 
global urbanvars "${urbanvars} Dfirewood Dfirewoodh Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoalh Dkeroseneh"
global urbanvars "${urbanvars} Dkerosene Delectricityh Delectricity Dcharcoal Dcharcoalh Dlpg"  
global urbanvars "${urbanvars} rainfallQ1 rainfallQ3 rainfallQ4"
global urbanvars "${urbanvars} rainfallQ1sq rainfallQ2sq rainfallQ3sq rainfallQ4sq"
			   
			   

****Code to estimate the drivers of the difference between predictions in 2011 and 2018

use "$path\data\finaldata\oaxaca_urban.dta", clear 


reg lnwelf $urbanvars [pw = pop_wgt] if year == 2011
predict yhaturban 

oaxaca yhaturban $urbanvars [pw = pop_wgt], by(year) pooled relax nose
matrix results_urban = r(table)'
putexcel set oaxaca_urban.xlsx, sheet("urban", replace) modify
putexcel A1 = "Type"
putexcel B1 = "Variable"
putexcel C1 = "Value"
putexcel A2 = matrix(results_urban), rownames





cap log close

























