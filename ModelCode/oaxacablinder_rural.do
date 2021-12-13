*fversion 15 
clear 
matrix drop _all 
cap log close
cap log using oaxacablinder_rural, text replace 

glo path "D:\Poverty_Estimates_India"
glo pathout "D:\Poverty_Estimates_India\IndiaModelRuns"

glo data  "$path\data\replication" //contains input data
glo data2  "$pathout\data\replication" //contains input data

glo analysis  "$pathout\analysis" //all these are output datasets, the result of "analysis"

global ruralvars "Dagecat1h Dagecat1 Dagecat2 Dagecat2h Dagecat3 Dagecat3h Dagecat4h Dagecat4 Dagecat5" 
global ruralvars "${ruralvars} hhsizeh hhsize highskillocc highskillocch" 
global ruralvars "${ruralvars} middleskillocc middleskillocch indh agri agrih scst scsth selfemployed selfemployedh" 
global ruralvars "${ruralvars} depratio depratioh hhage"
global ruralvars "${ruralvars} Dmarriedh Dmarried" 
global ruralvars "${ruralvars} Dmaleh Dmale" 
global ruralvars "${ruralvars} Dfirewood Ddungcakeh Ddungcake Dnocookingh Dnocooking Dcokecoal"
global ruralvars "${ruralvars} Dkerosene Delectricityh Delectricity Dcharcoal Dlpg Dlpgh"  
global ruralvars "${ruralvars} rainfallQ1 rainfallQ2 rainfallQ3 rainfallQ4"
global ruralvars "${ruralvars} rainfallQ1sq rainfallQ2sq rainfallQ3sq"

		   
			   

****Code to estimate the drivers of the difference between predictions in 2011 and 2018

use "$path\data\finaldata\oaxaca_rural.dta", clear 

reg lnwelf $ruralvars [pw = pop_wgt] if year == 2011
predict yhatrural 

oaxaca yhatrural $ruralvars [pw=pop_wgt], by(year) pooled relax nose

matrix results_rural = r(table)'
putexcel set oaxaca_rural.xlsx, sheet("rural", replace) modify
putexcel A1 = "Type"
putexcel B1 = "Variable"
putexcel C1 = "Value"
putexcel A2 = matrix(results_rural),rownames
cap log close

























