*fversion 15 
clear 
matrix drop _all 
cap log close
cap log using oaxacablinder, text replace 

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

use "$path\data\finaldata\oaxaca_rural.dta", clear 

reg lnwelf $ruralvars [pw = pop_wgt] if year == 2011
predict yhatrural 

oaxaca yhatrural $ruralvars [pw = pop_wgt], by(year) pooled relax nose
estimates save "${analysis}/oaxaca_rural", replace

use "$path\data\finaldata\oaxaca_urban.dta", clear 

reg lnwelf $urbanvars [pw = pop_wgt] if year == 2011
predict yhaturban 

oaxaca yhaturban $urbanvars [pw = pop_wgt], by(year) pooled relax nose
estimates save "${analysis}/oaxaca_urban", replace


cap log close

























