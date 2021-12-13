*** This script is something of road map to run all models as well as model validations
******* To run a full simulation, there is master file that contains some locals and globals which should be run
******* then a specific file that runs both stages of the small area estimation as well as any preceding model selection

**** Here are the four latest models for predicting into 2017/18 poverty rates

***For Model 1 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_stepwise_ruralnn_rainspline.do"

***For Model 2 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_matchvars_nonnormal_spline.do"

***For Model 3 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_sw_ruralnn_rainsq.do"

***For Model 4
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_lasso_ruralnn_rainsq.do"


***** Here are the validations
**** For 2004/05 poverty rates
***For Model 1 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae04_using1112_stepwise_ruralnn_rainspline.do"

***For Model 2 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae04_using1112_matchvars_nonnormal_spline.do"

***For Model 3 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae04_using1112_sw_ruralnn_rainsq.do"

***For Model 4
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae04_using1112_lasso_ruralnn_rainsq.do"


**** For 2009/10 poverty rates
***For Model 1 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae09_using1112_stepwise_ruralnn_rainspline.do"

***For Model 2 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae09_using1112_matchvars_nonnormal_spline.do"

***For Model 3 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae09_using1112_sw_ruralnn_rainsq.do"

***For Model 4
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae09_using1112_lasso_ruralnn_rainsq.do"


****** We also use the SCS Health 2014 to predict poverty rates into 2014/15. This is important for validating then
****** the use of the SCS Health in part and the idea was to see how we compare to David and Pallavi's estimates in the same year
**** For 2014/15 poverty rates
***For Model 1 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae14_using1112_stepwise_ruralnn_rainspline.do"

***For Model 2 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae14_using1112_matchvars_nonnormal_spline.do"

***For Model 3 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae14_using1112_sw_ruralnn_rainsq.do"

***For Model 4
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae14_using1112_lasso_ruralnn_rainsq.do"




**** Normal distribution both rural and urban error models (run on version 2 of the stata code)
***For Model 1 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_stepwise_allnormal_rainspline_v2.do"

***For Model 2 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_matchvars_allnormal_spline_v2.do"

***For Model 3 
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_sw_allnormal_rainsq_v2.do"

***For Model 4
do "D:\Poverty_Estimates_India\SAE_India\Master_00_test_newvars.do" 
do "D:\Poverty_Estimates_India\SAE_India\sae18_using1112_lasso_allnormal_rainsq_v2.do"




