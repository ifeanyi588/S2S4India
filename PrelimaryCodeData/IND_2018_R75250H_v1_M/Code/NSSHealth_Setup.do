clear
local who "ife"

* INSTRUCTION : include a line for your directory before use. There are a couple of examples below

if ("`who'"=="lau") glo path "C:\Users\WB473845\OneDrive - WBG\SAR\India"
if ("`who'"=="ife") glo path "C:\Users\wb559885\WBG\Laura Liliana Moreno Herrera - India"

*setting the working directory

cd "${path}\1.India2017\2.Data\1.Consumption"

*setting up the number of demarcations of that splits the first variables of each dataset into required number of variables
local l01length 3 5	2 3	1 1	3 2	2 2	1 1	4 1	1 2	2 5	2 1	1 1	4 4	4 6	6 3	1 1	1 1	1 48 3 3 10
local l02length 34 2 5 2 1 5 3 1 1 1 2 1 2 2 1 2 1 8 8 44 3 3 10														
local l03length 34 2 3 2 1 1 3 1 2 2 1 3 1 1 1 1 1 1 1 1 63 3 3 10													
local l04length 34 2 3 2 1 3 1 1 3 1 1 1 73 3 3 10																					
local l05length 34 2 1 2 2 3 2 1 1 1 1 1 1 3 1 1 1 1 1 1 1 5 1 1 1 3 53 3 3 10							
local l06length 34 2 1 2 2 3 1 8 8 8 8 8 8 8 8 8 8 1 3 3 10																
local l07length 34 2 1 2 2 3 8 1 1 1 2 8 61 3 3 10																					
local l08length 34 2 1 2 2 3 2 2 2 2 1 1 5 1 1 1 1 1 1 1 60 3 3 10													
local l09length 34 2 1 2 2 3 1 1 1 1 1 1 8 8 8 8 8 8 8 8 8 4 3 3 10												
local l10length 34 2 1 2 2 3 8 1 1 2 8 62 3 3 10 																					
local l11length 34 2 3 2 3 2 2 1 2 1 1 1 1 1 1 1 68 3 3 10																	
local l12length 34 2 3 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 8 2 59 3 3 10												
local l13length 34 2 3 2 3 1 1 1 3 1 1 2 8 1 1 1 8 1 1 8 2 41 3 3 10												

foreach f of numlist 1/13 {  
	*noi display "${path}\1.India2017\2.Data\1.Consumption\R75250L0`f'h.TXT"
	if (`f'<10) local f="0`f'"
	import delimited "${path}\1.India2017\2.Data\1.Consumption\R75250L`f'h.TXT", encoding(Big5) clear 
	*C:\Users\wb559885\WBG\Laura Liliana Moreno Herrera - India\1.India2017\2.Data\1.Consumption
	local v=0
	local l1=1
	
	foreach l2 in `l`f'length' {
		local v=`v'+1
		gen var`v'=substr(v1, `l1', `l2')
		local l1=`l1'+`l2'
	} 

	
	if `f' == 1 {
		la var var1		"Centre, Round"
		la var var2		"FSU Serial No."
		la var var3		"Round"
		la var var4		"Schedule"
		la var var5		"Sample"
		la var var6		"Sector"
		la var var7		"NSS-Region"
		la var var8		"District"
		la var var9		"Stratum"
		la var var10	"Sub-stratum"
		la var var11	"Sub-Round"
		la var var12	"Sub-sample"
		la var var13	"FOD-Sub-Region"
		la var var14	"Hamlet group/ Sub-block no."
		la var var15	"Second-stage-stratum no."
		la var var16	"Sample hhld. No."
		la var var17	"Level" 
		la var var18	"Filler"
		la var var19	"Sl.no. of informant(as in col. 1, block 4)"
		la var var20	"Response Code"
		la var var21	"Survey Code"
		la var var22	"Substitution Code/ Casualty code"
		la var var23	"Employee code"
		la var var24	"Employee code"
		la var var25	"Employee code"
		la var var26	"Date of Survey"
		la var var27	"Date of Despatch"
		la var var28	"Time to canvass (minutes)"
		la var var29	"No. of investigators (FI/ ASO) in the team"
		la var var30	"Remarks by FI or JSO in block 9"
		la var var31	"Remarks in block 10 by supervisory officer"
		la var var32	"Remarks elsewhere in Sch.  by FI or JSO" 
		la var var33	"Remarks elsewhere in Sch. by supervisory officer"
		la var var34	"Blank"
		la var var35	"NSS"
		la var var36	"NSC"
		la var var37	"MULT"

	}
	
	if `f' == 2 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Household size"
		la var var5		"Whether HHD paid major share for childbirth expenses for any non-HHD female member"
		la var var6		"NIC-2008 five digit code"
		la var var7		"NCO-2004 three digit code"
		la var var8		"Household type"
		la var var9		"Religion"
		la var var10	"Social group" 
		la var var11	"Type of latrine usually used"
		la var var12	"Access to latrine"
		la var var13	"How many members use the latrine"
		la var var14	"Major source of drinking water"
		la var var15	"Arrangement of garbage disposal"
		la var var16	"Primary source of energy for cooking"
		la var var17	"Was there sudden outbreak of communicable disease in the community"
		la var var18	"Amount of medical insurance premium (Rs.)"
		la var var19	"Household usual consumer expenditure (Rs.)"
		la var var20	"Blank"
		la var var21	"NSS"
		la var var22	"NSC"
		la var var23	"MULT"

	}
	
	if `f' == 3 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Person serial no."
		la var var5		"Relation to head"
		la var var6		"Gender"
		la var var7		"Age(in years)"
		la var var8		"Marital status"
		la var var9		"General education"
		la var var10	"Usual principal activity status code"
		la var var11	"During last 365 days-whether hopitalised" 
		la var var12	"If 1 in col. 9, no. of times hospitalised"
		la var var13	"Whether pregnant (female members of age 15 to 49 years)"
		la var var14	"Whether paid major share for child-birth expenses"
		la var var15	"Whether suffered from any communicable disease"
		la var var16	"Whether suffering from any chronic ailment"
		la var var17	"Whether suffered/suffering from any other ailment: any time during last 15 days"
		la var var18	"Whether suffered/suffering from any other ailment: on the day before date of survey"
		la var var19	"Whether covered by any scheme for health expenditure support"
		la var var20	"Reporting of col. 14 to 16"
		la var var21	"Blank"
		la var var22	"NSS"
		la var var23	"NSC"
		la var var24	"MULT"

	}
	
	if `f' == 4 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Person serial no."
		la var var5		"Gender"
		la var var6		"Age at death"
		la var var7		"Whether medical attention received before death"
		la var var8		"Whether hospitalised"
		la var var9		"If 1 in col. 6, no. of times hospitalised"
		la var var10	"Reason for non-hospitalisation just before death"
		la var var11	"Whether pregnant any time during last 365 days"
		la var var12	"If 1 in col. 9, time of death"
		la var var13	"Blank"
		la var var14	"NSS"
		la var var15	"NSC"
		la var var16	"MULT"

	}
	
	if `f' == 5 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of hospitalisation case"
		la var var5		"Srl. no. of member hospitalised (as in col. 1, bl. 4A & 4B/ 5)"
		la var var6		"Age (as in col. 5, bl. 4A & 4B/ col. 4, bl.5)"
		la var var7		"Nature of ailment"
		la var var8		"Nature of treatment"
		la var var9		"Type of medical institution"
		la var var10	"Reason for not availing govt./public hospital"
		la var var11	"Type of ward"
		la var var12	"When admitted"
		la var var13	"When discharged"
		la var var14	"Duration of stay in hospital (days)"
		la var var15	"Surgery"
		la var var16	"Medicine"
		la var var17	"X-ray/ ECG/ EEG/ Scan"
		la var var18	"Other diagnostic tests"
		la var var19	"Treated before hospitalisation"
		la var var20	"If 1 in item 15, nature of treatment"
		la var var21	"If 1 in item 15, level of care"
		la var var22	"If 1 in item 15, duration of treatment (days)"
		la var var23	"Treatment continued after discharge?"
		la var var24	"If 1 in item 19, nature of treatment"
		la var var25	"If 1 in item 19, level of care"
		la var var26	"If 1 in item 19, duration of treatment (days)"
		la var var27	"Blank"
		la var var28	"NSS"
		la var var29	"NSC"
		la var var30	"MULT"

	}
	
	if `f' == 6 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of hospitalisation case (as in item 1, bl. 6)"
		la var var5		"Srl. no. of member hospitalised (as in item 2, bl. 6)"
		la var var6		"Age (as in item 3, bl. 6)"
		la var var7		"Whether any medical advice provided free"
		la var var8		"Package component (Rs.)"
		la var var9		"Doctor's/ surgeon's fee (Rs.)"
		la var var10	"Medicines (Rs.)"
		la var var11	"Diagonistic tests (Rs.)"
		la var var12	"Bed charges (Rs.)"
		la var var13	"Other medical expenses (Rs.)"
		la var var14	"Medical expenditure(Rs.):total (items 5 to 10)"
		la var var15	"Transport for patient (Rs.)"
		la var var16	"Other non-medical expenses (Rs.)"
		la var var17	"Expenditure (Rs.): Total (items 11 to 13)"
		la var var18	"Blank"
		la var var19	"NSS"
		la var var20	"NSC"
		la var var21	"MULT"

	}
	
	if `f' == 7 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of hospitalisation case (as in item 1, bl. 6)"
		la var var5		"Srl. no. of member hospitalised (as in item 2, bl. 6)"
		la var var6		"Age (as in item 3, bl. 6)"
		la var var7		"Total amount reimbursed by medical insurance (Rs.)"
		la var var8		"Major source of finance"
		la var var9		"2nd most important source"
		la var var10	"Place of hospitalisation (code)"
		la var var11	"If code 5 in item 18, state code"
		la var var12	"Loss of household income, if any, due to hospitalisation (Rs.)"
		la var var13	"Blank"
		la var var14	"NSS"
		la var var15	"NSC"
		la var var16	"MULT"

	}
	
	if `f' == 8 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of spell of ailment"
		la var var5		"Srl. no. of member reporting ailment (as in col. 1, bl. 4A/ 5)"
		la var var6		"Age (as in col. 5, bl. 4A/ col. 4, bl.5)"
		la var var7		"No. of days within the ref. period - ill"
		la var var8		"No. of days within the ref. period - on restricted activity"
		la var var9		"No. of days within the ref. period - confined to bed"
		la var var10	"Nature of ailment"
		la var var11	"Whether chornic"
		la var var12	"Status of ailment"
		la var var13	"Total duration of ailment (days)"
		la var var14	"Nature of treatment"
		la var var15	"Whether hospitalised"
		la var var16	"Whether treatment taken on medical advice"
		la var var17	"Level of care"
		la var var18	"Reason for not availing Govt. sources"
		la var var19	"Reason for not seeking medical advice"
		la var var20	"Whom consulted"
		la var var21	"Blank"
		la var var22	"NSS"
		la var var23	"NSC"
		la var var24	"MULT"

	
	}
	
	if `f' == 9 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of spell of ailment(as in item 1, block 8)"
		la var var5		"Srl. no. of member reporting ailment(as in item 2, block 8)"
		la var var6		"Age (in years as in item 3, bl. 8)"
		la var var7		"Whether any medical service provided free"
		la var var8		"Surgery"
		la var var9		"Medicine received (AYUSH)"
		la var var10	"Medicine received other than AYUSH"
		la var var11	"X-ray/ ECG/ EEG/ Scan"
		la var var12	"Other diagnostic tests"
		la var var13	"Doctor's/ surgeon's fee (Rs.)"
		la var var14	"Medicines (Rs.): AYUSH"
		la var var15	"Medicines (Rs.): Other than AYUSH"
		la var var16	"Diagonistic tests (Rs.)"
		la var var17	"Other medical expenses (Rs.)"
		la var var18	"Medical expenditure(Rs.): total(items 10 to 14)"
		la var var19	"Transport for patient (Rs.)"
		la var var20	"Other expenses (Rs.)"
		la var var21	"Expenditure (Rs.): Total (items 15 to 17)"
		la var var22	"Blank"
		la var var23	"NSS"
		la var var24	"NSC"
		la var var25	"MULT"

	}
	
	if `f' == 10 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of spell of ailment(as in item 1, block 8)"
		la var var5		"Srl. no. of member reporting ailment(as in item 2, block 8)"
		la var var6		"Age (in years as in item 3, bl. 8)"
		la var var7		"Total amount reimbursed by medical insurance (Rs.)"
		la var var8		"Major source of finance"
		la var var9		"Place of treatment"
		la var var10	"If code 5 in item 21, state code"
		la var var11	"Loss of household income, due to treatment(Rs.)"
		la var var12	"Blank"
		la var var13	"NSS"
		la var var14	"NSC"
		la var var15	"MULT"

	}
	
	if `f' == 11 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of  member (as in col. 1, bl. 4A)"
		la var var5		"Age (as in col. 5, bl. 4A)"
		la var var6		"No. of sons living"
		la var var7		"No. of daughters living"
		la var var8		"State of economic independence"
		la var var9		"No. of dependents"
		la var var10	"Person financially supporting aged person"
		la var var11	"Place of stay"
		la var var12	"Living arrangement"
		la var var13	"Physical mobility"
		la var var14	"Person helping"
		la var var15	"Own perception-current state of health"
		la var var16	"Own perception- change in state of health"
		la var var17	"Blank"
		la var var18	"NSS"
		la var var19	"NSC"
		la var var20	"MULT"

	}
	
	if `f' == 12 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of  member (as in col. 1, bl. 4)"
		la var var5		"Age in months"
		la var var6		"BCG"
		la var var7		"oral Polio Vaccine doses : birth dose"
		la var var8		"oral Polio Vaccine doses : OPV1"
		la var var9		"oral Polio Vaccine doses : OPV2"
		la var var10	"oral Polio Vaccine doses : OPV3"
		la var var11	"oral Polio Vaccine doses : booster dose"
		la var var12	"DPT/Pentavalent doses: DPT-1/Pentavalent-1"
		la var var13	"DPT/Pentavalent doses: DPT-2/Pentavalent-2"
		la var var14	"DPT/Pentavalent doses: DPT-3/Pentavalent-3"
		la var var15	"DPT/Pentavalent doses: booster"
		la var var16	"Measles"
		la var var17	"Other immunisation"
		la var var18	"Information source of immunisation"
		la var var19	"Source of most immunisation"
		la var var20	"Expenditure on immunisation during last 365 days(Rs.)"
		la var var21	"Visit to anganwari centre during last 30 days(in days)"
		la var var22	"Blank"
		la var var23	"NSS"
		la var var24	"NSC"
		la var var25	"MULT"

	}
	
	if `f' == 13 {
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Srl. no. of  member (as in bl. 4/ 5)"
		la var var5		"Age (as in bl. 4/ 5)"
		la var var6		"Srl. no. of pregnancy(1/2)"
		la var var7		"Whether received tetanus toxoid vaccine during pregnancy"
		la var var8		"Whether consumed Iron and Folic Acid(IFA) during pregnancy"
		la var var9		"How many days IFA were consumed"
		la var var10	"Major source of receiving pre-natal care"
		la var var11	"Nature of pre-natal care"
		la var var12	"No. of pre-natal care visits"
		la var var13	"Total expenditure incurred on pre-natal care (Rs.)"
		la var var14	"Outcome of pregnancy"
		la var var15	"Place of delivery / abortion"
		la var var16	"Delivery was attended by"
		la var var17	"Expenditure of delivery at home(Rs.)"
		la var var18	"Major source of receiving post-natal care"
		la var var19	"Nature of post-natal care"
		la var var20	"Total expenditure incurred on post-natal care (Rs.)"
		la var var21	"Visit to anganwari centre(AWC) during last 30 days)in days)"
		la var var22	"Blank"
		la var var23	"NSS"
		la var var24	"NSC"
		la var var25	"MULT"

	}
	
	
	save "${path}\1.India2017\2.Data\1.Consumption\R75250L`f'h.dta", replace
	
	
	
	
	
	
	
	
	
	
	
	
}





















