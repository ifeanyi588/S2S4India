clear
local who "ife"

* include a line for your directory before use

if ("`who'"=="lau") glo path "C:\Users\WB473845\OneDrive - WBG\SAR\India"
if ("`who'"=="ife") glo path "C:\Users\wb559885\WBG\Laura Liliana Moreno Herrera - India"

*setting the working directory

cd "${path}\1.India2017\2.Data\1.Consumption"


*setting up the number of demarcations of that splits the first variables of each dataset into required number of variables

local l01length 3 5 2 3 1 1 3 2 2 2 1 1 4 1 1 2 2 5 2 1 1 1 4 4 4 6 6 3 1 1 1 1 1 48 3 3 10
local l02length 34 2 5 2 5 3 1 1 1 1 1 1 1 1 1 1 2 2 1 8 5 2 3 3 10
local l03length 34 2 3 2 1 3 2 2 1 1 2 1 8 64 3 3 10
local l04length 34  2 3 2 1 1 3 1 2 2 2 2 1 1 1 1 1 1 1 64 3 3 10
local l05length 34  2 3 2 3 2 2 2 2 2 1 2 1 1 1 1 1 1 1 1 1 8 1 8 1 1 1 1 1 1 1 1 1 1 1 32 3 3 10
local l06length 34 2 3 2 3 8 8 8 8 8 8 2 2 8 8 1 1 1 11 3 3 10
local l07length 34 2 3 2 3 1 2 2 1 1 2 2 1 1 8 2 59 3 3 10
local l08length 34 2 3 2 3 2 1 1 2 1 75 3 3 10


*labeling the variable in loop 

foreach f of numlist 1(1)8 {  

	import delimited "${path}\1.India2017\2.Data\1.Consumption\R75252L0`f'.TXT", encoding(Big5) clear 
	*C:\Users\wb559885\WBG\Laura Liliana Moreno Herrera - India\1.India2017\2.Data\1.Consumption
	local v=0
	local l1=1
	
	foreach l2 in `l0`f'length' {
		local v=`v'+1
		gen var`v'=substr(v1, `l1', `l2')
		local l1=`l1'+`l2'
	} 
	
	
	
	
	
	
	
	if `f'==1 {
		*Sch. 25.2 :    LEVEL - 01(Blocks 1, 2 and 11)
		*srl. no.	        Item
		la var var1	    "Centre, Round"
		la var var2	    "FSU Serial No."
		la var var3	    "Round"
		la var var4	    "Schedule"
		la var var5	    "Sample"
		la var var6	    "Sector"
		la var var7	    "NSS-Region"
		la var var8	    "District"
		la var var9	    "Stratum"
		la var var10	"Sub-stratum"
		la var var11	"Sub-Round"
		la var var12	"Sub-sample"
		la var var13	"FOD-Sub-Region"
		la var var14	"Hamlet group/ Sub-block no."
		la var var15	"Second-stage-stratum no."
		la var var16	"Sample hhld. No."
		la var var17	"Level "
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
		la var var32	"Remarks elsewhere in Sch.  by FI or JSO "
		la var var33	"Remarks elsewhere in Sch. by supervisory officer"
		la var var34	"Blank"
		la var var35	"NSS"
		la var var36	"NSC"
		la var var37	"MULT"
	}
	if `f'==2 {
		*ch. 25.2 :    LEVEL - 02 (Block 3)
		*srl. no.	        Item
		la var var1		"Common-ID"
		la var var2		"Level"
		la var var3		"Filler"
		la var var4		"Household size"
		la var var5		"NIC-2008 five digit code"
		la var var6		"NCO-2004 three digit code"
		la var var7		"Household type"
		la var var8		"Religion"
		la var var9		"Social group" 
		la var var10	"Distance to nearest school having primary level classes"
		la var var11	"Distance to nearest school having upper level classes"
		la var var12	"Distance to nearest school having secondary level classes"
		la var var13	"Whether household has computer"
		la var var14	"Whether any member has internet facility"
		la var var15	"Any erstwhile household member of age 3-35 years is currently attending education"
		la var var16	"For 1 in item 1, block 3 and 4 in col. 11 of block 4, whether staying in students' hostel"
		la var var17	"For code 1 in item 13, location of the parent household: State/ UT code"
		la var var18	"For code 1 in item 13, location of the parent household: District code"
		la var var19	"For code 1 in item 13, location of the parent household: Sector"
		la var var20	"Household's usual consumer expenditure (Rs.)"
		la var var21	"Blank"
		la var var22	"NSS"
		la var var23	"NSC"
		la var var24	"MULT"
	}
	
	if `f'==3 {	
		*Sch. 25.2 :   LEVEL - 03 (Block 3.1)
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no."
		la var var5	"Gender"
		la var var6	"Age(in years)"
		la var var7	"Present place of residence: State/UT code"
		la var var8	"Present place of residence: District code"
		la var var9	"Present place of residence: Sector"
		la var var10	"Type of residence"
		la var var11	"Level of current enrolment in the basic course"
		la var var12	"Whether any expenditure Incurred/to be incurred by the HHD during the current academic year for the member"
		la var var13	"If 1 in col. 12, expenditure incurred/to be incurred by the HHD during the current academic year for the member(Rs.)"
		la var var14	"Blank"
		la var var15	"NSS"
		la var var16	"NSC"
		la var var17	"MULT"
	}
	
	if `f'==4 {		
		*Sch. 25.2 :   LEVEL - 04 (Block 4)
			
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no."
		la var var5	"Relation to head"
		la var var6	"Gender"
		la var var7	"Age(in years)"
		la var var8	"Marital status"
		la var var9	"Education level: general"
		la var var10	"Education level: technical"
		la var var11	"For codes 06, 07, 08, 10 to 16 in col.7: class/grade completed"
		la var var12	"For codes 06, 07, 08, 10 to 16 in col.7: year(s ) of education completed after the class/grade recorded in col.9"
		la var var13	"For persons of age 3 to 35 years status of enrolment"
		la var var14	"For persons of age 5 years and above: whether able to operate a computer"
		la var var15	"For persons of age 5 years and above: whether able to use internet"
		la var var16	"For persons of age 5 years and above: whether used internet during last 30 days"
		la var var17	"For persons of age 12 to 59 years, whether receiving/received any vocational/technical training"
		la var var18	"Whether having certificate of disability"
		la var var19	"If 1 in col. 16, type of disability as per the certificate"
		la var var20	"Blank"
		la var var21	"NSS"
		la var var22	"NSC"
		la var var23	"MULT"
	
	}	
	
	if `f' == 5 {
	
		*Sch. 25.2 :   LEVEL - 05 (Block 5)	
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no. (as in col. 1, block 4)"
		la var var5	"Age (as in col. 5, block 4)"
		la var var6	"Age at entry in school(years)"
		la var var7	"Language mainly spoken at home"
		la var var8	"Medium of instruction"
		la var var9	"Level of current enrolment in the basic course"
		la var var10	"Course currently attending"
		la var var11	"For codes 08,10 to 16 in item 5, type of course currently attaneding"
		la var var12	"Duration of the current academic year of the course currently attending (in months)"
		la var var13	"Whether present class/ grade/ year of study is same as that of previous year"
		la var var14	"Type of institution"
		la var var15	"Nature of institution"
		la var var16	"Reason for attending private institution"
		la var var17	"Reason for attending private institution"
		la var var18	"For persons with disabilities whether attending/attended special school"
		la var var19	"Whether the student received free education"
		la var var20	"Whether tution fee is paid/payable"
		la var var21	"For codes 2, 3 in item 16, reason for tuition fee partly paid/partly payable or tuition fee not paid/not payable"
		la var var22	"For codes 1 to 6 or 9 in item 17, amount of tuition fee waived(Rs.)"
		la var var23	"Received scholarship/ stipend"
		la var var24	"For code 1 in item 19 amount received (Rs.)"
		la var var25	"For code 1 in item 19 type of scholarship/stipend/reimbursement"
		la var var26	"For code 1 in item 19 agency awarding the scholarship/stipend/reimbursement"
		la var var27	"Whether received free/subsidised textbooks"
		la var var28	"Whether received free/subsidised stationery"
		la var var29	"Whether free mid-day meal/ tiffin/ nutritionis  provided by the institution"
		la var var30	"For 1 in item 25, agency providing free mid-day meal/tiffin/nutrition"
		la var var31	"Mode of transport"
		la var var32	"Whether concession received"
		la var var33	"Distance(d) of educational institution from place of residence"
		la var var34	"Whether changed educational institution during last 365 days"
		la var var35	"Whether taking/taken private coaching"
		la var var36	"Blank"
		la var var37	"NSS"
		la var var38	"NSC"
		la var var39	"MULT"
	}
	
	if `f' == 6 {
	
		*Sch. 25.2 :   LEVEL - 06 (Block 6)
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no. (as in item 1, block 5)"
		la var var5	"Age (as in item 2, block 5)"
		la var var6	"Course fee (Rs.)"
		la var var7	"Books, stationery and uniform (Rs.)"
		la var var8	"Transport (Rs.)"
		la var var9	"Private coaching (Rs.)"
		la var var10	"Other expenditure (Rs.)"
		la var var11	"Total expenditure (Rs.): Items 3 to 7"
		la var var12	"Source of funding the expenditure on the basic course during the current academic year: first major source"
		la var var13	"Source of funding the expenditure on the basic course during the current academic year: second major source"
		la var var14	"Expenditure on education on courses other than basic course(Rs.)"
		la var var15	"Expenditure on preparation for higher/additional studies(Rs.)"
		la var var16	"Whether worked for 30 days or more"
		la var var17	"For code 1 in col. 13, status of the economic activity"
		la var var18	"For 2 in col. 13, whether seeking/ was available for work"
		la var var19	"Blank"
		la var var20	"NSS"
		la var var21	"NSC"
		la var var22	"MULT"
	
	} 
	
	if `f' == 7 {
	
		*Sch. 25.2 :   LEVEL - 07 (Block 7)
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no. (as in col. 1, block 4)"
		la var var5	"Age (as in col. 5, block 4)"
		la var var6	"Whether ever attended"
		la var var7	"Age at first enrollment in school (years)"
		la var var8	"Level of last enrolment"
		la var var9	"Type of education of the course last attended"
		la var var10	"Whether completed the level last enrolled"
		la var var11	"if the last class attended was class XII or below, grade/ class completed"
		la var var12	"Age when last attended/ last enrolled (years)"
		la var var13	"Type of institution last attended"
		la var var14	"Whether preparing/ prepared for higher/ additional studies during last 365 days"
		la var var15	"Expenditure incurred on preparation for higher/ additional studies(Rs.)"
		la var var16	"Major reason for never enrolled/ ever enrolled but currently not attending"
		la var var17	"Blank"
		la var var18	"NSS"
		la var var19	"NSC"
		la var var20	"MULT"
	}
	
	if `f' == 8 {
	
		*Sch. 25.2 :   LEVEL - 08 (Block 8)	
		*srl. no.	        Item
			
		la var var1	"Common-ID"
		la var var2	"Level"
		la var var3	"Filler"
		la var var4	"Person serial no. (as in col. 1, block 4)"
		la var var5	"Age (as in col. 5, block 4)"
		la var var6	"Field of training"
		la var var7	"Duration of training"
		la var var8	"Type of training"
		la var var9	"Source of funding the training"
		la var var10	"Whether the training completed during last 365 days"
		la var var11	"Blank"
		la var var12	"NSS"
		la var var13	"NSC"
		la var var14	"MULT"
	
	}
	

	
	save "${path}\1.India2017\2.Data\1.Consumption\R75252L0`f'.dta", replace

}


