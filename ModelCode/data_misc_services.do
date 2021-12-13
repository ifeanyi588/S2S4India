*****************************************************************************************************
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
** Modified	 7/31/2017
                                                                                             
******************************************************************************************************
   	
	local all 61 66 68 72
	local no61 66 68 72
	local no72 61 66 68
	  	
************************************ Data for Miscellaneous Consumer Services **********************************************
/* 2014-15 
* Blocks 5 and 6 for transportation expenditures ; Block 7 for other miscellaneous services
clear
use "$data72\NSS72_Sch1-T1_bk_5_6_7_8_9.dta", clear
save "$data72\nss72_10_1.dta", replace

* 2011-12
* Block 10 for miscellaneous consumer goods and services
clear
datalibweb, coun(IND) y(2011) t(SARRAW) sur(IND_2011_NSS68-SCH1.0-T1_v01_M) filen(NSS68_Sch1-T1_bk_10.dta)
save "$data68\nss68_10_1.dta", replace

* 2009 - 2010
* Block 10 for miscellaneous consumer goods and services
clear
datalibweb, coun(IND) y(2009) t(SARRAW) sur(IND_2009_NSS66-SCH1.0-T1_v01_M) filen(NSS66_Sch1_Type1_bk_10.dta)
save "$data66\nss66_10_1.dta", replace

* 2004-05
datalibweb, coun(IND) y(2004) t(SARRAW) sur(IND_2004_NSS61-SCH1.0_v01_M) filen(bk_10.dta)
save "$data61\nss61_10_1.dta", replace
*/
************************************Creating Variables **********************************************
***** 2014-15 ***** 
clear
use "$data72\nss72_10_1.dta"
rename B1_v05 sector
rename B1_v07 district
rename B1_v02 round
rename B1_v06 state_reg
rename B5_v03 totexp
keep hhwt sector district round state_reg ID state B5_v02 totexp
reshape wide totexp, i(ID) j(B5_v02)
foreach var in 0 500	501	502	503	504	505	506	507	509	510	511	512	513	514	515	516	517	518	520	521	522	523	524	529	530	531	532	533	539	540	541	542	549	550	551	554	559	560	561	569	570	571	572	573	579	580	581	582	583	589	590	591	592	599	600	601	602	603	604	605	606	607	608	610	619	629	630	631	632	639	640	641	649	650	651	652	653	659	669	679	680	681	682	683	684	685	686	687	689	690	691	692	693	694	699{
replace totexp`var'=0 if totexp`var'==.
* Calculating yearly expenditures
replace totexp`var'=totexp`var'*(365/30) 
}

* Transport = overnight round journeys + round journeys + services incidental to transport (car parking charges + porter charges) 
gen transp = totexp509 + totexp529 + totexp649
* Transport = airfare + railway fare + bus/tram fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp1 = totexp509 - totexp507 +totexp529 - totexp524  + totexp641

* DOES NOT INCLUDE BUS/TRAM FARE and SCHOOL BUS/VAN. Transport = airfare + railway fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp2 =  totexp509 - totexp507 +totexp529 - totexp524  + totexp641 - totexp502 - totexp512 - totexp513 - totexp514 - totexp515 - totexp516 - totexp517


* Miscellaneous services: domestic help, barber & beauty , washerman, laundry, priest, grinding, tailor
gen hh = totexp530 + totexp531 + totexp533 + totexp549 + totexp560 + totexp590 + totexp651 + totexp669  

* Information/Communication: telephones, internet, fax, email 
gen communi = totexp589

* Recreation: photography, hire of VCD/DVD, club/gym fees, cinema/theatre, fairs/picnics.
gen rec = totexp600 + totexp601 + totexp602 + totexp603 + totexp604  

drop totexp*
gen year=2014
save "$data72\nss72_10_2.dta", replace 

*****  2011-12 ***** 
use "$data68\nss68_10_1.dta", clear
rename B10_v03 totexp
rename B1_v05 sector
rename B1_v07 district
rename B1_v02 round
rename B1_v06 state_reg
keep hhwt totexp sector district round state_reg ID B10_v02 state
reshape wide totexp, i(ID) j(B10_v02)

foreach var in 420	421	422	423	424	429	430	431	432	433	434	435	436	437	438	439	440	441	442	443	444	445	449	450	451	452	453	454	455	456	457	459	460	461	462	463	464	465	466	467	468	470	471	472	473	479	480	481	482	483	484	485	486	487	488	490	491	492	493	494	495	496	497	499	500	501	502	503	504	505	506	507	508	510	511	512	513	519	520	521	522	523	529	539	540	541	549{
replace totexp`var'=0 if totexp`var'==.
replace totexp`var'=totexp`var'*(365/30) 
}

* Transport = all conveyance 
gen transp = totexp519 
* Transport = airfare + railway fare + bus/tram fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp1 = totexp500 + totexp501+ totexp502 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 + totexp512

* DOES NOT INCLUDE BUS/TRAM FARE and SCHOOL BUS/VAN. Transport = airfare + railway fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp2 = totexp500 + totexp501 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 


* Miscellaneous services: domestic help, barber & beauty , washerman, laundry, priest, grinding, tailor
gen hh = totexp480 + totexp481 + totexp482 + totexp483 + totexp484 + totexp485 + totexp486 + totexp492  

* Information/Communication: telephones, internet, fax, email 
gen communi = totexp487 + totexp488 + totexp490 + totexp496

* Recreation : cinema/theatre, fairs/picnics, club fees,photography, hire of VCD/DVD
gen rec = totexp430 + totexp431 + totexp433 + totexp435  + totexp436

drop totexp*
gen year=2011
save "$data68\nss68_10_2.dta", replace 

*****  2009 - 2010 ***** 

clear
use "$data66\nss66_10_1.dta"
rename hhid ID
rename S1B10_v03 totexp
rename S1B1_v05 sector
rename S1B1_v07 district
rename S1B1_v02 round
rename S1B1_v06 state_reg 
keep hhwt totexp sector district round state_reg ID S1B10_v02 state
reshape wide totexp, i(ID) j(S1B10_v02)

foreach var in 420	421	422	423	424	429	430	431	432	433	434	435	436	437	438	439	440	441	442	443	444	445	449	450	451	452	453	454	455	456	457	459	460	461	462	463	464	465	466	467	468	470	471	472	473	479	480	481	482	483	484	485	486	487	488	490	491	492	493	494	495	496 499	500	501	502	503	504	505	506	507	508	510	511	512	513	519	520	521	522	523	529	539	540	541	549{
replace totexp`var'=0 if totexp`var'==.
replace totexp`var'=totexp`var'*(365/30) 
}

* transp = all conveyance 
gen transp = totexp519 
* Transport = airfare + railway fare + bus/tram fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp1 = totexp500 + totexp501+ totexp502 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 + totexp512

* DOES NOT INCLUDE BUS/TRAM FARE and SCHOOL BUS/VAN. Transport = airfare + railway fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp2 = totexp500 + totexp501 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 


* Miscellaneous services: domestic help, barber & beauty , washerman, laundry, priest, grinding, tailor
gen hh = totexp480 + totexp481 + totexp482 + totexp483 + totexp484 + totexp485 + totexp486 + totexp492  

* Information/Communication: telephones, postal (no internet)
gen communi = totexp487 + totexp488 + totexp490 

* Recreation: cinema/theatre, fairs/picnics, club fees,photography, hire of VCD/DVD
gen rec = totexp430 + totexp431 + totexp433 + totexp435  + totexp436

drop totexp*
gen year=2009
save "$data66\nss66_10_2.dta", replace 


***** 2004-05 ***** 
clear
use "$data61\nss61_10_1.dta"
rename hhid ID
rename weight hhwt
rename value totexp
tostring fsu hamlet secstage hhsno, replace
gen z=0
tostring z, replace
gen newhhsno = hhsno if strlen(hhsno)==2
replace newhhsno = z + hhsno if strlen(hhsno)==1
gen hhid=fsu+ hamlet+ secstage + newhhsno
drop z fsu hamlet secstage hhsno newhhsno ID
rename hhid ID

keep hhwt totexp sector district round stat_reg ID item_cod
reshape wide totexp, i(ID) j(item_cod)
foreach var in 420	421	422	423	424	429	430	431	432	433	434	435	436	437	438	439	440	441	442	443	444	445	449	450	451	452	453	454	455	456	457	459	460	461	462	463	464	465	466	467	468	470	471	472	473	479	480	481	482	483	484	485	486	487	488	490	491	492	493	494	499	500	501	502	503	504	505	506	507	508	510	511	512	513	519	520	521	522	529	539	540	541	549{
replace totexp`var'=0 if totexp`var'==.
replace totexp`var'=totexp`var'*(365/30)*(1/100) 
}

* Transport = all conveyance 
gen transp = totexp519 
* DOES NOT INCLUDE ANY FUELS. Transport = airfare + railway fare + bus/tram fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp1 = totexp500 + totexp501+ totexp502 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 + totexp512

* DOES NOT INCLUDE ANY FUELS OR BUS/TRAM FARE and SCHOOL BUS/VAN. Transport = airfare + railway fare + taxi/autorickshaw fare + steamer/boat fare + rickshaw (hand-drawn & cycle) fare + horsecart fare + porter charges
gen transp2 = totexp500 + totexp501 + totexp503 + totexp504 + totexp505 + totexp506 + totexp507 


* Miscellaneous services: domestic help (no attendant) , barber & beauty , washerman, laundry, priest, grinding, tailor
gen hh = totexp480 + totexp481 + totexp482 + totexp483 + totexp484 + totexp485 + totexp491  

* Information/Communication: telephones, postal (no mobile phones; internet)
gen communi = totexp487 + totexp488 

* Recreation: cinema/theatre, fairs/picnics, club fees,photography, hire of VCD/DVD
gen rec = totexp430 + totexp431 + totexp433 + totexp435  + totexp436

drop totexp*
gen year=2004
save "$data61\nss61_10_2.dta", replace 

***** Calculating Real Expenditures and Converting to Yearly Expenditures *****
foreach y in `all'{
clear
use "$path\data\NSSO`y'\nss`y'_10_2.dta"
* Calculating in Real Terms (2011 Rs.)
merge m:1 year using "$path\data\CPI2011\cpi2011.dta"
keep if _merge==3
drop _merge
foreach var in transp transp1 transp2 hh communi rec{
* gen `var'y = `var'*(365/30)

gen `var'r = (`var')/cpiurban if sector==2
replace `var'r= (`var')/cpirural if sector==1
}
drop cpirural cpiurban
label var year "Survey Year (July to June)"
label var transpr "Transport Expenses (Year)"
label var transp1r "Transport Expenses (Year) no fuel"
label var transp2r "Transport Expenses (Year) no bus "
label var recr "Recreation Expenses (Year)" 
label var communir "Communication Expenses (Year)"  
label var hhr "Household Services Expenses (Year)"   
save "$path\data\NSSO`y'\nss`y'_10.dta", replace 
}


