/****************************************************************************

This do-file creates nielsen_2004_2016.dta

Final version: David Dorn, May 12, 2020

*****************************************************************************/

** preamble **
set more off
clear all
// access to ado file
sysdir set PLUS input/code/ado


** loop over Nielsen files, save as stata files **
local myFiles : dir "input/raw/nielsen" files "*.csv"
local fileCount = 1
foreach file in `myFiles'  {

	import delimited input/raw/nielsen/`file', delimiter(comma) clear
	
	drop in 1
	assert v1[1] == "Geography"
	drop in 1
	
	rename v1 geography
	rename v2 dataStream
	rename v3 customRange
	rename v4 dates
	rename v5 daypart
	rename v6 channel
	rename v7 affiliation
	rename v8 time
	rename v9 characteristic
	rename v10 demographic
	rename v11 metrics
	rename v12 rating
	rename v13 share
	rename v14 intab
	rename v15 installed
	rename v16 impressions
	rename v17 sumOfWeights
	rename v18 indicator
	
	local numObs = _N
	drop in `numObs'	
	
	
	** prepare to reshape **
	replace channel = subinstr(channel, " ", "", .)
	keep geography dates channel characteristic rating share intab imp sumOfWeights installed
	drop if channel == ""
	assert rating == "0.000" if channel == "MSNA"
	drop if channel == "MSNA"
	rename rating rtg
	
	** reduce the data to one record per geo/date/characteristic **
	*		data is originally one record per geo/date/char/channel
	reshape wide rtg share imp intab sumOfWeights installed, i(geo dates characteristic) j(channel) string
	
	gen file = `fileCount'
	
	save runtime/datasets/stata_`fileCount'.dta, replace
	
	local fileCount = `fileCount' + 1

}


** append Nielsen files together **
local appendCount = `fileCount' - 1
while `appendCount' > 1  {
	
	local appendCount = `appendCount' - 1
	append using runtime/datasets/stata_`appendCount'.dta
	
}

** ensure there are no duplicates in the files **
duplicates tag geo char dates, gen(dup1)
sum dup1
assert `r(mean)' == 0
drop dup1


** create date variables **
split date, gen(date_)
drop dates
rename date_1 month
rename date_2 year
replace year = "20" + year
replace month = "2" if month == "FEB"
replace month = "5" if month == "MAY"
replace month = "7" if month == "JUL"
replace month = "11" if month == "NOV"


** remove commas and destring variables **
foreach vari of varlist _all  {
	tostring `vari', force replace
	replace `vari' = subinstr(`vari', ",", "",.)
}
destring rtg* share* imp* intab* sumOfWeights* installed* month year file, replace
encode characteristic, gen(char)
drop characteristic
rename char characteristic


* add on dma for 2017
preserve
import excel runtime/crosswalk/dma_shifting_boundaries.xlsx, firstrow clear
foreach vari of varlist _all   {
	tostring(`vari'), replace
	replace `vari' = upper(`vari')
}
drop if geo == ""
drop dma2017 dma2016 dma2015
tempfile cw2
save `cw2', replace
forvalues year = 18(-1)15  {
import excel runtime/crosswalk/cw_county_dma_new_`year'.xls, firstrow clear
drop if DMAcode == .
rename DMAName dma20`year'
gen geography = CountyName + " CO. " + State
replace geo = "OGLALA LAKOTA CO. SD" if geo == "SHANNON CO. SD"
keep geo dma
tempfile cw`year'
save `cw`year'', replace
}
use `cw2', clear
merge 1:1 geo using `cw15', nogen
merge 1:1 geo using `cw16', nogen
merge 1:1 geo using `cw17', nogen
merge 1:1 geo using `cw18', nogen
forvalues yr = 2015(-1)2005 {
	local ym1 = `yr' - 1
	gen change`yr' = (dma`ym1' != dma`yr' & dma`ym1' != "" & dma`yr' != "")
}
forvalues yr = 2018(-1)2005  {
	local ym1 = `yr' - 1
	local yp1 = `yr' + 1
	replace dma`ym1' = dma`yr' if dma`ym1' == ""
}
drop change*
reshape long dma, i(geo) j(year_TV)
drop if year == 2018
tempfile crosswalk
save `crosswalk', replace
restore

replace geo = "Oglala Lakota Co. SD" if geo == "Shannon Co. SD"
replace geo = upper(geo)
gen year_TV = year
replace year_TV = year + 1 if month == 11
merge m:1 geography year_TV using `crosswalk', gen(m10)


// align names with the spreadsheet that crosswalks to fips scores
replace geo = upper(geo)
preserve
import delimited runtime/crosswalk/county_nameChange_manual.csv, clear
	drop in 1  // this deletes title row of spreadsheet
	rename v1 geography
	rename v2 newGeo
	
	tempfile crosswalk
	save `crosswalk', replace
restore
merge m:1 geography using `crosswalk'
assert _m != 2 // any manual corrections should match data
drop _m
replace geo = newGeo if newGeo != ""
drop newGeo


// add on fips scores
preserve
import delimited runtime/crosswalk/county_to_fips_raw.csv, clear
	rename v1 state
	rename v2 stateCode
	rename v3 countyCode
	rename v4 geography

	drop if state == "PR"  //drop Puerto Rico from analysis
	replace geography = subinstr(geography, "County", "Co.", .)
	replace geography = subinstr(geography, "Parish", "Co.", .)
	replace geography = subinstr(geography, "Municipality", "Co.", .)
	replace geography = subinstr(geography, "St.", "St", .)
	
	replace geo = geo + " " + state
	replace geo = upper(geo)
	gen fips = stateCode * 1000 + countyCode
	keep geo fips	
	
	tempfile crosswalk
	save `crosswalk', replace
restore

merge m:1 geography using `crosswalk'
assert geo == "ROCHESTR CITY CO. NY" if _m == 1 // Rochester city is counted 
						//separately in nielsen data, but
						// doesn't have own fips code. already counted in data
						// drop oglala laokta co. sd 
keep if _m == 3  // not all counties present in nielsen data (i.e. _m == 2)
drop _m


// add on commuting zone codes
rename fips cty_fips
merge m:1 cty_fips using runtime/crosswalk/cw_cty_czone.dta
assert _m != 1
keep if _m == 3
drop _m


// drop AK, DC, HI
gen state_fips=floor(cty_fips/1000)
drop if state_fips==2 | state_fips==11 | state_fips==15
drop state_fips


// create year files
forvalues year = 2004(1)2017  {
	preserve
	keep if year == `year'
	save runtime/datasets/nielsen_`year', replace
	restore
}


****************************************************************************************
* Compiling Nielsen data at the czone x year-month x population group level
****************************************************************************************

* preparing and appending the Nielsen data
forvalues y=2004(4)2016 {
   use ../Nielsen/runtime/datasets/nielsen_`y'.dta, clear
   * keep only 2 race x 3 age group cells
   drop if characteristic==1 /* all TV Households */
   drop if characteristic==6 /* white HOH 18-24 */
   drop if characteristic==2 /* nonwhite HOH 18-24 */
   * dummy variables for race-age groups
   gen group_w_1834=(characteristic==7)
   gen group_w_3554=(characteristic==8)
   gen group_w_55up=(characteristic==9)
   gen group_nw_1834=(characteristic==3)
   gen group_nw_3554=(characteristic==4)
   gen group_nw_55up=(characteristic==5)
   * indicate year and month
   tab year
   tab month
   assert month==2 | month==5 | month==7 | month==11
   * dummy variables for year-month
   gen t_`y'02=1 if month==2
   gen t_`y'05=1 if month==5
   gen t_`y'07=1 if month==7
   gen t_`y'11=1 if month==11
   * indicator for CZ x group x year-month
   gen double czgroupmth=10000*czone+100*characteristic+month
   * create unique sum-of-weights and intab variable (SOW, intab do not vary by channel)
   gen SOW=sumOfWeightsFXNC
   gen INTAB=intabCNN
   * create HUT (households using TV variable) - this varies across TV stations due to rounding error
   * use maximum value
   gen HUT=0 
   foreach k in CNN FXNC HLN MSNBC {
      gen HUT_`k'=100*impressions`k'/share`k'
	  replace HUT=HUT_`k' if HUT_`k'!=. & HUT_`k'>HUT
   }
   
   * sanity checks
   egen totHH=total(INTAB)
   summ totHH
   drop totHH
   egen totSOW=total(SOW)
   summ totSOW
   drop totSOW
   egen totimpFX=total(impressionsFXNC)
   summ totimpFX
   drop totimpFX
   
   * keep relevant variables
   keep cty_fips czone group_* impressions* INTAB SOW HUT year month t_* czgroupmth
   * collapse by CZ x group x year-month
   collapse (mean) czone group_* year month t_* (sum) impressions* INTAB SOW HUT, by(czgroupmth)
   
   * compute shares and ratings
   foreach k in CNN FXNC HLN MSNBC {
      gen rtg`k'=100*impressions`k'/SOW
	  replace rtg`k'=0 if impressions`k'==0
	  gen shr`k'=100*impressions`k'/HUT
	  replace shr`k'=0 if impressions`k'==0
	  replace shr`k'=0 if HUT==0
	  assert shr`k'<=100
   }
   * sanity checks
   egen totHH=total(INTAB)
   summ totHH
   drop totHH
   egen totSOW=total(SOW)
   summ totSOW
   drop totSOW
   egen totimpFX=total(impressionsFXNC)
   summ totimpFX
   drop totimpFX

   * compute total households by czone-month
   by czone month, sort: egen INTAB_CZ=total(INTAB)
   * save temporary
   save temp_nielsen_`y'.dta, replace
}
use temp_nielsen_2004.dta, clear
erase temp_nielsen_2004.dta
forvalues y=2008(4)2016 {
	append using temp_nielsen_`y'.dta
	erase temp_nielsen_`y'.dta
}

* set value of time dummies to zero if not equal to one
foreach var of varlist t_* {
   replace `var'=0 if `var'!=1
}


****************************************************************************************
* Merge with CZ-level RHS variables
* (keep only 48 mainland states)
****************************************************************************************

sort czone
merge czone using ../CZ/cz_variables.dta
assert _merge!=2
keep if _merge==3
drop _merge


*******************************************************************
* Variable definitions
*******************************************************************

foreach var of varlist d_imp_usch_pd d_imp_otch_lag_pd l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 {
   * define post-period as 2007 or later
   gen t2_`var'=`var'*(year>=2007)
}

* time trend by group
foreach var of varlist group_* {
   gen t2_`var'=`var'*(year>=2007)
}  

* time trend by Census division
foreach var of varlist reg_* {
   gen t2_`var'=`var'*(year>=2007)
}  

* combined rating
gen rtgALL=rtgFXNC+rtgCNN+rtgMSNBC
* combined rating with HLN
gen rtgALL4=rtgFXNC+rtgCNN+rtgMSNBC+rtgHLN
* combined share
gen shrALL=shrFXNC+shrCNN+shrMSNBC

* market share
foreach k in FXNC CNN MSNBC {
   gen mktsh`k'=100*(rtg`k')/(rtgFXNC+rtgCNN+rtgMSNBC)
   gen mktsh4`k'=100*(rtg`k')/(rtgFXNC+rtgCNN+rtgMSNBC+rtgHLN)
}

* race interactions
foreach var of varlist t2_d_imp_usch_pd t2_d_imp_otch_lag_pd {
   * define post-period as 2007 or later
   gen w`var'=`var'*(group_nw_1834+group_nw_3554+group_nw_55up==0)
   gen nw`var'=`var'*(group_nw_1834+group_nw_3554+group_nw_55up==1)
}

* group interactions
foreach var of varlist t2_d_imp_usch_pd t2_d_imp_otch_lag_pd {
   * define post-period as 2007 or later
   gen w1834_`var'=`var'*group_w_1834
   gen w3554_`var'=`var'*group_w_3554
   gen w55up_`var'=`var'*group_w_55up
   gen nw1834_`var'=`var'*group_nw_1834
   gen nw3554_`var'=`var'*group_nw_3554
   gen nw55up_`var'=`var'*group_nw_55up
}

* drop baseline categories
drop t2_reg_neweng
drop group_w_1834
drop t2_group_w_1834

* keep relevant variables
keep czone t_* group_* rtgALL mktshFXNC mktshCNN mktshMSNBC SOW t2_d_imp_usch_pd t2_d_imp_otch_lag_pd t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t2_group* w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd
order czone t_* group_* rtgALL mktshFXNC mktshCNN mktshMSNBC SOW t2_d_imp_usch_pd t2_d_imp_otch_lag_pd t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t2_group* w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd
foreach var of varlist czone t_* group_* rtgALL mktshFXNC mktshCNN mktshMSNBC SOW t2_d_imp_usch_pd t2_d_imp_otch_lag_pd t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t2_group* w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd {
   label variable `var' ""
}

save nielsen_2004_2016.dta, replace
