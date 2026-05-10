/****************************************************************************

This do-file creates house_2002_2016.dta

Final version: David Dorn, May 12, 2020

*****************************************************************************/



****************************************************************************************
* PART 1: Outcomes at county x district level
****************************************************************************************

*******************************************************************
* County x district data - vote counts 2002-2010
*******************************************************************

foreach y in 2002 2004 2006 2008 2010 {
   use ../House/dta/Leip_`y'_final.dta, clear
   * adjust FIPS code for Miami-Dade
   replace fips=12025 if fips==12086
   * drop AK, HI
   drop if CD=="AK 1" | CD=="HI 1" | CD=="HI 2"
   * county FIPS
   gen cty_fips=fips
   * state FIPS
   gen state_fips=floor(cty_fips/1000)
   * save
   save Leip_`y'_temp.dta, replace
}

forvalues y=2002(2)2010 {
   use Leip_`y'_temp.dta, clear

   * assert unique county-district combinations
   egen tag=tag(fips CD)
   tab fips CD if tag==0
   drop if tag==0
   drop tag 

   * numerical district code
   gen cd=substr(CD,-2,2)
   destring cd, replace
   * state-district identifier
   gen statedistrict=100*state_fips+cd
   * define cell identifier
   gen cell_id=100*cty_fips+cd
   
   * sample selection: drop a small number of statewide/overseas votes in ME, and of Federal/Limited votes in RI
   drop if county=="Statewide" & state_fips==23
   drop if county=="Overseas" & state_fips==23
   drop if county=="Federal/Limited" & state_fips==44
   drop if county=="Federal" & state_fips==44
   
   * drop cells that always have zero votes (in NY 15, AZ 2, CO 1)
   drop if cell_id==3600515
   drop if cell_id==3608115 
   drop if cell_id==402502 
   drop if cell_id==800101 
   
   * correction for Vermont: count Bernie Sanders (Independent) as a Democrat in 2002/2004
   * (in 2002, no Democrat was running for Vermont; in 2004, Sanders won against a Democrat; as of 2006, the seat was held by Democrats)
   replace Democratic=Independent if state_fips==50 & `y'==2002
   replace Independent=0 if state_fips==50 & `y'==2002
   gen DemocraticX=Independent if state_fips==50 & `y'==2004
   replace Independent=Democratic if state_fips==50 & `y'==2004
   replace Democratic=DemocraticX if state_fips==50 & `y'==2004
   drop DemocraticX
   
   * correction for Broward county FL 21 in 2010: set total votes from 0.001 to 0
   summ TotalVote if TotalVote>0, detail
   replace TotalVote=0 if TotalVote==0.001
   
   * variable definition: total votes in cell
   replace Democratic=0 if Democratic==.
   replace Republican=0 if Republican==.
   gen totvote_`y'=TotalVote
   gen totvoter_`y'=Republican
   gen totvoted_`y'=Democratic
   
   * variable definition: gross and net vote shares
   gen shgd_`y'=Democratic/TotalVote
   gen shgr_`y'=Republican/TotalVote
   gen shnd_`y'=(Democratic)/(Democratic+Republican)
   gen shnr_`y'=(Republican)/(Democratic+Republican)

   * variable definition: district-level overall, Democrat, Republican, third party
   by statedistrict, sort: egen totvote_distr_`y'=total(TotalVote)
   by statedistrict, sort: egen totdvote_distr_`y'=total(Democratic)
   by statedistrict, sort: egen totrvote_distr_`y'=total(Republican)
   gen totovote_distr_`y'=totvote_distr_`y'-totdvote_distr_`y'-totrvote_distr_`y'  
   
   * variable definition: 
   * unopposed (winner gets all votes)
   * twoparty (both a Republican and a Democrat competing)
   * thirdparty (at least one Independent or Other Party Candidate competing) 
   gen unopposed_`y'=(totdvote_distr_`y'==totvote_distr_`y')
   replace unopposed_`y'=1 if (totrvote_distr_`y'==totvote_distr_`y')
   gen twoparty_`y'=(totdvote_distr_`y'>0 & totrvote_distr_`y'>0)  
   gen thirdparty_`y'=(totovote_distr_`y'>0)   
   
   * vote share at the district level
   * bins for electoral margins in district
   gen shnr_distr_`y'=(totrvote_distr_`y'/(totrvote_distr_`y'+totdvote_distr_`y'))
   gen r10_`y'=(shnr_distr_`y'>=.0 & shnr_distr_`y'<.10)
   gen r20_`y'=(shnr_distr_`y'>=.10 & shnr_distr_`y'<.20)
   gen r30_`y'=(shnr_distr_`y'>=.20 & shnr_distr_`y'<.30)
   gen r40_`y'=(shnr_distr_`y'>=.30 & shnr_distr_`y'<.40)
   gen r50_`y'=(shnr_distr_`y'>=.40 & shnr_distr_`y'<.50)
   gen r60_`y'=(shnr_distr_`y'>=.50 & shnr_distr_`y'<.60)
   gen r70_`y'=(shnr_distr_`y'>=.60 & shnr_distr_`y'<.70)
   gen r80_`y'=(shnr_distr_`y'>=.70 & shnr_distr_`y'<.80)
   gen r90_`y'=(shnr_distr_`y'>=.80 & shnr_distr_`y'<.90)
   gen r100_`y'=(shnr_distr_`y'>=.90 & shnr_distr_`y'<=1.00)   

   * keep relevant variables
   keep cell_id state_fips cty_fips cd CD statedistrict *_`y' 
   sort cell_id
   save temp_`y'.dta, replace
   erase Leip_`y'_temp.dta
}

* year-specific corrections for unopposed races: 2002
local y=2002
use temp_`y'.dta, clear

* districts with zero-vote unopposed races (note: AR-3 is unopposed in 2002 but has positive number of votes in all counties except 5009)
tab CD if totvote_`y'==0 & unopposed_`y'==1

* set vote share to 1 for winning party in unopposed zero-vote races
foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
  replace `var'=1 if CD=="FL 11" | CD=="FL 20" & `y'==2002 
  replace `var'=0 if CD=="AR 3" | CD=="FL 10" | CD=="FL 12" | CD=="FL 14" | CD=="FL 21" & `y'==2002 
}
foreach var of varlist shgr_`y' shnr_`y' shnr_distr_`y' r100_`y' {
   replace `var'=0 if CD=="FL 11" | CD=="FL 20" & `y'==2002
   replace `var'=1 if CD=="AR 3" | CD=="FL 10" | CD=="FL 12" | CD=="FL 14" | CD=="FL 21" & `y'==2002 
}

* assert no missing variable values 
foreach var of varlist *_`y' {
   assert `var'!=.
}

save temp_`y'.dta, replace

* year-specific corrections for unopposed races: 2004
local y=2004
use temp_`y'.dta, clear

* districts with zero-vote unopposed races 
tab CD if totvote_`y'==0 & unopposed_`y'==1

* set vote share to 1 for winning party in unopposed zero-vote races
foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
  replace `var'=1 if CD=="AR 4" | CD=="FL 19" | CD=="FL 23" & `y'==2004
  replace `var'=0 if CD=="FL 7" | CD=="FL 24" | CD=="FL 25" | CD=="LA 4" & `y'==2004
}
foreach var of varlist shgr_`y' shnr_`y' shnr_distr_`y' r100_`y' {
  replace `var'=0 if CD=="AR 4" | CD=="FL 19" | CD=="FL 23" & `y'==2004
  replace `var'=1 if CD=="FL 7" | CD=="FL 24" | CD=="FL 25" | CD=="LA 4" & `y'==2004
}

* assert no missing variable values 
foreach var of varlist *_`y' {
   assert `var'!=. 
}

save temp_`y'.dta, replace


* year-specific corrections for unopposed races: 2006
local y=2006
use temp_`y'.dta, clear

* districts with zero-vote unopposed races 
tab CD if totvote_`y'==0 & unopposed_`y'==1

* set vote share to 1 for winning party in unopposed zero-vote races
foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
  replace `var'=1 if CD=="FL 2" | CD=="FL 3" | CD=="FL 19" | CD=="FL 20" | CD=="FL 23" & `y'==2006
}
foreach var of varlist shgr_`y' shnr_`y' shnr_distr_`y' r100_`y' {
  replace `var'=0 if CD=="FL 2" | CD=="FL 3" | CD=="FL 19" | CD=="FL 20" | CD=="FL 23" & `y'==2006
}

* replace cell vote share with district vote share if no votes in the cell
* (in 2006, one overlap in CO 1 has a total of 1 vote, but none for republicans or democrats)
replace shnd_`y'=1-shnr_distr_`y' if shnd_`y'==. 
replace shnr_`y'=shnr_distr_`y' if shnr_`y'==. 

* assert no missing variable values 
foreach var of varlist *_`y' {
   assert `var'!=. 
}

save temp_`y'.dta, replace


* year-specific corrections for unopposed races: 2008
local y=2008
use temp_`y'.dta, clear

* districts with zero-vote unopposed races 
tab CD if totvote_`y'==0 & unopposed_`y'==1

* set vote share to 1 for winning party in unopposed zero-vote races
foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
  replace `var'=0 if CD=="LA 5" & `y'==2008
  replace `var'=1 if CD=="LA 3" | CD=="FL 3" | CD=="FL 17" | CD=="AR 1" & `y'==2008
}
foreach var of varlist shgr_`y' shnr_distr_`y' shnr_`y' r100_`y' {
  replace `var'=1 if CD=="LA 5" & `y'==2008
  replace `var'=0 if CD=="LA 3" | CD=="FL 3" | CD=="FL 17" | CD=="AR 1" & `y'==2008
}

* replace cell vote share with district vote share if no votes in the cell
* (in 2008, one overlap in AL 3 has zero votes)
replace shnd_`y'=1-shnr_distr_`y' if shnd_`y'==. 
replace shnr_`y'=shnr_distr_`y' if shnr_`y'==. 
replace shgd_`y'=(totdvote_distr_`y'/totvote_distr_`y') if shgd_`y'==. 
replace shgr_`y'=(totrvote_distr_`y'/totvote_distr_`y') if shgr_`y'==. 

* assert no missing variable values 
foreach var of varlist *_`y' {
   assert `var'!=. 
}

save temp_`y'.dta, replace

* year-specific corrections for unopposed races: 2010
local y=2010
use temp_`y'.dta, clear

* districts with zero-vote unopposed races 
tab CD if totvote_`y'==0 & unopposed_`y'==1

* set vote share to 1 for winning party in unopposed zero-vote races
foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
  replace `var'=0 if CD=="FL 21" | CD=="LA 7" | CD=="OK 4" & `y'==2010
}
foreach var of varlist shgr_`y' shnr_`y' shnr_distr_`y' r100_`y' {
  replace `var'=1 if CD=="FL 21" | CD=="LA 7" | CD=="OK 4" & `y'==2010
}

* assert no missing variable values 
foreach var of varlist *_`y' {
   assert `var'!=. 
}

save temp_`y'.dta, replace



*******************************************************************
* Correct 2010 voting data for rezoning in ME, PA, TX (2004) and GA, TX (2006)
*******************************************************************

*** Maine and Pennsylvania in 2004-2010

forvalues y=2004(2)2010 {
	use temp_`y'.dta, clear
	keep if state_fips==23 | state_fips==42
	
	* total votes in ME, PA
	egen totvoteMEPA=total(totvote_`y')
	summ totvoteMEPA
	drop totvoteMEPA
	
	gen cd109=cd
	destring cd109, replace
	gen ctycd109=100*cty_fips+cd109
	sort ctycd109
	save temp_MEPA_`y'.dta, replace
	
	use ../House/crosswalks/cw_ctycd108_ctycd109.dta, clear
	sort ctycd109
	* merge ME, PA but not TX
	merge ctycd109 using temp_MEPA_`y'.dta
	assert _merge==3 if ctycd109<4800000 
	keep if _merge==3
	drop _merge
	
    gen cd108=ctycd108-100*floor(ctycd108/100)
	gen statedistr108=100*state_fips+cd108
	
    * compute vote counts by (108th district * county) cell - actual number of votes in geographic area
    foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd109_ctycd108)
	   replace `var'=`var'_wt
	}
	* compute weighted dummy indicators by (108th district * county) cell - index of post-rezoning districts
	foreach var of varlist shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd108_ctycd109)
	   replace `var'=`var'_wt
	}
	
	* collapse to (108th district * county)
    collapse (mean) totvote_`y' totvoted_`y' totvoter_`y' shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' state_fips cty_fips statedistr108 cd108, by (ctycd108) 
	
	* keep district indicators of the 108th Congress
	rename cd108 cd
	rename statedistr108 statedistrict
	gen double cell_id=100*cty_fips+cd
	drop ctycd108

    * compute district-level overall Democrat and Republican votes
    by statedistrict, sort: egen totvote_distr_`y'=total(totvote_`y')
    by statedistrict, sort: egen totrvote_distr_`y'=total(totvoter_`y')
    by statedistrict, sort: egen totdvote_distr_`y'=total(totvoted_`y')
	
	* total votes in ME, PA
	egen totvoteMEPA=total(totvote_`y')
	summ totvoteMEPA
	drop totvoteMEPA
	
	save temp_MEPA_`y'.dta, replace
}

*** Texas in 2004

foreach y in 2004 {
	use temp_`y'.dta, clear
	keep if state_fips==48
	
	* total votes in TX
	egen totvoteTX=total(totvote_`y')
	summ totvoteTX
	drop totvoteTX
	
	gen cd109=cd
	destring cd109, replace
	gen ctycd109=100*cty_fips+cd109
	sort ctycd109
	save temp_TX_`y'.dta, replace

	use ../House/crosswalks/cw_ctycd108_ctycd109.dta, clear
	sort ctycd109
	* merge TX but not ME, PA
	merge ctycd109 using temp_TX_`y'.dta
	assert _merge==3 if ctycd109>=4800000 
	keep if _merge==3
	drop _merge
	
    gen cd108=ctycd108-100*floor(ctycd108/100)
	gen statedistr108=100*state_fips+cd108
	
    * compute vote counts by (108th district * county) cell - actual number of votes in geographic area
    foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd109_ctycd108)
	   replace `var'=`var'_wt
	}
	* compute weighted dummy indicators by (108th district * county) cell - index of post-rezoning districts
	foreach var of varlist shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd108_ctycd109)
	   replace `var'=`var'_wt
	}
	
	* collapse to (108th district * county)
    collapse (mean) totvote_`y' totvoter_`y' totvoted_`y' shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' state_fips cty_fips statedistr108 cd108, by (ctycd108) 
	
	* keep district indicators of the 108th Congress
	rename cd108 cd
	rename statedistr108 statedistrict
	gen double cell_id=100*cty_fips+cd
	drop ctycd108

    * compute district-level overall Democrat and Republican votes
    by statedistrict, sort: egen totvote_distr_`y'=total(totvote_`y')
    by statedistrict, sort: egen totrvote_distr_`y'=total(totvoter_`y')
    by statedistrict, sort: egen totdvote_distr_`y'=total(totvoted_`y')
	
	* total votes in TX
	egen totvoteTX=total(totvote_`y')
	summ totvoteTX
	drop totvoteTX
	
	save temp_TX_`y'.dta, replace
}


*** Georgia and Texas in 2006-2010

forvalues y=2006(2)2010 {
	use temp_`y'.dta, clear
	keep if state_fips==13 | state_fips==48
	
	* total votes in GA, TX
	egen totvoteGATX=total(totvote_`y')
	summ totvoteGATX
	drop totvoteGATX
	
	gen cd110=cd
	destring cd110, replace
	gen ctycd110=100*cty_fips+cd110
	sort ctycd110
	save temp_GATX_`y'.dta, replace
	
	use ../House/crosswalks/cw_ctycd108_ctycd110.dta, clear
	sort ctycd110
	merge ctycd110 using temp_GATX_`y'.dta
	assert _merge==3 
	drop _merge
	
    gen cd108=ctycd108-100*floor(ctycd108/100)
	gen statedistr108=100*state_fips+cd108
	
    * compute vote counts by (108th district * county) cell - actual number of votes in geographic area
    foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd110_ctycd108)
	   replace `var'=`var'_wt
	}
	* compute weighted dummy indicators by (108th district * county) cell - index of post-rezoning districts
	foreach var of varlist shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_ctycd108_ctycd110)
	   replace `var'=`var'_wt
	}
	
	* collapse to (108th district * county)
    collapse (mean) totvote_`y' totvoter_`y' totvoted_`y' shgd_`y' shgr_`y' shnd_`y' shnr_`y' unopposed_`y' twoparty_`y' thirdparty_`y' shnr_distr_`y' r*_`y' state_fips cty_fips statedistr108 cd108, by (ctycd108) 
	
	* keep district indicators of the 108th Congress
	rename cd108 cd
	rename statedistr108 statedistrict
	gen double cell_id=100*cty_fips+cd
	drop ctycd108

    * compute district-level overall Democrat and Republican votes
    by statedistrict, sort: egen totvote_distr_`y'=total(totvote_`y')
    by statedistrict, sort: egen totrvote_distr_`y'=total(totvoter_`y')
    by statedistrict, sort: egen totdvote_distr_`y'=total(totvoted_`y')
	
	* total votes in TX, GA
	egen totvoteGATX=total(totvote_`y')
	summ totvoteGATX
	drop totvoteGATX
	save temp_GATX_`y'.dta, replace
}

*** merge data
use temp_2004.dta, clear
drop if state_fips==23 | state_fips==42 | state_fips==48
append using temp_MEPA_2004
append using temp_TX_2004
sort cell_id
summ cell_id
save temp_2004.dta, replace
erase temp_MEPA_2004.dta
erase temp_TX_2004.dta

forvalues y=2006(2)2010 {
	use temp_`y'.dta, clear
	drop if state_fips==13 | state_fips==23 | state_fips==42 | state_fips==48
	append using temp_MEPA_`y'
	append using temp_GATX_`y'
	sort cell_id
	summ cell_id
	save temp_`y'.dta, replace
	erase temp_MEPA_`y'.dta
	erase temp_GATX_`y'.dta
}


*******************************************************************
* County x district data - merge years
*******************************************************************

use temp_2002.dta, clear
merge cell_id using temp_2004.dta
assert _merge==3
drop _merge
sort cell_id
merge cell_id using temp_2006.dta
assert _merge==3
drop _merge
sort cell_id
merge cell_id using temp_2008.dta
assert _merge==3
drop _merge
sort cell_id
merge cell_id using temp_2010.dta
assert _merge==3
drop _merge

sort CD
save temp_vote.dta, replace

erase temp_2002.dta
erase temp_2004.dta
erase temp_2006.dta
erase temp_2008.dta
erase temp_2010.dta


****************************************************************************************
* Geography variables 
****************************************************************************************

* Broomfield Cty, CO: since county did largely split out of Boulder Cty in the 2000s, it is assigned to Boulder's CZ
* temporarily change Broomfield's county code to Boulder's
replace cty_fips=8013 if cell_id==801402

* recode Miami-Dade
replace cty_fips=12025 if cty_fips==12086

* CZones
sort cty_fips 
save temp_vote.dta, replace
use ../House/dta/cw_cty_czone.dta
keep cty_fips czone
sort cty_fips
save temp_czone.dta, replace
use temp_vote.dta, clear
merge cty_fips using temp_czone.dta
tab _merge
* note: states 2, 11, 15 are not covered in the voting data analysis; counties 30113, 51560, 51780 were eliminated between 1990 and 2000 
replace state_fips=floor(cty_fips/1000) if state_fips==.
assert _merge!=1
assert _merge!=2 if state_fips!=2 & state_fips!=11 & state_fips!=15 & cty_fips!=30113 & cty_fips!=51560 & cty_fips!=51780
keep if _merge==3
drop _merge
erase temp_vote.dta
erase temp_czone.dta

* Census division dummies 
gen state=substr(CD,1,2)
gen reg_neweng=(state=="ME" | state=="NH" | state=="VT" | state=="MA" | state=="RI" | state=="CT")
gen reg_midatl=(state=="NY" | state=="PA" | state=="NJ")
gen reg_encen=(state=="OH" | state=="MI" | state=="IN" | state=="IL" | state=="WI")
gen reg_wncen=(state=="MN" | state=="IA" | state=="MO" | state=="KS" | state=="NE" | state=="SD" | state=="ND")
gen reg_satl=(state=="DE" | state=="MD" | state=="DC" | state=="VA" | state=="WV" | state=="NC" | state=="SC" | state=="GA" | state=="FL")
gen reg_escen=(state=="KY" | state=="TN" | state=="AL" | state=="MS")
gen reg_wscen=(state=="AR" | state=="LA" | state=="OK" | state=="TX")
gen reg_mount=(state=="MT" | state=="ID" | state=="WY" | state=="CO" | state=="UT" | state=="NV" | state=="AZ" | state=="NM")
gen reg_pacif=(state=="CA" | state=="OR" | state=="WA" | state=="HI" | state=="AK")
assert reg_neweng+reg_midatl+reg_encen+reg_wncen+reg_satl+reg_escen+reg_wscen+reg_mount+reg_pacif

* undo change Broomfield's county code to Boulder's
replace cty_fips=8014 if cell_id==801402

gen ctycd108=100*cty_fips+cd
summ ctycd108
sort ctycd108
save temp_main.dta, replace


****************************************************************************************
* Vote Share data past 2010
****************************************************************************************

*******************************************************************
* County x district data - vote counts for 2012-2016
*******************************************************************

foreach y in 2012 2014 2016 {
   use ../House/dta/Leip_`y'_final.dta, clear
   * adjust FIPS code for Miami-Dade
   replace fips=12025 if fips==12086
   * drop AK, HI
   drop if CD=="AK 1" | CD=="HI 1" | CD=="HI 2"
   * county FIPS
   gen cty_fips=fips
   * state FIPS
   gen state_fips=floor(cty_fips/1000)
   * save
   save Leip_`y'_temp.dta, replace
}

forvalues y=2012(2)2016 {
   use Leip_`y'_temp.dta, clear

   * assert unique county-district combinations
   egen tag=tag(fips CD)
   tab fips CD if tag==0
   drop if tag==0
   drop tag 

   * numerical district code
   gen cd=substr(CD,-2,2)
   destring cd, replace
   * state-district identifier
   gen statedistrict=100*state_fips+cd
   * define cell identifier
   gen cell_id=100*cty_fips+cd
   
   * sample selection: drop a small number of statewide/overseas votes in ME, and of Federal/Limited votes in RI
   drop if county=="Statewide" & state_fips==23
   drop if county=="Overseas" & state_fips==23
   drop if county=="Federal/Limited" & state_fips==44
   drop if county=="Federal" & state_fips==44
   
   summ TotalVote 
   
   * variable definition: total votes in cell
   replace Democratic=0 if Democratic==.
   replace Republican=0 if Republican==.
   gen totvote_`y'=TotalVote
   gen totvoter_`y'=Republican
   gen totvoted_`y'=Democratic
   
   * variable definition: gross and net vote shares
   gen shgd_`y'=Democratic/TotalVote
   gen shgr_`y'=Republican/TotalVote
   gen shnd_`y'=(Democratic)/(Democratic+Republican)
   gen shnr_`y'=(Republican)/(Democratic+Republican)

   * variable definition: district-level overall, Democrat, Republican, third party
   by statedistrict, sort: egen totvote_distr_`y'=total(TotalVote)
   by statedistrict, sort: egen totdvote_distr_`y'=total(Democratic)
   by statedistrict, sort: egen totrvote_distr_`y'=total(Republican)
   *gen totovote_distr_`y'=totvote_distr_`y'-totdvote_distr_`y'-totrvote_distr_`y'  
   
   * variable definition: 
   * unopposed (winner gets all votes)
   * twoparty (both a Republican and a Democrat competing)
   * thirdparty (at least one Independent or Other Party Candidate competing) 
   gen unopposed_`y'=(totdvote_distr_`y'==totvote_distr_`y')
   replace unopposed_`y'=1 if (totrvote_distr_`y'==totvote_distr_`y')
   gen twoparty_`y'=(totdvote_distr_`y'>0 & totrvote_distr_`y'>0)  
   *gen thirdparty_`y'=(totovote_distr_`y'>0)   

   * vote share at the district level
   * bins for electoral margins in district
   gen shnr_distr_`y'=(totrvote_distr_`y'/(totrvote_distr_`y'+totdvote_distr_`y'))
   gen r10_`y'=(shnr_distr_`y'>=.0 & shnr_distr_`y'<.10)
   gen r20_`y'=(shnr_distr_`y'>=.10 & shnr_distr_`y'<.20)
   gen r30_`y'=(shnr_distr_`y'>=.20 & shnr_distr_`y'<.30)
   gen r40_`y'=(shnr_distr_`y'>=.30 & shnr_distr_`y'<.40)
   gen r50_`y'=(shnr_distr_`y'>=.40 & shnr_distr_`y'<.50)
   gen r60_`y'=(shnr_distr_`y'>=.50 & shnr_distr_`y'<.60)
   gen r70_`y'=(shnr_distr_`y'>=.60 & shnr_distr_`y'<.70)
   gen r80_`y'=(shnr_distr_`y'>=.70 & shnr_distr_`y'<.80)
   gen r90_`y'=(shnr_distr_`y'>=.80 & shnr_distr_`y'<.90)
   gen r100_`y'=(shnr_distr_`y'>=.90 & shnr_distr_`y'<=1.00)   
   
   * keep relevant variables
   keep cell_id CD state_fips cty_fips cd *_`y'
   sort cell_id
   save temp_`y'.dta, replace
   erase Leip_`y'_temp.dta
}

* year-specific corrections for unopposed races: 2012
local y=2012
use temp_`y'.dta, clear

   * districts with zero-vote unopposed races 
   tab CD if totvote_`y'==0 & unopposed_`y'==1
   * set vote share to 1 for winning party in unopposed zero-vote races
   foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
      replace `var'=1 if CD=="FL 24" & `y'==2012
	  replace `var'=0 if CD=="FL 15" & `y'==2012 
   }
   foreach var of varlist shgr_`y' shnr_`y' r100_`y' shnr_distr_`y' {
      replace `var'=0 if CD=="FL 24" & `y'==2012
 	  replace `var'=1 if CD=="FL 15" & `y'==2012 
   }
   
   * replace cell vote share with district vote share if no votes in the cell
   * (in 2012, one overlap in CA 13 and one in WI 4 have zero votes)
   replace shnd_`y'=1-shnr_distr_`y' if shnd_`y'==. 
   replace shnr_`y'=shnr_distr_`y' if shnr_`y'==. 
   replace shgd_`y'=(totdvote_distr_`y'/totvote_distr_`y') if shgd_`y'==. 
   replace shgr_`y'=(totrvote_distr_`y'/totvote_distr_`y') if shgr_`y'==. 
   
   * assert no missing variable values 
   foreach var of varlist *_`y' {
      assert `var'!=. 
   }

save temp_`y'.dta, replace

* year-specific corrections for unopposed races: 2014
local y=2014
use temp_`y'.dta, clear

   * districts with zero-vote unopposed races 
   tab CD if totvote_`y'==0 & unopposed_`y'==1
   * set vote share to 1 for winning party in unopposed zero-vote races
   foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
	  replace `var'=1 if CD=="FL 14" & `y'==2014
	  replace `var'=0 if CD=="FL 12" | CD=="FL 25" | CD=="FL 27" | CD=="OK 1" | CD=="TX 4" & `y'==2014
   }
   foreach var of varlist shgr_`y' shnr_`y' r100_`y' shnr_distr_`y' {
 	  replace `var'=0 if CD=="FL 14" & `y'==2014
 	  replace `var'=1 if CD=="FL 12" | CD=="FL 25" | CD=="FL 27" | CD=="OK 1" | CD=="TX 4" & `y'==2014
   }
   
   * replace cell vote share with district vote share if no votes in the cell
   * (in 2014, one overlap in CA 13 and one in WI 4 have zero votes)
   replace shnd_`y'=1-shnr_distr_`y' if shnd_`y'==. 
   replace shnr_`y'=shnr_distr_`y' if shnr_`y'==. 
   replace shgd_`y'=(totdvote_distr_`y'/totvote_distr_`y') if shgd_`y'==. 
   replace shgr_`y'=(totrvote_distr_`y'/totvote_distr_`y') if shgr_`y'==. 
   
   * assert no missing variable values 
   foreach var of varlist *_`y' {
      assert `var'!=. 
   }

save temp_`y'.dta, replace


* year-specific corrections for unopposed races: 2016
local y=2016
use temp_`y'.dta, clear

   * districts with zero-vote unopposed races 
   tab CD if totvote_`y'==0 & unopposed_`y'==1
   * set vote share to 1 for winning party in unopposed zero-vote races
   foreach var of varlist shgd_`y' shnd_`y' r10_`y' {
	  replace `var'=1 if CD=="FL 24" & `y'==2016
	  replace `var'=0 if CD=="OK 1" & `y'==2016
   }
   foreach var of varlist shgr_`y' shnr_`y' r100_`y' shnr_distr_`y' {
 	  replace `var'=0 if CD=="FL 24" & `y'==2016
 	  replace `var'=1 if CD=="OK 1" & `y'==2016
   }
   
   * replace cell vote share with district vote share if no votes in the cell
   * (in 2016, one overlap in CA 13 and one in WI 4 have zero votes)
   replace shnd_`y'=1-shnr_distr_`y' if shnd_`y'==. 
   replace shnr_`y'=shnr_distr_`y' if shnr_`y'==. 
   replace shgd_`y'=(totdvote_distr_`y'/totvote_distr_`y') if shgd_`y'==. 
   replace shgr_`y'=(totrvote_distr_`y'/totvote_distr_`y') if shgr_`y'==. 
   
   * assert no missing variable values 
   foreach var of varlist *_`y' {
      assert `var'!=. 
   }

save temp_`y'.dta, replace



*******************************************************************
* Crosswalk voting data to 2002
*******************************************************************

* save file with original 2016 data for redistricting states
use temp_2016.dta, clear
keep if (state_fips==12 | state_fips==37 | state_fips==51)
save temp_2016_redistrict.dta, replace

*** all states (drop those w/ 2016 redistricting later)

forvalues y=2012(2)2016 {
	use temp_`y'.dta, clear
	
	gen cd113=cd
	destring cd113, replace
	gen ctycd113=100*cty_fips+cd113
	sort ctycd113
	save temp_`y'.dta, replace

	use ../House/crosswalks/CW_cty113CD_cty108CD.dta, clear
	* assert that state does not change over time
	assert state00==state10
	destring county10, replace
    destring county00, replace
	* drop AK, DC, HI
	drop if state00==2 | state00==11 | state00==15

	gen ctycd108=100*county00+cd108
	gen ctycd113=100*county10+cd113
	sort ctycd113
	
	* change Miami-Date code to 1990 value
	replace county00=12025 if county00==12086
	replace county10=12025 if county10==12086
	* relabel Miami-Dade County
    replace ctycd108=ctycd108-6100 if ctycd108>=1208600 & ctycd108<=1208699
	replace ctycd113=ctycd113-6100 if ctycd113>=1208600 & ctycd113<=1208699
	
	sort ctycd113
	
	* merge with voting data
	merge ctycd113 using temp_`y'.dta
	tab _merge
	tab ctycd113 if _merge==1
	tab ctycd113 if _merge==2

	* drop following unmatched cells:
	* missing in voting data 2012: Weld Cty CO (8123 / CO-2) - has <6 population
	* missing in crosswalk 2012: San Francisco Cty CA (6075 / CA-13), Waukesha Cty WI (55133 / WI-04) - have 0 votes
	drop if ctycd113==812302 
	drop if ctycd113==607513 
	drop if ctycd113==5513304 
	
	* Bedford City matches into Bedford County in 2013 - in 2014 (and 2016 further below), count a fraction of 6225/(35428+6225) of Bedford County votes to Bedford City
	local x=`y'
	while `x'>=2014 {
	   save temp.dta, replace
	   keep if ctycd113==5101905 & cd108==5
	   save temp_bedford.dta, replace
	   foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	      replace `var'=`var'*35428/(35428+6225)
	   }
	   save temp_bedfordcty.dta, replace
	   use temp_bedford.dta, clear
	   foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	      replace `var'=`var'*6225/(35428+6225)
	   }
	   replace ctycd108=5151505 
	   replace ctycd113=5151505 
	   replace county00=51515
	   replace county10=51515
	   replace cty_fips=51515
	   replace pop00=6225
	   replace afact_108_113=1
	   save temp_bedfordcity.dta, replace
	   use temp.dta, clear
	   drop if ctycd113==5101905 & cd108==5
	   drop if ctycd113==5151505
	   append using temp_bedfordcty.dta
	   append using temp_bedfordcity.dta
	   erase temp.dta
	   erase temp_bedford.dta
	   erase temp_bedfordcty.dta
	   erase temp_bedfordcity.dta
	   local x=0
   	}

	* Clifton Forge ceased to exist in 2001. Assign its population and votes to Alleghany.
	replace ctycd108=5100509 if ctycd108==5156009
	
	* drop re-zoned states in 2016
	drop if (state10==12 | state10==37 | state10==51) & `y'==2016
    drop if (state_fips==12 | state_fips==37 | state_fips==51) & `y'==2016
	
	assert _merge==3 
	keep if _merge==3
	drop _merge
	
	* eliminate cells that always have zero votes 2002-2010 
	drop if county00==4025 & cd108==2 /* pop of 1 */
	drop if county00==8001 & cd108==1 /* pop of 0 */
	replace pop00=pop00+12780 if county00==36005 & cd108==7 /* pop of 12780; the new district 14 maps to old districts 7 (94%), 15 (5%) and 17 (<1%); attribute the overlap with district 15 to district 7 */
	drop if county00==36005 & cd108==15 
	drop if county00==36081 & cd108==15 /* pop of 0 */
	* elminate overlaps with population <1
	drop if pop00<1
	
	gen statedistr108=100*state_fips+cd108
	gen statedistr113=100*state_fips+cd113
	by statedistr108, sort: egen pop_cd108=total(pop00)
	by statedistr113, sort: egen pop_cd113=total(pop00)	
	
	* recompute allocation factors
	drop afact*
	by statedistr108, sort: egen pop108=total(pop00)
	gen double afact_108_113=pop00/pop108
	by statedistr113, sort: egen pop113=total(pop00)
	gen double afact_113_108=pop00/pop113
	
	by ctycd108, sort: egen pop_ctycd108=total(pop00)
	by ctycd113, sort: egen pop_ctycd113=total(pop00)
	gen afact_CD108_113=pop00/pop_ctycd108
	gen afact_CD113_108=pop00/pop_ctycd113
	
    * compute vote counts by (108th district * county) cell
    foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_CD113_108)
	   replace `var'=`var'_wt
	}
	* compute weighted dummy indicators by (108th district * county) cell
	foreach var of varlist unopposed_`y' twoparty_`y' shnr_distr_`y' r*_`y' {
	   by ctycd108, sort: egen `var'_wt=total(`var'*afact_CD108_113)
	   replace `var'=`var'_wt
	}

	* collapse to (108th district * county)
    collapse (mean) totvote_`y' totvoted_`y' totvoter_`y' unopposed_`y' twoparty_`y' shnr_distr_`y' r*_`y' state_fips cty_fips statedistr108 cd108, by (ctycd108) 
	
	* compute vote shares
    gen shgd_`y'=(totvoted_`y')/(totvote_`y')
    gen shgr_`y'=(totvoter_`y')/(totvote_`y')
    gen shnd_`y'=(totvoted_`y')/(totvoted_`y'+totvoter_`y')
    gen shnr_`y'=(totvoter_`y')/(totvoted_`y'+totvoter_`y')
	
	* keep district indicators of the 108th Congress
	rename cd108 cd
	rename statedistr108 statedistrict
	gen double cell_id=ctycd108
	drop ctycd108

    * compute district-level overall Democrat and Republican votes
    by statedistrict, sort: egen totvote_distr_`y'=total(totvote_`y')
    by statedistrict, sort: egen totdvote_distr_`y'=total(totvoted_`y')
    by statedistrict, sort: egen totrvote_distr_`y'=total(totvoter_`y')
	
	sort cell_id
	summ cell_id
	save temp_`y'.dta, replace
}


*** states with 2016 redistricting

foreach y in 2016 {
	use temp_`y'_redistrict.dta, clear

	gen cd115=cd
	destring cd115, replace
	gen ctyCD115=100*cty_fips+cd115
	drop cd115
	sort ctyCD115
	save temp_`y'_redistrict.dta, replace

	use ../House/crosswalks/CW_ctyCD115_ctyCD108_3states.dta, clear
    destring ctyCD108, replace
	destring ctyCD115, replace
	
	* relabel Miami-Dade County
    replace ctyCD108=ctyCD108-6100 if ctyCD108>=1208600 & ctyCD108<=1208699
	replace ctyCD115=ctyCD115-6100 if ctyCD115>=1208600 & ctyCD115<=1208699
	sort ctyCD115
	* merge with voting data
	merge ctyCD115 using temp_`y'_redistrict.dta
	tab _merge
	* there are 23 cells with zero votes - these cells on average have a population of 8 (maximum 123), and account for 0.02% of a 108th district (maximum 0.4%)
	* drop these cells
	drop if _merge==1 & ctyCD115!=5151505
	assert _merge==3 if ctyCD115!=5151505
	drop _merge

	* Bedford City matches into Bedford County in 2013 - in 2014 (and 2016 further below), count a fraction of 6299/(35637+6299) of Bedford County votes to Bedford City
	   save temp.dta, replace
	   keep if ctyCD115==5101905 & ctyCD108==5101905
	   save temp_bedford.dta, replace
	   foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	      replace `var'=`var'*35637/(35637+6299)
	   }
	   save temp_bedfordcty.dta, replace
	   use temp_bedford.dta, clear
	   foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	      replace `var'=`var'*6299/(35637+6299)
	   }
	   replace ctyCD108=5151505 
	   replace ctyCD115=5151505 
	   replace cty_fips=51515
	   replace pop00=6299
	   replace afact_108_115=1
	   save temp_bedfordcity.dta, replace
	   use temp.dta, clear
	   drop if ctyCD115==5101905 & ctyCD108==5101905
	   drop if ctyCD115==5151505
	   append using temp_bedfordcty.dta
	   append using temp_bedfordcity.dta
	   erase temp.dta
	   erase temp_bedford.dta
	   erase temp_bedfordcty.dta
	   erase temp_bedfordcity.dta
	   
   	* Clifton Forge ceased to exist in 2001. Assign its population and votes to Alleghany.
   	replace ctyCD108=5100509 if ctyCD108==5156009

	* create state, county and district codes
	destring ctyCD115, replace
	destring ctyCD108, replace
	gen county16=floor(ctyCD115/100)
	gen county00=floor(ctyCD108/100)
	gen state16=floor(county16/1000)
	gen state00=floor(county00/1000)
	gen cd115=ctyCD115-100*county16
	gen cd108=ctyCD108-100*county00
	* change Miami-Date code to 1990 value
	replace county00=12025 if county00==12086
	* county*108th district cell
	gen ctycd108=(100*county00)+cd108
	* 108th distrct
	gen statedistr108=100*state00+cd108
	* 115th distrct
	gen statedistr115=100*state16+cd115
	* 108th*115th distrct cell
	gen double cd108cd115=10000*statedistr108+statedistr115
	* county*108th district x 115th distrct cell
	gen double ctycd108_cd115=10000*ctycd108+statedistr115
	
	* eliminate cells that always have zero votes 2002-2010 
	drop if county00==4025 & cd108==2 /* pop of 1 */
	drop if county00==8001 & cd108==1 /* pop of 0 */
	replace pop00=pop00+12780 if county00==36005 & cd108==7 /* pop of 12780; the new district 14 maps to old districts 7 (94%), 15 (5%) and 17 (<1%); attribute the overlap with district 15 to district 7 */
	drop if county00==36005 & cd108==15 
	drop if county00==36081 & cd108==15 /* pop of 0 */
	* elminate overlaps with population <1
	drop if pop00<1
	
	by statedistr108, sort: egen pop_cd108=total(pop00)
	by statedistr115, sort: egen pop_cd113=total(pop00)	
	
	* recompute allocation factors
	drop afact*
	by statedistr108, sort: egen pop108=total(pop00)
	gen double afact_108_115=pop00/pop108
	by statedistr115, sort: egen pop115=total(pop00)
	gen double afact_115_108=pop00/pop115
	
	by ctyCD108, sort: egen pop_ctycd108=total(pop00)
	by ctyCD115, sort: egen pop_ctycd115=total(pop00)
	gen afact_CD108_115=pop00/pop_ctycd108
	gen afact_CD115_108=pop00/pop_ctycd115
	
    * compute vote counts by (108th district * county) cell
    foreach var of varlist totvote_`y' totvoted_`y' totvoter_`y' {
	   by ctyCD108, sort: egen `var'_wt=total(`var'*afact_CD115_108)
	   replace `var'=`var'_wt
	}
	* compute weighted dummy indicators by (108th district * county) cell
	foreach var of varlist unopposed_`y' twoparty_`y' shnr_distr_`y' r*_`y' {
	   by ctyCD108, sort: egen `var'_wt=total(`var'*afact_CD108_115)
	   replace `var'=`var'_wt
	}
	
	* collapse to (108th district * county)
    collapse (mean) totvote_`y' totvoted_`y' totvoter_`y' unopposed_`y' twoparty_`y' shnr_distr_`y' r*_`y' state_fips cty_fips statedistr108 cd108, by (ctyCD108) 
	
	* compute vote shares
    gen shgd_`y'=(totvoted_`y')/(totvote_`y')
    gen shgr_`y'=(totvoter_`y')/(totvote_`y')
    gen shnd_`y'=(totvoted_`y')/(totvoted_`y'+totvoter_`y')
    gen shnr_`y'=(totvoter_`y')/(totvoted_`y'+totvoter_`y')
	
	* keep district indicators of the 108th Congress
	rename cd108 cd
	rename statedistr108 statedistrict
	gen double cell_id=ctyCD108
	drop ctyCD108

    * compute district-level overall Democrat and Republican votes
    by statedistrict, sort: egen totvote_distr_`y'=total(totvote_`y')
    by statedistrict, sort: egen totdvote_distr_`y'=total(totvoted_`y')
    by statedistrict, sort: egen totrvote_distr_`y'=total(totvoter_`y')
	
	sort cell_id
	summ cell_id
	save temp_`y'_redistrict.dta, replace
}


* merge all voting files, merge with main data
use temp_2016.dta, clear
append using temp_2016_redistrict.dta
sort cell_id 
summ cell_id
merge cell_id using temp_2014.dta

assert _merge==3
tab cell_id if _merge==1
tab cell_id if _merge==2
keep if _merge==3
drop _merge
sort cell_id
merge cell_id using temp_2012.dta

assert _merge==3
tab _merge
keep if _merge==3
drop _merge


* Broomfield county CO was created largely out of Boulder county in 2001. Split the Boulder observation in two, using 2010 vote counts as weights.
   save temp.dta, replace
   keep if cell_id==801302
   save temp_boulder.dta, replace
   foreach var of varlist totvote* {
      replace `var'=`var'*10614/(67199+10614)
   }
   replace cell_id=801402
   replace cty_fips=8014
   save temp_broomfield.dta, replace
   use temp_boulder.dta, clear
   foreach var of varlist totvote* {
      replace `var'=`var'*67199/(67199+10614)
   }
   save temp_boulder.dta, replace
   use temp.dta, clear
   drop if cell_id==801302
   append using temp_broomfield.dta
   append using temp_boulder.dta
   erase temp.dta
   erase temp_broomfield.dta
   erase temp_boulder.dta
   
gen ctycd108=cell_id
keep ctycd108 *_2012 *_2014 *_2016 
sort ctycd108
merge ctycd108 using temp_main.dta

assert _merge==3
tab _merge
keep if _merge==3

drop _merge
sort cell_id
save temp_vote.dta, replace

erase temp_2012.dta
erase temp_2014.dta
erase temp_2016.dta
erase temp_2016_redistrict.dta



*******************************************************************
* County and county x district data - registered voters in 2002-2010 (c)
* and 2000 cell population (c x d)
*******************************************************************

use ../House/dta/Cong_108_pop_2000.dta, clear
drop reg_*
sort cty_fips
save temp.dta, replace
forvalues k=2002(2)2010 {
   use ../House/dta/TO_`k'_raw.dta, clear
   rename fips cty_fips
   * drop state totals
   drop if county=="T"
   * drop empty cells
   drop if county==""
   * drop counties "999"
   drop if cty_fips==23999
   drop if cty_fips==44999
   * drop AK, DC, HI
   drop if st==2 | st==11 | st==15
   * adjust Miami county code
   replace cty_fips=12025 if cty_fips==12086
   * Set registered voters from zero to missing for North Dakota in 2004-2010
   replace reg_voters_`k'=. if reg_voters_`k'==0 & st==38
   * states with missing data on registered voters
   disp "states w/o data on registered voters, year `k'"
   tab st if reg_voters_`k'==.
   keep cty_fips reg_voters_`k'
   sort cty_fips
   merge cty_fips using temp.dta
   assert _merge==3
   drop _merge
   sort cty_fips
   save temp.dta, replace
}
erase temp.dta

* define cell identifier
gen cd=substr(congressionaldistrict,-2,.)
destring cd, replace
gen cell_id=100*cty_fips+cd
* assert unique cells
egen tag=tag(cell_id)
assert tag==1
drop tag

keep cell_id vote* reg*
sort cell_id
save temp_pop.dta, replace

use temp_vote.dta, clear
merge cell_id using temp_pop.dta
tab _merge
* consistently observed cells w/o population data
tab cell_id if _merge==1 
* cells with population data but no voting data
tab cell_id if _merge==2
* cells with population data but no voting data, excluding cells with population <=1
tab vote_pop if _merge==2
drop if _merge==2 & vote_pop<=1
tab cell_id if _merge==2
* NY-15, county 36005 - this cell (part of Bronx) exists in the voting data but has always zero votes and is thus dropped earlier in this file 
drop if cell_id==3600515
assert _merge==3
drop _merge
erase temp_pop.dta

* Rename cell population
rename vote_pop cell_pop_2000

* County-level voting population
drop cty_fips
gen cty_fips=floor(cell_id/100)
bysort cty_fips: egen cty_pop_2000=total(cell_pop_2000)

* Rename registered voter variable to avoid confusion with region dummies
rename reg_voters_2002 no_reg_voters_2002
rename reg_voters_2004 no_reg_voters_2004
rename reg_voters_2006 no_reg_voters_2006
rename reg_voters_2008 no_reg_voters_2008
rename reg_voters_2010 no_reg_voters_2010

* Total county-level votes
* Turnout among registered voters
forvalues y=2002(2)2010 {
   bysort cty_fips: egen totvote_cty_`y'=total(totvote_`y')
   gen vote_share_`y'=totvote_cty_`y'/no_reg_voters_`y'
}
* Winsorize share at 1
forvalues y=2002(2)2010 {
   summ vote_share_`y'
   tab cell_id if vote_share_`y'>1 & no_reg_voters_`y'!=.
   replace vote_share_`y'=1 if vote_share_`y'>1 & no_reg_voters_`y'!=.
   assert vote_share_`y'>=0 & vote_share_`y'<=1 if no_reg_voters_`y'!=.

}
sort czone
save temp_vote.dta, replace



*******************************************************************
* Variable definitions: Cell weights
*******************************************************************

* compute population shares within districts
assert cell_pop_2000>0 
by CD, sort: egen dist_pop=total(cell_pop_2000)
gen sh_distpop_2000=cell_pop_2000/dist_pop
* set to zero for Broomfield CO, which didn't exist in the 2000 Census
replace sh_distpop_2000=0 if cell_id==801402
assert sh_distpop_2000>0 & sh_distpop_2000<=1 if cell_id!=801402 & state_fips!=50


* compute vote shares within districts
foreach y in 2002 2010 {
   gen sh_distvote_`y'=totvote_`y'/totvote_distr_`y' if totvote_distr_`y'!=0 
}   
* district vote shares are missing in districts with unopposed zero vote races
* FL 10, 11, 12, 14, 20, 21 in 2002
tab CD if sh_distvote_2002==.
* FL 21, LA 7, OK 4 in 2010
tab CD if sh_distvote_2010==.
* use shares from 2010 (2002) when 2002 (2010) data is missing
replace sh_distvote_2002=sh_distvote_2010 if sh_distvote_2002==.
replace sh_distvote_2010=sh_distvote_2002 if sh_distvote_2010==.
* give equal weight to both counties in FL 21, which is missing in both 2002 and 2010
replace sh_distvote_2002=0.5 if CD=="FL 21"
replace sh_distvote_2010=0.5 if CD=="FL 21"


* correlation population weights with vote share weights
pwcorr sh_distpop_2000 sh_distvote_2002 

* observations where only one weight is positive
tab cell_id if sh_distvote_2002==0 & sh_distpop_2000>0
tab cell_id if sh_distvote_2002>0 & sh_distpop_2000==0
* in 2002, Leip (strangely) records no votes for county 5009 in AR-3 while all other counties in AR-3 have positive votes
* in 2002, there are vote counts for Broomfield Cty CO, which did not yet exist in the 2000 Census
* Replace population-based shares with vote-based shares and vice versa for all county overlaps of AR-3 and CO-2
replace sh_distvote_2002=sh_distpop_2000 if CD=="AR 3"
replace sh_distpop_2000=sh_distvote_2002 if CD=="CO 2"

****** use population as main weighting variable 
gen sh_district_2002=sh_distpop_2000



*******************************************************************
* Variable definitions: Redistricting indicators
*******************************************************************

* redistricting in 2004 (all districts in ME, PA, TX, except TX-16
gen redistrict_2004=0
replace redistrict_2004=1 if state_fips==23 | state_fips==42 | state_fips==48
replace redistrict_2004=0 if CD=="TX 16"

* redistricting in 2006 (all districts in GA; in Texas, only districts 15, 21, 23, 25, 28 changed after a Federal Court redrew district 23)
gen redistrict_2006=0
replace redistrict_2006=1 if state_fips==13
replace redistrict_2006=1 if (CD=="TX 15" | CD=="TX 21" | CD=="TX 23" | CD=="TX 25" | CD=="TX 28")

* redistricting in 2012 (all districts except the at-large districts of DE, MT, ND, SD, VT, WY, and WV-1)
* (note: the five districts of AL-1, CT-1, MN-8, OK-1, OK-5, OR-2 have only minimal changes; >99% of the old district's population falls into the new one
* and >99% of the new one's population falls into the old one. However, inspection of the district maps shows that there were indeed small boundary changes
* to each of these districts)
gen redistrict_2012=1
replace redistrict_2012=0 if (CD=="DE 1" | CD=="MT 1" | CD=="ND 1" | CD=="SD 1" | CD=="VT 1" | CD=="WV 1" | CD=="WY 1")

* redistricting in 2016 (all districts in NC, VA; 24 of 27 districts in FL (but all 108th districts affected by boundary changes))
gen redistrict_2016=(state_fips==12 | state_fips==37 | state_fips==51)



*******************************************************************
* Add external data
*******************************************************************

* DIME, Nominate, Tea Party
sort cell_id
merge 1:1 cell_id using ../Distr/distr_variables.dta
tab _merge
assert _merge==3
drop _merge

* Trade shock, CZ controls
sort czone
save temp.dta, replace
use ../CZ/cz_variables.dta, clear
keep czone d_imp_usch_pd d_imp_otch_lag_pd l_sh_routine33 l_task_outsource l_shind_manuf_cbp
sort czone
merge czone using temp.dta
assert _merge==3
drop _merge

* Cty controls
sort cty_fips
save temp.dta, replace
use ../Cty/cty_variables.dta, clear
keep cty_fips shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic majority_white
sort cty_fips
merge cty_fips using temp.dta
assert _merge!=2
keep if _merge==3
drop _merge

erase temp_main.dta
erase temp_vote.dta
erase temp.dta


*******************************************************************
* Redistricting-adjusted changes of electoral outcomes
*******************************************************************

* vote shares, dummy for Republican win
foreach k in shnr_ shnr_distr_ rwin_ {
forvalues t=2004(2)2016 {
   * w/o redistricting adjustment	
   quietly gen d_`k'2002_`t'=`k'`t'-`k'2002	
   * with redistricting adjustment
   quietly gen d2_`k'2002_`t'=`k'`t'-`k'2002
   quietly replace d2_`k'2002_`t'=d2_`k'2002_`t'-(`k'2004-`k'2002) if redistrict_2004==1 & `t'>=2004
   quietly replace d2_`k'2002_`t'=d2_`k'2002_`t'-(`k'2006-`k'2004) if redistrict_2006==1 & `t'>=2006
   quietly replace d2_`k'2002_`t'=d2_`k'2002_`t'-(`k'2012-`k'2010) if redistrict_2012==1 & `t'>=2012
   quietly replace d2_`k'2002_`t'=d2_`k'2002_`t'-(`k'2016-`k'2014) if redistrict_2016==1 & `t'>=2016
   * scale to percentage points
   quietly replace d2_`k'2002_`t'=100*d2_`k'2002_`t'
   * restrict to -100,+100 interval
   quietly replace d2_`k'2002_`t'=-100 if d2_`k'2002_`t'<-100 & d2_`k'2002_`t'!=.
   quietly replace d2_`k'2002_`t'=100 if d2_`k'2002_`t'>100 & d2_`k'2002_`t'!=.
   }
}	


*******************************************************************
* Final variable definitions
*******************************************************************

* competitive and non-comeptitive districts
forvalues y=2002(2)2016 {
	gen snhr_distr108_`y'=totrvote_distr_`y'/(totrvote_distr_`y'+totdvote_distr_`y')
}

gen dist_solid_dem=(snhr_distr108_2002<0.45 & snhr_distr108_2004<0.45 & snhr_distr108_2006<0.45 & snhr_distr108_2008<0.45 & snhr_distr108_2010<0.45)
gen dist_solid_rep=(snhr_distr108_2002>0.55 & snhr_distr108_2004>0.55 & snhr_distr108_2006>0.55 & snhr_distr108_2008>0.55 & snhr_distr108_2010>0.55)
gen dist_competitive=1-dist_solid_dem-dist_solid_rep

gen dist_solid_dem_2002=(snhr_distr108_2002<0.40)
gen dist_solid_rep_2002=(snhr_distr108_2002>0.60)
gen dist_competitive_2002=1-dist_solid_dem_2002-dist_solid_rep_2002

summ dist_solid_dem dist_solid_rep dist_competitive [aw=sh_district_2002]
summ dist_solid_dem_2002 dist_solid_rep_2002 dist_competitive_2002 [aw=sh_district_2002]

* vote share change in solid and competitive districts
gen d2_shnr_solidd_2002_2010=d2_shnr_2002_2010*(dist_solid_dem)
gen d2_shnr_solidr_2002_2010=d2_shnr_2002_2010*(dist_solid_rep)
gen d2_shnr_competitive_2002_2010=d2_shnr_2002_2010*(dist_competitive)


* turnout in districts with unopposed races and no redistricting
by cty_fips, sort: egen totuo02=total(unopposed_2002)
by cty_fips, sort: egen totuo10=total(unopposed_2010)
gen d2_turnout_2002_2010=vote_share_2010-vote_share_2002 if totuo02==0 & totuo10==0 & redistrict_2004==0 & redistrict_2006==0



*******************************************************************
* Final variable adjustments
*******************************************************************

* drop omitted geography category
rename reg_neweng neweng

* rename CD identifier
gen congressionaldistrict=CD


foreach var of varlist congressionaldistrict cty_fips czone dhs2_tot_cont_2002_2010 dhs2_cont_tcile1_2002_* dhs2_cont_tcile2_2002_* dhs2_cont_tcile3_2002_* d2_turnout_2002_2010 d2_shnr_2002_* d2_shnr_*_2002_2010 d2_rwin_2002_* d2_cfavg_demlib_2002_* d2_cfavg_demmod_2002_* d2_cfavg_repmod_2002_* d2_cfavg_repcon_2002_* d2_nominate_*_2002_2010 d2_teaparty_2002_2010 d_party_* d_imp_usch_pd d_imp_otch_lag_pd reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic sh_district_2002 majority_white redistrict_* {
  label variable `var' ""
}
keep congressionaldistrict cty_fips czone dhs2_tot_cont_2002_2010 dhs2_cont_tcile1*_2002_* dhs2_cont_tcile2*_2002_* dhs2_cont_tcile3*_2002_* d2_turnout_2002_2010 d2_shnr_2002_* d2_shnr_*_2002_2010 d2_rwin_2002_* d2_cfavg_demlib_2002_* d2_cfavg_demmod_2002_* d2_cfavg_repmod_2002_* d2_cfavg_repcon_2002_* d2_nominate_*_2002_2010 d2_teaparty_2002_2010 d_party_* d_imp_usch_pd d_imp_otch_lag_pd reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic sh_district_2002 majority_white redistrict_* 
order congressionaldistrict cty_fips czone dhs2_tot_cont_2002_2010 dhs2_cont_tcile1*_2002_* dhs2_cont_tcile2*_2002_* dhs2_cont_tcile3*_2002_* d2_turnout_2002_2010 d2_shnr_2002_* d2_shnr_*_2002_2010 d2_rwin_2002_* d2_cfavg_demlib_2002_* d2_cfavg_demmod_2002_* d2_cfavg_repmod_2002_* d2_cfavg_repcon_2002_* d2_nominate_*_2002_2010 d2_teaparty_2002_2010 d_party_* d_imp_usch_pd d_imp_otch_lag_pd reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic sh_district_2002 majority_white redistrict_*
 



save house_2002_2016.dta, replace


