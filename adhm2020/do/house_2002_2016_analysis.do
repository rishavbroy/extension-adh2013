*******************************************************************
* Analysis of US House Elections (Campaign Contributions and Election Outcomes), 2002-2016
*******************************************************************

* David Dorn, final version Feburary 27, 2020
* Input file: house_2002_2016.dta


cap log close
set more off
clear matrix
clear mata
clear
set maxvar 20000

log using ../log/house_2002_2016_analysis.log, text replace

use ../dta/house_2002_2016.dta, clear

* define vector of full controls
local full_ctrl="reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic"


*******************************************************************
* TABLE 3: CHANGE IN CONTRIBUTIONS BY DONOR IDEOLOGY, 2002-2010
*******************************************************************

* panel A: total contributions 2002-2010
local depvar="tot_cont"
summ dhs2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel B: total left-wing contributions 2002-2010
local depvar="cont_tcile1"
summ dhs2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel C: total moderate contributions 2002-2010
local depvar="cont_tcile2"
summ dhs2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel D: total right-wing contributions 2002-2010
local depvar="cont_tcile3"
summ dhs2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 dhs2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)


*******************************************************************
* TABLE 4: TURNOUT, PARTY VOTE SHARES AND ELECTION PROBABILITIES, 2002-2010
*******************************************************************

* column 1: turnout among registered voters in opposed races, 2002-2010 (w/o unopposed races, redistricted cells, and states where variable is missing)
local depvar="turnout"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* column 2: Republican two-party vote share
local depvar="shnr"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* column 3: Republican two-party vote share in solid Democratic districts
local depvar="shnr_solidd"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* column 4: Republican two-party vote share in competitive districts
local depvar="shnr_competitive"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* column 5: Republican two-party vote share in solid Republican districts
local depvar="shnr_solidr"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* column 6: Republican election probability
local depvar="rwin"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)


*******************************************************************
* TABLE A1: REDISTRICTING
*******************************************************************

* indicator for change of party in districts without and with redistricting
summ d_party_2002_2004 [aw=sh_district_2002] if redistrict_2004==0
summ d_party_2002_2004 [aw=sh_district_2002] if redistrict_2004==1
summ d_party_2004_2006 [aw=sh_district_2002] if redistrict_2006==0
summ d_party_2004_2006 [aw=sh_district_2002] if redistrict_2006==1
summ d_party_2006_2008 [aw=sh_district_2002] 
summ d_party_2008_2010 [aw=sh_district_2002] 
summ d_party_2010_2012 [aw=sh_district_2002] if redistrict_2012==0
summ d_party_2010_2012 [aw=sh_district_2002] if redistrict_2012==1
summ d_party_2012_2014 [aw=sh_district_2002]
summ d_party_2014_2016 [aw=sh_district_2002] if redistrict_2016==0
summ d_party_2014_2016 [aw=sh_district_2002] if redistrict_2016==1


*******************************************************************
* TABLE A2: ELECTION PROBABILITIES BY PARTY AND POLITICAL POSITION, 2002-2010, ALTERNATIVE IDEOLOGY DEFINITIONS
*******************************************************************

* panel A: classification based on average CF score
foreach k in demlib demmod repmod repcon {
   local y=2010 
		local depvar="cfavg_`k'"
		disp "election victory by `k', classification based on average CF score"
		summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
	
}

* panel B: classification based on DW-Nominate with linear trend
foreach k in demlib demmod repmod repcon {
   local y=2010 
		local depvar="nominate_`k'"
		disp "election victory by `k', classification based on DW-Nominate"
		summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
	
}

* supplementary result: election probability of politicians affiliated with Tea Party, Liberty or Freedom Caucus
    local y=2010
   	local depvar="teaparty"
   	disp "election victory by tea party'"
   	summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
   	ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)


*******************************************************************
* TABLE S2: DESCRIPTIVES FOR TRADE EXPOSURE
*******************************************************************

* column 1: 2002-2010
summ d_imp_usch_pd [aw=sh_district_2002], detail

* note: column 2 uses data from a separate file


* supplementary result: change in import exposure vs manufacturing share
reg d_imp_usch_pd l_shind_manuf_cbp [aw=sh_district_2002]

* supplementary result: trade exposure in majority and minority white counties
summ d_imp_usch_pd if majority_white==1 [aw=sh_district_2002]
summ d_imp_usch_pd if majority_white==0 [aw=sh_district_2002]


*******************************************************************
* TABLE S5: VOTE SHARES AND ELECTION PROBABILITIES BY PARTY, 2002-2010 and 2002-2016, SEQUENTIAL ADDITION OF CONTROLS
*******************************************************************

* panel A: Republican election probability 2002-2010
local depvar="rwin"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel B: Republican election probability 2002-2016
local depvar="rwin"
summ d2_`depvar'_2002_2016 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel C: Republican two-party vote share 2002-2010
local depvar="shnr"
summ d2_`depvar'_2002_2010 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2010 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)

* panel D: Republican two-party vote share 2002-2016
local depvar="shnr"
summ d2_`depvar'_2002_2016 [aw=sh_district_2002]
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
ivreg2 d2_`depvar'_2002_2016 (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)


*******************************************************************
* TABLE S6: ELECTION PROBABILITIES BY PARTY AND POLITICAL POSITION, 2002-2010 and 2002-2016, SEQUENTIAL ADDITION OF CONTROLS
*******************************************************************

* panels for period 2002-2010
foreach k in demlib demmod repmod repcon {
   local y=2010 
		local depvar="cfavg_`k'"
		disp "election victory by `k', 2002-`y'"
		summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)	
}

* panels for period 2002-2016
foreach k in demlib demmod repmod repcon {
   local y=2016 
		local depvar="cfavg_`k'"
		disp "election victory by `k', 2002-`y'"
		summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=sh_district_2002], cluster(czone congressionaldistrict)
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
}


*******************************************************************
* TABLE S8 (FIGURE 4): CHANGE IN CONTRIBUTIONS BY DONOR IDEOLOGY, 2002-20XX
*******************************************************************

* regressions by donor ideology tercile and year
forvalues k=1(1)3 {
   forvalues y=2004(2)2016 {
		local depvar="cont_tcile`k'"
		disp "contributions tercile `k', year 2002-`y'"
		summ dhs2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 dhs2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}


*******************************************************************
* TABLE S9 (FIGURE 5): PARTY VOTE SHARES AND ELECTION PROBABILITIES, 2002-20XX
*******************************************************************

* panel A: Republican election probability
forvalues y=2004(2)2016 {
	local depvar="rwin"
	disp "Republican victory, year 2002-`y'"
	summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
	ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
}

* panel B: Republican two-party vote share
forvalues y=2004(2)2016 {
	local depvar="shnr"
	disp "Republican two-party vote share, year 2002-`y'"
	summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
	ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
}


*******************************************************************
* TABLE S10 (FIGURE 6): ELECTION PROBABILITIES BY PARTY AND POLITICAL POSITION, 2002-20XX
*******************************************************************

* election probabilities for liberal Democrats, moderate Democrats, moderate Republicans, conservative Republicans
foreach k in demlib demmod repmod repcon {
   forvalues y=2004(2)2016 {
		local depvar="cfavg_`k'"
		disp "election probability for party-position group `k', year 2002-`y'"
		summ d2_`depvar'_2002_`y' [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}


*******************************************************************
* TABLE S11 (FIGURE 7): ELECTION PROBABILITIES BY PARTY AND POLITICAL POSITION, 2002-20XX, WHITE VS NONWHITE AREAS
*******************************************************************

* panel A: election probabilities for liberal Democrats, moderate Democrats, moderate Republicans, conservative Republicans, majority white counties
foreach k in demlib demmod repmod repcon {
   forvalues y=2004(2)2016 {
		local depvar="cfavg_`k'"
		disp "majority NH-white counties: election probability for party-position group `k', year 2002-`y'"
		summ d2_`depvar'_2002_`y' if majority_white==1 [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1 [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}

* panel B: election probabilities for liberal Democrats, moderate Democrats, moderate Republicans, conservative Republicans, majority nonwhite/Hispanic counties
foreach k in demlib demmod repmod repcon {
   forvalues y=2004(2)2016 {
		local depvar="cfavg_`k'"
		disp "minority NH-white counties: election probability for party-position group `k', year 2002-`y'"
		summ d2_`depvar'_2002_`y' if majority_white==0 [aw=sh_district_2002]
		ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0 [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}


*******************************************************************
* TABLE S12 (FIGURE A1): CHANGE IN CONTRIBUTIONS BY DONOR IDEOLOGY, 2002-20XX, WHITE VS NONWHITE AREAS
*******************************************************************

* panel A: contributions by ideology tercile and period, majority white counties
forvalues k=1(1)3 {
   forvalues y=2004(2)2016 {
		local depvar="cont_tcile`k'"
		disp "majority NH-white counties: contributions tercile `k', year 2002-`y'"
		summ dhs2_`depvar'_2002_`y' if majority_white==1 [aw=sh_district_2002] 
		ivreg2 dhs2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1 [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}

* panel B: contributions by ideology tercile and period, majority nonwhite/Hispanic counties
forvalues k=1(1)3 {
   forvalues y=2004(2)2016 {
		local depvar="cont_tcile`k'"
		disp "minority NH-white counties: contributions tercile `k', year 2002-`y'"
		summ dhs2_`depvar'_2002_`y' if majority_white==0 [aw=sh_district_2002] 
		ivreg2 dhs2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0 [aw=sh_district_2002], cluster(czone congressionaldistrict)
	}
}


*******************************************************************
* TABLE S13 (FIGURE S5): ELECTION PROBABILITY BY PARTY, 2002-20XX, WHITE VS NONWHITE AREAS
*******************************************************************

* panel A: Republican election probability, majority white counties
forvalues y=2004(2)2016 {
	local depvar="rwin"
	disp "Republican victory, year 2002-`y'"
	summ d2_`depvar'_2002_`y' if majority_white==1 [aw=sh_district_2002]
	ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1 [aw=sh_district_2002], cluster(czone congressionaldistrict)
}

* panel B: Republican election probability, majority nonwhite/Hispanic counties
forvalues y=2004(2)2016 {
	local depvar="rwin"
	disp "Republican victory, year 2002-`y'"
	summ d2_`depvar'_2002_`y' if majority_white==0 [aw=sh_district_2002]
	ivreg2 d2_`depvar'_2002_`y' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0 [aw=sh_district_2002], cluster(czone congressionaldistrict)
}





log close
