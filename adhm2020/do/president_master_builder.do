/****************************************************************************

This do-file creates president_2000_2016.dta

Final version: David Dorn, May 12, 2020

*****************************************************************************/


* county-level variables
use ../Cty/cty_variables.dta, clear

* drop Broomfield Cty, which has no votes in 2000 and thus has zero weight, and Bedford which ceased to exist
drop if cty_fips==8014 | cty_fips==51515
sort cty_fips 
save temp_vote.dta, replace


* add geography variables
use ../President/dta/cw_cty_czone.dta
keep cty_fips czone
sort cty_fips
save temp_cty.dta, replace
use temp_vote.dta, clear
merge cty_fips using temp_cty.dta
tab _merge
* note: states 2 and 15 are not covered in the voting data analysis; counties 30113 and 51560 and 51780 and 51515 were eliminated after 1990
replace state_fips=floor(cty_fips/1000) if state_fips==.
tab _merge
assert _merge==3 if state_fips!=2 & state_fips!=15 & cty_fips!=30113 & cty_fips!=51515 & cty_fips!=51560 & cty_fips!=51780 
keep if _merge==3
drop _merge
erase temp_vote.dta
erase temp_cty.dta

* Census division dummies 
rename stateAbb state
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

sort czone
save temp_vote.dta, replace


* add CZ variables
use ../CZ/cz_variables.dta, clear
keep  czone d_imp_usch_pd_0008 d_imp_otch_lag_pd_0008 l_shind_manuf_cbp l_sh_routine33 l_task_outsource 
merge czone using temp_vote.dta
assert _merge!=2
keep if _merge==3
drop _merge
erase temp_vote.dta


* variable definitions and scaling

* Controls: drop omitted geography and population categories
rename reg_neweng neweng

* Outcomes: scaling
foreach var of varlist sh* {
   replace `var'=100*`var'
}

* Controls: scaling
foreach var of varlist *routine* {
   replace `var'=100*`var'
}

keep cty_fips czone shnr_pres2000 shnr_pres2008 shnr_pres2016 totvote_2000pres d_imp_usch_pd_0008 d_imp_otch_lag_pd_0008 l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres1992 shnr_pres1996 reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic 
foreach var of varlist cty_fips czone shnr_pres2000 shnr_pres2008 shnr_pres2016 totvote_2000pres d_imp_usch_pd_0008 d_imp_otch_lag_pd_0008 l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres1992 shnr_pres1996 reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic {
   label variable `var' ""
}
order cty_fips czone shnr_pres2000 shnr_pres2008 shnr_pres2016 totvote_2000pres d_imp_usch_pd_0008 d_imp_otch_lag_pd_0008 l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres1992 shnr_pres1996 reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic 

save president_2000_2016.dta, replace



