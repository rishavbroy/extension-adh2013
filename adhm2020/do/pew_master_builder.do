/****************************************************************************

This do-file creates pew_2004_2015.dta

Final version: David Dorn, May 12, 2020

*****************************************************************************/

* Create tempfiles (to store each cleaned year before appending occurs)

tempfile clean2004adj
tempfile clean2011adj
tempfile clean2014adj
tempfile clean2015adj

* 2004 data

import delimited "../PEW/dta/data2004.csv", varnames(1) clear

gen year = 2004

rename rid respid
rename cregion census_region
rename fips cty_fips

gen married = 0
replace married = 1 if marital == 1
drop marital

label define sex 1 "Male" 2 "Female"
label values sex sex

gen race_ = .
replace race_ = 1 if race == 1
replace race_ = 2 if race == 2
replace race_ = 3 if race != 1 & race != 2 & race != 9
drop race
rename race_ race
label define race 1 "White" 2 "Black" 3 "Other" 
label values race race

replace hisp = 2 if hisp != 1 & hisp != 9
replace hisp = . if hisp == 9
label define hisp 1 "Hispanic" 2 "Non-Hispanic" 
label values hisp hisp

gen edcat = .
replace edcat = 1 if educ == 1 | educ == 2 | educ == 3
replace edcat = 2 if educ == 4 | educ == 5
replace edcat = 3 if educ == 6 | educ == 7 | educ == 8
label define edcat 1 "HS or Less" 2 "Some College" 3 "College Graduate"
label values edcat edcat
drop educ

gen values_scale_pm10 = 0

replace values_scale_pm10 = values_scale_pm10+1 if q11a == 1 | q11a == 2
replace values_scale_pm10 = values_scale_pm10-1 if q11a == 3 | q11a == 4

replace values_scale_pm10 = values_scale_pm10-1 if q11b == 1 | q11b == 2
replace values_scale_pm10 = values_scale_pm10+1 if q11b == 3 | q11b == 4

replace values_scale_pm10 = values_scale_pm10+1 if q11c == 1 | q11c == 2
replace values_scale_pm10 = values_scale_pm10-1 if q11c == 3 | q11c == 4

replace values_scale_pm10 = values_scale_pm10-1 if q11d == 1 | q11d == 2
replace values_scale_pm10 = values_scale_pm10+1 if q11d == 3 | q11d == 4

replace values_scale_pm10 = values_scale_pm10-1 if q11f == 1 | q11f == 2
replace values_scale_pm10 = values_scale_pm10+1 if q11f == 3 | q11f == 4

replace values_scale_pm10 = values_scale_pm10-1 if q11g == 1 | q11g == 2
replace values_scale_pm10 = values_scale_pm10+1 if q11g == 3 | q11g == 4

replace values_scale_pm10 = values_scale_pm10+1 if q11i == 1 | q11i == 2
replace values_scale_pm10 = values_scale_pm10-1 if q11i == 3 | q11i == 4

replace values_scale_pm10 = values_scale_pm10-1 if q11n == 1 | q11n == 2
replace values_scale_pm10 = values_scale_pm10+1 if q11n == 3 | q11n == 4

replace values_scale_pm10 = values_scale_pm10+1 if q20r == 1 | q20r == 2
replace values_scale_pm10 = values_scale_pm10-1 if q20r == 3 | q20r == 4

replace values_scale_pm10 = values_scale_pm10-1 if q20u == 1 | q20u == 2
replace values_scale_pm10 = values_scale_pm10+1 if q20u == 3 | q20u == 4

keep respid census_region state cty_fips weight age income sex race hisp edcat values_scale_pm10 year

save `clean2004adj', replace


* 2011 data

import delimited "../PEW/dta/data2011.csv", varnames(1) clear

gen year = 2011

rename mergeid respid
rename cregion census_region
rename fips cty_fips

label define sex 1 "Male" 2 "Female"
label values sex sex

gen race_ = .
replace race_ = 1 if race1m1 == 1
replace race_ = 2 if race1m1 == 2
replace race_ = 3 if race1m1 != 1 & race1m1 != 2 & race1m1 != 9
drop race1m1
rename race_ race
label define race 1 "White" 2 "Black" 3 "Other"
label values race race

gen hisp = 1 if hisp4 == 1
replace hisp = 2 if hisp4 != 1 & hisp4 != 9
replace hisp = . if hisp4 == 9
drop hisp4
label define hisp 1 "Hispanic" 2 "Non-Hispanic" 
label values hisp hisp

tab race hisp [aw=weight]

gen edcat = .
replace edcat = 1 if educ == 1 | educ == 2 | educ == 3
replace edcat = 2 if educ == 4 | educ == 5
replace edcat = 3 if educ == 6 | educ == 7 | educ == 8
label define edcat 1 "HS or Less" 2 "Some College" 3 "College Graduate"
label values edcat edcat
drop educ

gen values_scale_pm10 = 0

replace values_scale_pm10 = values_scale_pm10+1 if q17a == 1 | q17a == 2
replace values_scale_pm10 = values_scale_pm10-1 if q17a == 3 | q17a == 4

replace values_scale_pm10 = values_scale_pm10-1 if q17b == 1 | q17b == 2
replace values_scale_pm10 = values_scale_pm10+1 if q17b == 3 | q17b == 4

replace values_scale_pm10 = values_scale_pm10+1 if q17c == 1 | q17c == 2
replace values_scale_pm10 = values_scale_pm10-1 if q17c == 3 | q17c == 4

replace values_scale_pm10 = values_scale_pm10-1 if q17d == 1 | q17d == 2
replace values_scale_pm10 = values_scale_pm10+1 if q17d == 3 | q17d == 4

replace values_scale_pm10 = values_scale_pm10-1 if q17f == 1 | q17f == 2
replace values_scale_pm10 = values_scale_pm10+1 if q17f == 3 | q17f == 4

replace values_scale_pm10 = values_scale_pm10-1 if q17g == 1 | q17g == 2
replace values_scale_pm10 = values_scale_pm10+1 if q17g == 3 | q17g == 4

replace values_scale_pm10 = values_scale_pm10+1 if q17i == 1 | q17i == 2
replace values_scale_pm10 = values_scale_pm10-1 if q17i == 3 | q17i == 4

replace values_scale_pm10 = values_scale_pm10-1 if q17n == 1 | q17n == 2
replace values_scale_pm10 = values_scale_pm10+1 if q17n == 3 | q17n == 4

replace values_scale_pm10 = values_scale_pm10+1 if q37r == 1 | q37r == 2
replace values_scale_pm10 = values_scale_pm10-1 if q37r == 3 | q37r == 4

replace values_scale_pm10 = values_scale_pm10-1 if q37u == 1 | q37u == 2
replace values_scale_pm10 = values_scale_pm10+1 if q37u == 3 | q37u == 4

keep respid census_region state cty_fips weight age income sex race hisp edcat values_scale_pm10 year

save `clean2011adj', replace


* 2014 data

import delimited "../PEW/dta/dataset.csv", varnames(1) clear

gen year = 2014

rename cregion census_region
rename fips cty_fips
rename ideoconsist values_scale_pm10

label define sex 1 "Male" 2 "Female"
label values sex sex

tab sex [aw=weight]

gen race_ = .
replace race_ = 1 if racem1 == 1
replace race_ = 2 if racem1 == 2
replace race_ = 3 if racem1 != 1 & racem1 != 2 & racem1 != 9
drop racem1
rename race_ race
label define race 1 "White" 2 "Black" 3 "Other"
label values race race

replace hisp = 2 if hisp != 1 & hisp != 9
replace hisp = . if hisp == 9
label define hisp 1 "Hispanic" 2 "Non-Hispanic" 
label values hisp hisp

tab race hisp [aw=weight]

gen edcat = .
replace edcat = 1 if educ == 1 | educ == 2 | educ == 3
replace edcat = 2 if educ == 4 | educ == 5
replace edcat = 3 if educ == 6 | educ == 7 | educ == 8
label define edcat 1 "HS or Less" 2 "Some College" 3 "College Graduate"
label values edcat edcat
drop educ

tab edcat [aw=weight]

keep respid census_region state cty_fips weight age income sex race hisp edcat values_scale_pm10 year

save `clean2014adj', replace


* 2015

import delimited "../PEW/dta/data2015.csv", varnames(1) clear

gen year = 2015

rename cregion census_region
rename fips cty_fips

label define sex 1 "Male" 2 "Female"
label values sex sex

gen race_ = .
replace race_ = 1 if racem1 == 1
replace race_ = 2 if racem1 == 2
replace race_ = 3 if racem1 != 1 & racem1 != 2 & racem1 != 9
drop racem1
rename race_ race
label define race 1 "White" 2 "Black" 3 "Other"
label values race race

replace hisp = 2 if hisp != 1 & hisp != 9
replace hisp = . if hisp == 9
label define hisp 1 "Hispanic" 2 "Non-Hispanic" 
label values hisp hisp

tab race hisp [aw=weight]

gen edcat = .
replace edcat = 1 if educ == 1 | educ == 2 | educ == 3
replace edcat = 2 if educ == 4 | educ == 5
replace edcat = 3 if educ == 6 | educ == 7 | educ == 8
label define edcat 1 "HS or Less" 2 "Some College" 3 "College Graduate"
label values edcat edcat
drop educ

gen values_scale_pm10 = 0

replace values_scale_pm10 = values_scale_pm10+1 if q42a == 1 
replace values_scale_pm10 = values_scale_pm10-1 if q42a == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42b == 1 
replace values_scale_pm10 = values_scale_pm10+1 if q42b == 2

replace values_scale_pm10 = values_scale_pm10+1 if q42c == 1 
replace values_scale_pm10 = values_scale_pm10-1 if q42c == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42d == 1 
replace values_scale_pm10 = values_scale_pm10+1 if q42d == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42f == 1 
replace values_scale_pm10 = values_scale_pm10+1 if q42f == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42g == 1 
replace values_scale_pm10 = values_scale_pm10+1 if q42g == 2

replace values_scale_pm10 = values_scale_pm10+1 if q42h == 1 
replace values_scale_pm10 = values_scale_pm10-1 if q42h == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42i == 1
replace values_scale_pm10 = values_scale_pm10+1 if q42i == 2

replace values_scale_pm10 = values_scale_pm10+1 if q106o == 1
replace values_scale_pm10 = values_scale_pm10-1 if q106o == 2

replace values_scale_pm10 = values_scale_pm10-1 if q42m == 1 
replace values_scale_pm10 = values_scale_pm10+1 if q42m == 2

keep respid census_region state cty_fips weight age income sex race hisp edcat values_scale_pm10 year

save `clean2015adj', replace


* merge years

use `clean2004adj', clear
append using `clean2011adj'
append using `clean2014adj'
append using `clean2015adj'


* add geography variables

* county code corrections 
* recode Miami-Dade
replace cty_fips=12025 if cty_fips==12086
* recode Oglala Lakota
replace cty_fips=46113 if cty_fips==46102
* Broomfield Cty: use data from Boulder Cty
replace cty_fips=8013 if cty_fips==8014
* drop AK, HI, PR
drop if state==2 | state==15 | state==72

* Census division identifiers
gen reg_neweng=(state==23 | state==33 | state==50 | state==25 | state==44 | state==9)
gen reg_midatl=(state==36 | state==42 | state==34)
gen reg_encen=(state==39 | state==26 | state==18 | state==17 | state==55)
gen reg_wncen=(state==27 | state==19 | state==29 | state==20 | state==31 | state==46 | state==38)
gen reg_satl=(state==10 | state==24 | state==11 | state==51 | state==54 | state==37 | state==45 | state==13 | state==12)
gen reg_escen=(state==21 | state==47 | state==1 | state==28)
gen reg_wscen=(state==5 | state==22 | state==40 | state==48)
gen reg_mount=(state==30 | state==16 | state==56 | state==8 | state==49 | state==32 | state==4 | state==35)
gen reg_pacif=(state==6 | state==41 | state==53 | state==15 | state==2)

sort cty_fips 
save temp.dta, replace

* CZ codes
use ../PEW/dta/cw_cty_czone.dta, clear
merge cty_fips using temp.dta
tab _merge
assert _merge!=2
* drop counties that don't appear in Pew data
drop if _merge==1
drop _merge

sort czone
save temp.dta, replace


* add CZ-level variables

use ../CZ/cz_variables.dta, clear
keep czone d_imp_usch_pd d_imp_otch_lag_pd l_shind_manuf_cbp l_sh_routine33 l_task_outsource reg_*
sort czone
merge czone using temp.dta
tab _merge
* drop CZ that don't appear in Pew data
drop if _merge==1
drop _merge

sort cty_fips
save temp.dta, replace

* add Cty-level variables

use ../Cty/cty_variables.dta, clear
keep cty_fips shnr_pres2000 shnr_pres1996
sort cty_fips
merge cty_fips using temp.dta
tab _merge

* drop counties that don't appear in Pew data
drop if _merge==1
drop _merge
erase temp.dta


* variable definitions

* period dummy 
gen t2=(year==2011 | year==2014 | year==2015 | year==2017)
gen t11=(year==2011)
gen t15=(year==2015)

* demographics
gen age2=age*age
replace sex=0 if sex==2 /* male dummy */
gen wnh=(race==1 & hisp==2)
gen black=(race==2 & hisp==2)
gen hispanic=(hisp==1)
gen other=1-wnh-hispanic-black
assert wnh+black+hispanic+other==1
gen missing=(race==. | hisp==.)

* shock x post interaction
gen d_imp_usch_pd_t2=d_imp_usch_pd*t2
gen d_imp_otch_lag_pd_t2=d_imp_otch_lag_pd*t2

* shock x race interaction
gen d_imp_usch_pd_t2w=d_imp_usch_pd*t2*(wnh==1)
gen d_imp_otch_lag_pd_t2w=d_imp_otch_lag_pd*t2*(wnh==1)
gen d_imp_usch_pd_t2nw=d_imp_usch_pd*t2*(wnh==0)
gen d_imp_otch_lag_pd_t2nw=d_imp_otch_lag_pd*t2*(wnh==0)
foreach var of varlist d_imp_usch_pd_t2w d_imp_usch_pd_t2nw d_imp_otch_lag_pd_t2w d_imp_otch_lag_pd_t2nw {
   replace `var'=. if other==1
}

* CZ control x 2014 interaction
gen l_shind_manuf_cbp_t2=l_shind_manuf_cbp*t2
gen l_task_outsource_t2=l_task_outsource*t2
gen l_sh_routine33_t2=l_sh_routine33*t2
gen shnr_pres2000_t2=shnr_pres2000*t2
gen shnr_pres1996_t2=shnr_pres1996*t2
foreach k in midatl encen wncen satl escen wscen mount pacif {
   gen t2reg_`k'=reg_`k'*t2
}
gen hispanic_t2=hispanic*t2
gen wnh_t2=wnh*t2
gen other_t2=other*t2
gen black_t2=black*t2
gen age_t2=age*t2
gen age2_t2=age2*t2
gen sex_t2=sex*t2
gen edcat1=(edcat==1)
gen edcat2=(edcat==2)
gen edcat3=(edcat==3)
gen edcat1_t2=(edcat==1)*t2
gen edcat2_t2=(edcat==2)*t2
gen edcat3_t2=(edcat==3)*t2

* rename outcome variable
rename values_scale_pm10 val10 

* balance weights by year
by year, sort: egen totwt=total(weight)
gen double adjweight=weight/totwt

* center/right/left views
gen center = 100*(abs(val10) <= 2)
gen right = 100*(val10 >= 3)
gen left = 100*(val10 <= -3)


* descriptives

* Pew scores
by year, sort: summ val10 left right center [aw=weight]
by year, sort: summ val10 left right center if wnh==0 [aw=weight]
by year, sort: summ val10 left right center if wnh==1 [aw=weight]

* trade shock
summ d_imp_usch_pd d_imp_otch_lag_pd [aw=adjweight], detail

* Means of control variables 
summ age age2 sex wnh hispanic edcat1 edcat2 edcat3 l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2 [aw=adjweight] 

* other variables used
summ t2 year adjweight czone [aw=adjweight] 

* number of observations by race
tab wnh
tab hispanic
tab black
tab other


* final variable name changes

replace weight=adjweight
gen female=sex-1
gen female_t2=female*t2
gen nhwhite=wnh
gen nhwhite_t2=wnh_t2
gen edu_hs=edcat2
gen edu_hs_t2=edcat2_t2
gen edu_c=edcat3
gen edu_c_t2=edcat3_t2
gen t14=(year==2014)

rename d_imp_usch_pd_t2w d_imp_usch_pd_t2nhw 
rename d_imp_usch_pd_t2nw d_imp_usch_pd_t2hb
rename d_imp_otch_lag_pd_t2w d_imp_otch_lag_pd_t2nhw 
rename d_imp_otch_lag_pd_t2nw d_imp_otch_lag_pd_t2hb


keep czone year t11 t14 t15 weight val10 left center right age age2 female nhwhite hispanic black edu_hs edu_c age_t2 age2_t2 female_t2 nhwhite_t2 hispanic_t2 black_t2 edu_hs_t2 edu_c_t2 d_imp_usch_pd_t2 d_imp_otch_lag_pd_t2 d_imp_usch_pd_t2nhw d_imp_usch_pd_t2hb d_imp_otch_lag_pd_t2nhw d_imp_otch_lag_pd_t2hb t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2
order czone year t11 t14 t15 weight val10 left center right age age2 female nhwhite hispanic black edu_hs edu_c age_t2 age2_t2 female_t2 nhwhite_t2 hispanic_t2 black_t2 edu_hs_t2 edu_c_t2 d_imp_usch_pd_t2 d_imp_otch_lag_pd_t2 d_imp_usch_pd_t2nhw d_imp_usch_pd_t2hb d_imp_otch_lag_pd_t2nhw d_imp_otch_lag_pd_t2hb t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2
foreach var of varlist czone year t11 t14 t15 weight val10 left center right age age2 female nhwhite hispanic black edu_hs edu_c age_t2 age2_t2 female_t2 nhwhite_t2 hispanic_t2 black_t2 edu_hs_t2 edu_c_t2 d_imp_usch_pd_t2 d_imp_otch_lag_pd_t2 d_imp_usch_pd_t2nhw d_imp_usch_pd_t2hb d_imp_otch_lag_pd_t2nhw d_imp_otch_lag_pd_t2hb t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2 {
 label variable `var' ""
 }
 
 
 save ../PEW/pew_2004_2015.dta, replace
