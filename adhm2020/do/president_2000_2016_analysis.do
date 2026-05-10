*******************************************************************
* Analysis of Presidential Elections 2000-2016
*******************************************************************

* David Dorn, final version February 28, 2020

* Input file: president_2000_2016.dta


*******************************************************************
* Administrative commands
*******************************************************************

cap log close
set more off
clear matrix
clear

log using ../log/president_2000_2016_analysis.log, text replace

use ../dta/president_2000_2016.dta, clear


*******************************************************************
* TABLE S2: DESCRIPTIVES FOR TRADE EXPOSURE
*******************************************************************

* column 2: import exposure 2000-2008
gen weight=totvote_2000pres
summ d_imp_usch_pd_0008 [aw=weight], detail

* note: column 1 of this table is created by house_2002_2016_analysis.do


*******************************************************************
* TABLE 5: REPUBLICAN VOTE SHARE IN PRESIDENTIAL ELECTIONS
*******************************************************************

gen depvar=.

* panel A: 2000 - 2008
replace depvar=shnr_pres2008-shnr_pres2000
summ depvar d_imp_usch_pd_0008 shnr_pres2008 shnr_pres2000 if depvar!=. [aw=weight], detail
reg depvar d_imp_usch_pd_0008 [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource reg* [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres1992 shnr_pres1996 reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic [aw=weight], cluster(czone)

* panel B: 2000 - 2016
replace depvar=shnr_pres2016-shnr_pres2000
summ shnr_pres2016 shnr_pres2016 shnr_pres2000 if depvar!=. [aw=weight], detail
reg depvar d_imp_usch_pd_0008 [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource reg* [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic [aw=weight], cluster(czone)
ivreg2 depvar (d_imp_usch_pd_0008=d_imp_otch_lag_pd_0008) l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres1992 shnr_pres1996 reg* l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic [aw=weight], cluster(czone)


log close



