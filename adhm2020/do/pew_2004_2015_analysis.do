*******************************************************************
* PEW ideology 
*******************************************************************

* David Dorn, final version February 28, 2020


*******************************************************************
* Administrative Commands
*******************************************************************

cap log close                       /* closes open log files */
set more off                        /* tells Stata not to pause after each step of calculation */
clear                               /* clears current memory */
set memory 6g                       /* increases available memory */
set linesize 200					/* increases log table width */
set matsize 2000


log using ../log/pew_2004_2015_analysis.log, replace text

use ../dta/pew_2004_2015.dta, clear


*******************************************************************
* TABLE 1: IDEOLOGICAL POSITION BY YEAR AND RACIAL GROUP
*******************************************************************

* panel a: all races/ethnicities
by year, sort: summ val10 left center right [aw=weight]
* panel b: non-hispanic whites
by year, sort: summ val10 left center right if nhwhite==1 [aw=weight]
* panel c: hispanics and non-whites
by year, sort: summ val10 left center right if nhwhite==0 [aw=weight]


*******************************************************************
* TABLE S14: INDIVIDUAL-LEVEL PEW IDEOLOGY VALUES
*******************************************************************

* columns 1-5: overall effect
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic black edu_hs edu_c (d_imp_usch_pd_t2=d_imp_otch_lag_pd_t2) [aw=weight], a(czone) cluster(czone) ffirst
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic black edu_hs edu_c l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 (d_imp_usch_pd_t2=d_imp_otch_lag_pd_t2) [aw=weight], a(czone) cluster(czone) 	
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic black edu_hs edu_c shnr_pres1996_t2 shnr_pres2000_t2 l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 (d_imp_usch_pd_t2=d_imp_otch_lag_pd_t2) [aw=weight], a(czone) cluster(czone) 
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic black edu_hs edu_c t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2 (d_imp_usch_pd_t2=d_imp_otch_lag_pd_t2) [aw=weight], a(czone) cluster(czone) 	
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic black edu_hs edu_c age_t2 age2_t2 female_t2 nhwhite_t2 hispanic_t2 black_t2 edu_hs_t2 edu_c_t2 t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2 (d_imp_usch_pd_t2=d_imp_otch_lag_pd_t2) [aw=weight], a(czone) cluster(czone) 	

* columns 6-7: two-way race split non-Hispanic whites vs Hispanics and blacks
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic edu_hs edu_c (d_imp_usch_pd_t2nhw d_imp_usch_pd_t2hb=d_imp_otch_lag_pd_t2nhw d_imp_otch_lag_pd_t2hb) [aw=weight], a(czone) cluster(czone) 	
test d_imp_usch_pd_t2nhw=d_imp_usch_pd_t2hb
reghdfe val10 t11 t14 t15 age age2 female nhwhite hispanic edu_hs edu_c age_t2 age2_t2 female_t2 nhwhite_t2 hispanic_t2 edu_hs_t2 edu_c_t2 t2reg* l_shind_manuf_cbp_t2 l_sh_routine33_t2 l_task_outsource_t2 shnr_pres2000_t2 shnr_pres1996_t2 (d_imp_usch_pd_t2nhw d_imp_usch_pd_t2hb=d_imp_otch_lag_pd_t2nhw d_imp_otch_lag_pd_t2hb) [aw=weight], a(czone) cluster(czone) 	
test d_imp_usch_pd_t2nhw=d_imp_usch_pd_t2hb

* supplementary information: number of distinct CZs with data
unique czone
* supplementary information: number of CZs with 2004 and 2011-15 data
by czone, sort: egen obs2004=total(year==2004)
drop if obs2004==0
unique czone
drop obs2004


log close
