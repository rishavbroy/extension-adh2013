*******************************************************************
* Analysis of Nielsen Ratings and Market Shares, 2004-2016
*******************************************************************

* David Dorn, final version February 28, 2020

* Input file: nielsen_2004_2016.dta


*******************************************************************
* Administrative commands
*******************************************************************

cap log close
set more off
clear matrix
clear mata
clear
set maxvar 5000
set matsize 2000

log using ../log/nielsen_2004_2016_analysis.log, text replace

use ../dta/nielsen_2004_2016.dta, clear


*******************************************************************
* TABLE 2: NIELSEN RATINGS AND MARKET SHARES, 11/2004 - 11/2012
*******************************************************************

* sample period
gen in_sample=.
replace in_sample=(t_200411==1 | t_201211==1)

foreach var of varlist rtgALL mktshFXNC mktshCNN mktshMSNBC {
   summ `var' [aw=SOW] if in_sample==1
   summ `var' [aw=SOW] if in_sample==1 & t_200411==1
   summ `var' [aw=SOW] if in_sample==1 & t_201211==1
   disp "outcome `var'"
   reghdfe `var' group_* t_201211 t2_d_imp_usch_pd if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
   reghdfe `var' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
}


*******************************************************************
* TABLE S3: FOX NEWS MARKET SHARE, 11/2004 - 11/2008-2012-2016, GROUP INTERACTIONS
*******************************************************************

* column 1: period 11/2004 - 11/2008
replace in_sample=(t_200411==1 | t_200811==1)
foreach var of varlist mktshFXNC {
   summ `var' [aw=SOW] if in_sample==1
   summ `var' [aw=SOW] if in_sample==1 & t_200411==1
   summ `var' [aw=SOW] if in_sample==1 & t_200811==1
   reghdfe `var' group_* t2_group* t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t_200811 (w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd=w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   test w1834_t2_d_imp_usch_pd=nw1834_t2_d_imp_usch_pd
   test w3554_t2_d_imp_usch_pd=nw3554_t2_d_imp_usch_pd
   test w55up_t2_d_imp_usch_pd=nw55up_t2_d_imp_usch_pd
}

* column 2: period 11/2004 - 11/2012
replace in_sample=(t_200411==1 | t_201211==1)
foreach var of varlist mktshFXNC {
   summ `var' [aw=SOW] if in_sample==1
   summ `var' [aw=SOW] if in_sample==1 & t_200411==1
   summ `var' [aw=SOW] if in_sample==1 & t_201211==1
   reghdfe `var' group_* t2_group* t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t_201211 (w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd=w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   test w1834_t2_d_imp_usch_pd=nw1834_t2_d_imp_usch_pd
   test w3554_t2_d_imp_usch_pd=nw3554_t2_d_imp_usch_pd
   test w55up_t2_d_imp_usch_pd=nw55up_t2_d_imp_usch_pd
}

* column 3: period 11/2004 - 11/2016
replace in_sample=(t_200411==1 | t_201611==1)
foreach var of varlist mktshFXNC {
   summ `var' [aw=SOW] if in_sample==1
   summ `var' [aw=SOW] if in_sample==1 & t_200411==1
   summ `var' [aw=SOW] if in_sample==1 & t_201611==1
   reghdfe `var' group_* t2_group* t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource t2_shnr_pres2000 t2_shnr_pres1996 t2_reg* t_201611 (w1834_t2_d_imp_usch_pd w3554_t2_d_imp_usch_pd w55up_t2_d_imp_usch_pd nw1834_t2_d_imp_usch_pd nw3554_t2_d_imp_usch_pd nw55up_t2_d_imp_usch_pd=w1834_t2_d_imp_otch_lag_pd w3554_t2_d_imp_otch_lag_pd w55up_t2_d_imp_otch_lag_pd nw1834_t2_d_imp_otch_lag_pd nw3554_t2_d_imp_otch_lag_pd nw55up_t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   test w1834_t2_d_imp_usch_pd=nw1834_t2_d_imp_usch_pd
   test w3554_t2_d_imp_usch_pd=nw3554_t2_d_imp_usch_pd
   test w55up_t2_d_imp_usch_pd=nw55up_t2_d_imp_usch_pd
}


*******************************************************************
* TABLE S4: NIELSEN RATINGS AND MARKET SHARES, all months 2004 - all months 2012
*******************************************************************

replace in_sample=(t_200411==1 | t_201211==1 | t_200402==1 | t_201202==1 | t_200405==1 | t_201205==1 | t_200407==1 | t_201207==1)
foreach var of varlist rtgALL mktshFXNC mktshCNN mktshMSNBC {
   disp "outcome `var'"
   reghdfe `var' group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211  t2_d_imp_usch_pd if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)
   reghdfe `var' t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
   reghdfe `var' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_200402 t_200405 t_200407 t_201202 t_201205 t_201207 t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
}


*******************************************************************
* TABLE S7: NIELSEN RATINGS AND MARKET SHARES, 11/2004 - 11/2008-2012-2016
*******************************************************************

* column 1: 2004 - 2008
replace in_sample=(t_200411==1 | t_200811==1)
foreach var of varlist rtgALL mktshFXNC mktshCNN mktshMSNBC {
   disp "outcome `var', period 11/2004 - 11/2008"
   reghdfe `var' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_200811 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
}

* column 2: 2004 - 2012
replace in_sample=(t_200411==1 | t_201211==1)
foreach var of varlist rtgALL mktshFXNC mktshCNN mktshMSNBC {
   disp "outcome `var', period 11/2004 - 11/2012"
   reghdfe `var' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201211 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
}

* column 3: 2004 - 2016
replace in_sample=(t_200411==1 | t_201611==1)
foreach var of varlist rtgALL mktshFXNC mktshCNN mktshMSNBC {
   disp "outcome `var', period 11/2004 - 11/2016"
   reghdfe `var' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_201611 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
}


            
log close





