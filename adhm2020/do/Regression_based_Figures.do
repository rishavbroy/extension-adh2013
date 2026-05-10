
* This files generates regression based graphs in the paper 



use ../dta/house_2002_2016.dta, clear
set more off

* vector of full controls
local full_ctrl="reg* l_shind_manuf_cbp l_sh_routine33 l_task_outsource shnr_pres2000 shnr_pres1996 l_sh_pop_f l_sh_pop_edu_c l_sh_fborn l_sh_pop_age_1019 l_sh_pop_age_2029 l_sh_pop_age_3039 l_sh_pop_age_4049 l_sh_pop_age_5059 l_sh_pop_age_6069 l_sh_pop_age_7079 l_sh_pop_age_8000 l_sh_pop_white l_sh_pop_black l_sh_pop_asian l_sh_pop_hispanic"

gen samevar=d_imp_usch_pd


*******************************************************************
* Figure 4 
*******************************************************************

foreach donorType in ""  {
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr=2004(2)2016 {
		local ideoCount = 1
		forvalues k=1(1)3 {
			local depvar="cont_tcile`k'`donorType'"
			* save summary stats about dependent variable
			summ dhs2_`depvar'_2002_`yr' [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			* save regression output
			ivreg2 dhs2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			* save graph features
			mat x[`row', 3] = `ideoCount'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
			local ideoCount = `ideoCount' + 1
		}
		local x_pos = `x_pos' + 1
	}
	preserve
	clear
	set obs 21
	svmat x
	rename x1 point
	rename x2 se
	rename x3 tercile
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(rows(3) region(col(white)) order(1 "liberal donors" 2 "moderate donors" 3 "conservative donors")) ///
		xlabel( 2 "2002-04" 6 "2002-06" 10 "2002-08" 14 "2002-10" 18 "2002-12" 22 "2002-14" 26 "2002-16", noticks) xtitle("") ///
		graphregion(color(white)) ytitle(100 x proportionate change in campaign contributions, size(small)) ylabel(-100(50)200, gmax gmin)
	    graph export ../gph/Fig4_Regressions_Contributions.pdf, replace
	
	restore
}

*******************************************************************
* Figure A1
*******************************************************************

foreach split in above  {
	local above_direc >
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		forvalues tcile = 1(1)3  {
			summ dhs2_cont_tcile`tcile'_2002_`yr'  [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			ivreg2 dhs2_cont_tcile`tcile'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1 [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			mat x[`row', 3] = `tcile'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
		}
		local x_pos = `x_pos' + 1
	}
	local yrange_above -100(50)200

	preserve
	clear
	set obs 21
	svmat x
	rename x1 point
	rename x2 se
	rename x3 tercile
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black)), ///
		legend(rows(3) region(col(white)) order(1 "liberal donors" 2 "moderate donors" 3 "conservative donors")) ///
		xlabel( 2 "2002-04" 6 "2002-06" 10 "2002-08" 14 "2002-10" 18 "2002-12" 22 "2002-14" 26 "2002-16", noticks) xtitle("")   ///
		graphregion(color(white)) ytitle(100 x proportionate change in campaign contributions, size(small)) ylabel(`yrange_`split'', gmax gmin)
	graph export ../gph/AppFig1a_Regressions_Whites_Contributions.pdf, replace
	restore
}

*
foreach split in below  {
	
	local below_direc <=
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		forvalues tcile = 1(1)3  {
			summ dhs2_cont_tcile`tcile'_2002_`yr'  [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			ivreg2 dhs2_cont_tcile`tcile'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0 [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			mat x[`row', 3] = `tcile'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
		}
		local x_pos = `x_pos' + 1
	}
	
	local yrange_below -150(50)300
	preserve
	clear
	set obs 21
	svmat x
	rename x1 point
	rename x2 se
	rename x3 tercile
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black)), ///
		legend(rows(3) region(col(white)) order(1 "liberal donors" 2 "moderate donors" 3 "conservative donors")) ///
		xlabel( 2 "2002-04" 6 "2002-06" 10 "2002-08" 14 "2002-10" 18 "2002-12" 22 "2002-14" 26 "2002-16", noticks) xtitle("")   ///
		graphregion(color(white)) ytitle(100 x proportionate change in campaign contributions, size(small)) ylabel(`yrange_`split'', gmax gmin)
	graph export ../gph/AppFig1b_Regressions_NonWhites_Contributions.pdf, replace
	restore
}


*******************************************************************
* Figure 5a 
*******************************************************************

foreach depvar in rwin  {
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr=2004(2)2016 {
		disp "Republican two-party vote share, year 2002-`y'"
		summ d2_`depvar'_2002_`yr' [aw=sh_district_2002]
		mat x[`row', 6] = r(mean)
		mat x[`row', 7] = r(sd)
		ivreg2 d2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
		mat def A = e(b)
		mat def B = e(V)
		mat x[`row', 1] = A[1,1]
		mat x[`row', 2] = (B[1,1]) ^ (1/2)
		mat x[`row', 3] = 1
		mat x[`row', 4] = `yr'
		mat x[`row', 5] = `x_pos'
		local row = `row' + 1
		local x_pos = `x_pos' + 2
	}
	local rwin_ylabel Change in Probability Republican is Elected
	local shnr_ylabel Change in Republican Two-Party Vote Share
	local rwin_yrange ylabel(-20(10)60, gmax gmin)
	local shnr_yrange ylabel(-20(5)15, gmax gmin)
	preserve
	clear
	set obs 7
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(off) ///
		xlabel( 1 "2002-04" 3 "2002-06" 5 "2002-08" 7 "2002-10" 9 "2002-12" 11 "2002-14" 13 "2002-16", noticks) xtitle("")  ///
		graphregion(color(white)) ytitle(``depvar'_ylabel', size(small)) ``depvar'_yrange'
	graph export ../gph/Fig5a_Regs_Repub_Win_Share.pdf, replace
	restore
}

*******************************************************************
* Figure 5b 
*******************************************************************

foreach depvar in shnr  {
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr=2004(2)2016 {
		disp "Republican two-party vote share, year 2002-`y'"
		summ d2_`depvar'_2002_`yr' [aw=sh_district_2002]
		mat x[`row', 6] = r(mean)
		mat x[`row', 7] = r(sd)
		ivreg2 d2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
		mat def A = e(b)
		mat def B = e(V)
		mat x[`row', 1] = A[1,1]
		mat x[`row', 2] = (B[1,1]) ^ (1/2)
		mat x[`row', 3] = 1
		mat x[`row', 4] = `yr'
		mat x[`row', 5] = `x_pos'
		local row = `row' + 1
		local x_pos = `x_pos' + 2
	}
	local rwin_ylabel Change in Probability Republican is Elected
	local shnr_ylabel Change in Republican Two-Party Vote Share
	local rwin_yrange ylabel(-20(10)60, gmax gmin)
	local shnr_yrange ylabel(-20(5)15, gmax gmin)
	preserve
	clear
	set obs 7
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(off) ///
		xlabel( 1 "2002-04" 3 "2002-06" 5 "2002-08" 7 "2002-10" 9 "2002-12" 11 "2002-14" 13 "2002-16", noticks) xtitle("")  ///
		graphregion(color(white)) ytitle(``depvar'_ylabel', size(small)) ``depvar'_yrange'
	graph export ../gph/Fig5b_Regs_Repub_Win_Share.pdf, replace
	restore
}

*******************************************************************
* Figure S5a 
*******************************************************************

foreach depvar in rwin  {
foreach split in above  {
	local above_direc >
	local below_direc <=
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		disp "Republican two-party vote share, year 2002-`y'"
		summ d2_`depvar'_2002_`yr' [aw=sh_district_2002]
		mat x[`row', 6] = r(mean)
		mat x[`row', 7] = r(sd)
		ivreg2 d2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1  [aw=sh_district_2002], cluster(czone congressionaldistrict)
		mat def A = e(b)
		mat def B = e(V)
		mat x[`row', 1] = A[1,1]
		mat x[`row', 2] = (B[1,1]) ^ (1/2)
		mat x[`row', 3] = 1
		mat x[`row', 4] = `yr'
		mat x[`row', 5] = `x_pos'
		local row = `row' + 1
		local x_pos = `x_pos' + 2
	}
	local rwin_ylabel Change in Probability Republican is Elected
	local shnr_ylabel Change in Republican Two-Party Vote Share
	local rwin_above_yrange ylabel(-20(10)60, gmax gmin)
	local rwin_below_yrange ylabel(-30(10)70, gmax gmin)
	preserve
	clear
	set obs 7
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(off) ///
		xlabel( 1 "2002-04" 3 "2002-06" 5 "2002-08" 7 "2002-10" 9 "2002-12" 11 "2002-14" 13 "2002-16", noticks) xtitle("")  ///
		graphregion(color(white)) ytitle(``depvar'_ylabel', size(small))  ``depvar'_`split'_yrange'
	graph export ../gph/SuppFig5a_Regressions_White_RepublicanWin.pdf, replace
	restore
}
}

*******************************************************************
* Figure S5b 
*******************************************************************

foreach depvar in rwin  {
foreach split in above  {
	local above_direc >
	local below_direc <=
	mat def x = J(21,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		disp "Republican two-party vote share, year 2002-`y'"
		summ d2_`depvar'_2002_`yr' [aw=sh_district_2002]
		mat x[`row', 6] = r(mean)
		mat x[`row', 7] = r(sd)
		ivreg2 d2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0  [aw=sh_district_2002], cluster(czone congressionaldistrict)
		mat def A = e(b)
		mat def B = e(V)
		mat x[`row', 1] = A[1,1]
		mat x[`row', 2] = (B[1,1]) ^ (1/2)
		mat x[`row', 3] = 1
		mat x[`row', 4] = `yr'
		mat x[`row', 5] = `x_pos'
		local row = `row' + 1
		local x_pos = `x_pos' + 2
	}
	local rwin_ylabel Change in Probability Republican is Elected
	local shnr_ylabel Change in Republican Two-Party Vote Share
	local rwin_above_yrange ylabel(-20(10)60, gmax gmin)
	local rwin_below_yrange ylabel(-30(10)70, gmax gmin)
	preserve
	clear
	set obs 7
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(off) ///
		xlabel( 1 "2002-04" 3 "2002-06" 5 "2002-08" 7 "2002-10" 9 "2002-12" 11 "2002-14" 13 "2002-16", noticks) xtitle("")  ///
		graphregion(color(white)) ytitle(``depvar'_ylabel', size(small))  ``depvar'_`split'_yrange'
	graph export ../gph/SuppFig5b_Regressions_NonWhite_RepublicanWin.pdf, replace
	restore
}
}

*******************************************************************
* Figure 6 
*******************************************************************

foreach depType in "cfavg"  {
	mat def x = J(28,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr=2004(2)2016 {
		local ideoCount = 1
		foreach k in demlib demmod repmod repcon {
			local depvar="`depType'_`k'"
			summ d2_`depvar'_2002_`yr'  [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			ivreg2 d2_`depvar'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			mat x[`row', 3] = `ideoCount'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
			local ideoCount = `ideoCount' + 1
		}
		local x_pos = `x_pos' + 1
	}
	preserve
	clear
	set obs 28
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1, color(navy)) ///
		(bar point x_pos if ideology == 2, color(forest_green)) ///
		(bar point x_pos if ideology == 3, color(dkorange)) ///
		(bar point x_pos if ideology == 4, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(row(2) region(col(white)) order(1 "liberal dems" 2 "moderate dems" 3 "moderate repubs" 4 "conservative repubs")) ///
		xlabel( 2.5 "2002-04" 7.5 "2002-06" 12.5 "2002-08" 17.5 "2002-10" 22.5 "2002-12" 27.5 "2002-14" 32.5 "2002-16", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(-60(20)60)  ytitle(100 x Change in Win Probability by Party and Political Position, size(small))
	graph export ../gph/Fig6_Regressions_WinnerIdeology.pdf, replace 
	restore
}



*******************************************************************
* Figure 7a
*******************************************************************
foreach split in above  {
	local above_direc >
	mat def x = J(28,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		local ideoCount = 1
		foreach ideoType in demlib demmod repmod repcon  {
			summ d2_cfavg_`ideoType'_2002_`yr'  [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			ivreg2 d2_cfavg_`ideoType'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==1 [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			mat x[`row', 3] = `ideoCount'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
			local ideoCount = `ideoCount' + 1
		}
		local x_pos = `x_pos' + 1
	}
	local yrange_above -75(25)75
	preserve
	clear
	set obs 21
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1, color(navy)) ///
		(bar point x_pos if ideology == 2, color(forest_green)) ///
		(bar point x_pos if ideology == 3, color(dkorange)) ///
		(bar point x_pos if ideology == 4, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black)), ///
		legend(row(2) region(col(white)) order(1 "liberal dems" 2 "moderate dems" 3 "moderate repubs" 4 "conservative repubs")) ///
		xlabel( 2.5 "2002-04" 7.5 "2002-06" 12.5 "2002-08" 17.5 "2002-10" 22.5 "2002-12" 27.5 "2002-14" 32.5 "2002-16", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(`yrange_`split'', gmax gmin) ytitle(100 x Change in Win Probability by Party and Political Position, size(small))
	graph export ../gph/Fig7a_Regressions_Whites_WinnerIdeology.pdf, replace 
	
	restore
}

*******************************************************************
* Figure 7b
*******************************************************************
foreach split in below  {
	local below_direc <=
	mat def x = J(28,7,.)
	local row = 1
	local x_pos = 1
	forvalues yr = 2004(2)2016  {
		local ideoCount = 1
		foreach ideoType in demlib demmod repmod repcon  {
			summ d2_cfavg_`ideoType'_2002_`yr'  [aw=sh_district_2002]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			ivreg2 d2_cfavg_`ideoType'_2002_`yr' (d_imp_usch_pd=d_imp_otch_lag_pd) `full_ctrl' if majority_white==0 [aw=sh_district_2002], cluster(czone congressionaldistrict)
			mat def A = e(b)
			mat def B = e(V)
			mat x[`row', 1] = A[1,1]
			mat x[`row', 2] = (B[1,1]) ^ (1/2)
			mat x[`row', 3] = `ideoCount'
			mat x[`row', 4] = `yr'
			mat x[`row', 5] = `x_pos'
			local row = `row' + 1
			local x_pos = `x_pos' + 1
			local ideoCount = `ideoCount' + 1
		}
		local x_pos = `x_pos' + 1
	}
	
	local yrange_below -100(25)100
	preserve
	clear
	set obs 21
	svmat x
	rename x1 point
	rename x2 se
	rename x3 ideology
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if ideology == 1, color(navy)) ///
		(bar point x_pos if ideology == 2, color(forest_green)) ///
		(bar point x_pos if ideology == 3, color(dkorange)) ///
		(bar point x_pos if ideology == 4, color(maroon)) ///
		(rcap se_h se_l x_pos, lc(black)), ///
		legend(row(2) region(col(white)) order(1 "liberal dems" 2 "moderate dems" 3 "moderate repubs" 4 "conservative repubs")) ///
		xlabel( 2.5 "2002-04" 7.5 "2002-06" 12.5 "2002-08" 17.5 "2002-10" 22.5 "2002-12" 27.5 "2002-14" 32.5 "2002-16", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(`yrange_`split'', gmax gmin) ytitle(100 x Change in Win Probability by Party and Political Position, size(small))
	graph export ../gph/Fig7b_Regressions_NonWhites_WinnerIdeology.pdf, replace 
	
	restore
}


