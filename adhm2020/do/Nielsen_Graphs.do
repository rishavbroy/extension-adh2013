

set more off
clear matrix
clear mata
clear
set maxvar 5000
set matsize 2000

use ../dta/nielsen_time_series.dta, clear

**** line graph by race, november only ***************

collapse (sum) imp* (sum) SOW, by(czone year white)
foreach channel in CNN MSNBC FXNC  {
	gen rtg`channel' = impressions`channel' / SOW * 100
}
** race trend -- line graph
foreach white in 1 {
	matrix x = J(13,4,.)
	local colCount = 1
	foreach channel in CNN MSNBC FXNC  {
		local rowCount = 1
		forvalues yr = 2004(1)2016  {
			sum rtg`channel' if year == `yr' & white == `white' [aw = SOW]
			matrix x[`rowCount', `colCount'] = `r(mean)'
			local rowCount = `rowCount' + 1
		}
		local colCount = `colCount' + 1
	}

	preserve

	svmat x
	keep x*
	drop if x1 == .
	gen year = _n + 2003
	rename x1 CNN
	rename x2 MSNBC
	rename x3 FXNC
	twoway (line FXNC year, lwidth(medthick) lcolor(maroon)) ///
			(line CNN year, lwidth(medthick) lcolor(forest_green)) ///
			(line MSNBC year, lwidth(medthick) lcolor(navy)), ///
		xlabel(2004(2)2016) /*name(r`graphCount')*/ ///
		graphregion(color(white)) /*title("``gType'`race'Label'")*/ yscale(range(0 3)) ///
		ylabel(0(0.5)3, format(%03.1f) gmax gmin) /*ytick(0(0.25)3)*/  ///
		xtitle("") ytitle(Percent of TV HHs Watching Network) legend(rows(1) region(col(white)))
 
	graph export ../gph/SuppFig1a_Descriptives_Whites_Nielsen.pdf, replace
	restore
}
*
** race trend -- line graph
foreach white in 0 {
	matrix x = J(13,4,.)
	local colCount = 1
	foreach channel in CNN MSNBC FXNC  {
		local rowCount = 1
		forvalues yr = 2004(1)2016  {
			sum rtg`channel' if year == `yr' & white == `white' [aw = SOW]
			matrix x[`rowCount', `colCount'] = `r(mean)'
			local rowCount = `rowCount' + 1
		}
		local colCount = `colCount' + 1
	}

	preserve

	svmat x
	keep x*
	drop if x1 == .
	gen year = _n + 2003
	rename x1 CNN
	rename x2 MSNBC
	rename x3 FXNC
	twoway (line FXNC year, lwidth(medthick) lcolor(maroon)) ///
			(line CNN year, lwidth(medthick) lcolor(forest_green)) ///
			(line MSNBC year, lwidth(medthick) lcolor(navy)), ///
		xlabel(2004(2)2016) /*name(r`graphCount')*/ ///
		graphregion(color(white)) /*title("``gType'`race'Label'")*/ yscale(range(0 3)) ///
		ylabel(0(0.5)3, format(%03.1f) gmax gmin) /*ytick(0(0.25)3)*/  ///
		xtitle("") ytitle(Percent of TV HHs Watching Network) legend(rows(1) region(col(white)))
 
	graph export ../gph/SuppFig1b_Descriptives_NonWhites_Nielsen.pdf, replace
	restore
}
*
drop rtg*
collapse (sum) imp* (sum) SOW, by(czone year)
foreach channel in CNN MSNBC FXNC  {
	gen rtg`channel' = impressions`channel' / SOW * 100
}


**** line graph overall, november ***************

* collapse down to one observation per czone
local rtgLabel Ratings

matrix x = J(13,4,.)
local colCount = 1
foreach channel in CNN MSNBC FXNC  {
	local rowCount = 1
	forvalues yr = 2004(1)2016  {
		sum rtg`channel' if year == `yr' [aw = SOW]
		matrix x[`rowCount', `colCount'] = `r(mean)'
		local rowCount = `rowCount' + 1
	}
	local colCount = `colCount' + 1
}

preserve

svmat x
keep x*
drop if x1 == .
gen year = _n + 2003
rename x1 CNN
rename x2 MSNBC
rename x3 FXNC
twoway (line FXNC year, lwidth(medthick) lcolor(maroon)) ///
			(line CNN year, lwidth(medthick) lcolor(forest_green)) ///
			(line MSNBC year, lwidth(medthick) lcolor(navy)), ///
	xlabel(2004(2)2016) graphregion(color(white)) /*title("``depVar'Label'")*/ ///
	/*name(overall`depVar')*/ yscale(range(0 3)) ylabel(0(0.5)3, format(%03.1f) gmax gmin) ///
	xtitle("") ytitle(Percent of TV HHs Watching Network)  legend(rows(1) region(col(white)))
    graph export ../gph/Fig1_Descriptives_NielsenRatings.pdf, replace
restore


************ Figure S4: Exposure to Chinese Import Competition and Cable TV News Viewership, November 2004 to November 2008/2012/2016



use ../dta/nielsen_2004_2016.dta, clear

* sample period
gen in_sample=.

mat def x = J(9,7,.)
local row = 1
local x_pos = 1
forvalues yr=2008(4)2016 {
		local ideoCount = 1
		replace in_sample=(t_200411==1 | t_`yr'11==1)
foreach channel of varlist mktshFXNC mktshMSNBC mktshCNN {
				local depvar="`channel'`donorType'"
			* save summary stats about dependent variable
			summ `depvar' [aw=SOW]
			mat x[`row', 6] = r(mean)
			mat x[`row', 7] = r(sd)
			* save regression output
			reghdfe `depvar' t2_group* t2_reg* t2_shnr_pres2000 t2_shnr_pres1996 t2_l_shind_manuf_cbp t2_l_sh_routine33 t2_l_task_outsource group_* t_`yr'11 (t2_d_imp_usch_pd=t2_d_imp_otch_lag_pd) if (in_sample==1) [aw=SOW], absorb(czone) cluster(czone)   
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
	set obs 9
	svmat x
	rename x1 point
	rename x2 se
	rename x3 channel
	rename x4 year
	rename x5 x_pos
	rename x6 depvar_mean
	rename x7 depvar_stdev
	gen se_h = point + se * 1.96
	gen se_l = point - se * 1.96
	twoway ///
		(bar point x_pos if channel == 1, color(navy)) ///
		(bar point x_pos if channel == 2, color(maroon)) ///
		(bar point x_pos if channel == 3, color(forest_green)) ///
		(rcap se_h se_l x_pos, lc(black) /*lstyle(thin)*/), ///
		legend(rows(1) region(col(white)) order(1 "FOX News" 2 "MSNBC" 3 "CNN")) ///
		xlabel(0 " " 1 " " 2 "2004-2008"  3 " " 4 " " 5 " " 6 "2004-2012" 7 "  " 8 "  " 9 "  " 10 "2004-2016", tlength(0)) xtitle("") ///
		graphregion(color(white)) ytitle(100 x change in news TV market share, size(medium))
	graph export ../gph/SuppFig4_Regressions_ByPeriod_Nielsen.pdf, replace
	
	restore
