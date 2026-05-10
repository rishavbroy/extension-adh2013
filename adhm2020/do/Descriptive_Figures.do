
* This file generates Figures 2, 3, and S2 in the paper.

use ../dta/descriptive_figures.dta, clear


* Figure 3

sum mean_w_rep_cfscore if cycle == 1992
gen mean_w_rep_cfscore_ch = mean_w_rep_cfscore - `r(mean)'
sum mean_w_dem_cfscore if cycle == 1992
gen mean_w_dem_cfscore_ch = mean_w_dem_cfscore - `r(mean)'

twoway ///
	(connected mean_w_dem_cfscore_ch cycle, lc(navy) mc(navy) lp(solid)) ///
	(connected mean_w_rep_cfscore_ch cycle, lc(maroon) mc(maroon) lp(solid)), ///
	xlab(1992(2)2016) ylab(-0.3(.1)0.3, format(%03.1f)) tit("") xtit("Election Year") ///
	legend(region(col(white)) order(1 "Democratic mean" 2  "Republican mean")) graphregion(color(white)) ///
	ytit(Change in mean CF score of donors relative to 1992, size(small))
    graph export ../gph/Fig3_Descriptives_LegislatorIdeology.pdf, replace
	

* Figure 2 and S2
keep share_tot_cont_tcile* cycle
drop if cycle < 2002
forvalues t = 1(1)3   {
foreach demo in "_ind" "_corp" "_other"  {
gen share_tot_cont_tcile`demo'`t' = share_tot_cont_tcile`t'`demo'
drop share_tot_cont_tcile`t'`demo'
}
}
reshape long share_tot_cont_tcile share_tot_cont_tcile_ind share_tot_cont_tcile_corp share_tot_cont_tcile_other, i(cycle) j(tercile)
gen x_pos = .
local row = 1
forval x = 1(1)24  {
	replace x_pos = `row' in `x'
	local row = `row' + 1
	if mod(`x',3) == 0  {
		local row = `row' + 1
	}
}
	

foreach vari in share_tot_cont_tcile  {
	
		local ylabel ylab(0(0.1)0.5, format(%03.2f) labsize(small) nogrid) ytick(0(0.05)0.51, grid gmax gmin)
	
	gen point = `vari'
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)), ///
		legend(region(col(white)) label(1 "liberal donors") label(2 "moderate donors") label(3 "conservative donors") rows(3)) ///
		xlabel( 2 "2002" 6 "2004" 10 "2006" 14 "2008" 18 "2010" 22 "2012" 26 "2014" 30 "2016", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(-60(20)60)  ytitle(Share of contributions by donor ideology, size(small)) `ylabel'
	
	graph export ../gph/Fig2_Descriptives_Contributions.pdf, replace 
	drop point
	
}
*	

foreach vari in share_tot_cont_tcile_ind {
	
		local ylabel ylab(0(0.05)0.3, format(%03.2f) gmax gmin)
	
	gen point = `vari'
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)), ///
		legend(region(col(white)) label(1 "liberal donors") label(2 "moderate donors") label(3 "conservative donors") rows(3)) ///
		xlabel( 2 "2002" 6 "2004" 10 "2006" 14 "2008" 18 "2010" 22 "2012" 26 "2014" 30 "2016", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(-60(20)60)  ytitle(Share of contributions by donor ideology, size(small)) `ylabel'
	graph export ../gph/SuppFig2a_Descriptives_ContributionsIndividuals.pdf, replace
	drop point
	
}
	
	
foreach vari in share_tot_cont_tcile_corp {
	
		local ylabel ylab(0(0.05)0.3, format(%03.2f) gmax gmin)
	
	gen point = `vari'
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)), ///
		legend(region(col(white)) label(1 "liberal donors") label(2 "moderate donors") label(3 "conservative donors") rows(3)) ///
		xlabel( 2 "2002" 6 "2004" 10 "2006" 14 "2008" 18 "2010" 22 "2012" 26 "2014" 30 "2016", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(-60(20)60)  ytitle(Share of contributions by donor ideology, size(small)) `ylabel'
	graph export ../gph/SuppFig2b_Descriptives_ContributionsCorporations.pdf, replace
	drop point
	
}
	
foreach vari in share_tot_cont_tcile_other {
	
		local ylabel ylab(0(0.05)0.3, format(%03.2f) gmax gmin)
	
	gen point = `vari'
	twoway ///
		(bar point x_pos if tercile == 1, color(navy)) ///
		(bar point x_pos if tercile == 2, color(forest_green)) ///
		(bar point x_pos if tercile == 3, color(maroon)), ///
		legend(region(col(white)) label(1 "liberal donors") label(2 "moderate donors") label(3 "conservative donors") rows(3)) ///
		xlabel( 2 "2002" 6 "2004" 10 "2006" 14 "2008" 18 "2010" 22 "2012" 26 "2014" 30 "2016", noticks) xtitle("")	  ///
		graphregion(color(white)) ylabel(-60(20)60)  ytitle(Share of contributions by donor ideology, size(small)) `ylabel'
	graph export ../gph/SuppFig2c_Descriptives_ContributionsOther.pdf, replace
	drop point
	
}
		
