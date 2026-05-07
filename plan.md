# Goal
Run the following panel event study:
$$Y_{ct} = \alpha_c + \lambda_t + \sum_k \beta_k (\text{Exposure}_c \times 1[t=k]) + (X_c \times \lambda_t)'\,\Gamma + \varepsilon_{ct}$$
where, given a commuting zone (CZ) $c$ and year $t$, we have:

- $Y_{ct}$, Republican margin in presidential election year $t$
- $\alpha_c$, CZ fixed effects
- $\lambda_t$, election-year fixed effects
- $\text{Exposure}_c$, the ADH measure of 1990-2007 exposure
- $X_c$, a column vector of CZ-level baseline controls
	- 1990 manufacturing share, college share, and foreign-born share

We focus on running the event study so we can study separate horizons:

- Short-run effects (say, 2008-2012) are likely influenced by the recession and Obama-era response 
- Medium-run effects (say, ~2016) are likely influenced by Trump-specific rhetoric
- Long-run effects (e.g., 2020+) are likely influenced by COVID, inflation, etc.

# Process
## First:
We begin first with the replication package of ADH 2013 (saved under `replication/adh2013`), and second by building a map from current-day counties (or at least 2024 counties; there have been no substantial changes to counties since 2022, anyways) to 2023 counties to 2022 counties etc., all the way to 1990 counties and then to 1990 CZs. The process is like so:

1. Map 2020 counties to 1990 counties using [Ferrara, Testa, and Zhou (2024)](https://doi.org/10.1080/01615440.2024.2369230).
	1. **Weighting rules**:
		1. Area-based (model 1, or M1). 
		2. Population-based (M2), with county area divided into urban and rural areas. **(Fallback)**
		3. Population-based (M3), with county area divided into urban and rural areas after excluding non-inhabitable areas.
		4. Population-based (M4), with county area divided into urban and rural areas after excluding non-inhabitable areas, with additional weighting for topographic suitability (i.e., elevation).
		5. **Population-based (M5), with built-up settlement areas indicated in space (1810–2020 only)**.
		6. Population-based (M6), with built-up property counts indicated in space (1810–2020 only).

2. Aggregate 1990 counties to 1990 CZs using `replication/cz-data`, sourced from [https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas](https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas)
	1. Nothing complicated here. `cz-198090.xls` should match county names and FIPS codes to 1990 CZs. As far as I could tell, the county names and codes are from 1990 and the CZ codes match [Tolbert and Sizer (1996)](https://ageconsearch.umn.edu/record/278812?v=pdf), the paper ADH 2013 used for their CZs.

3. Match with the data from the replication package, `replication/adh2013`
	1. Everything's smooth sailing now; just join along the 1990 CZ code. Also don't forget that out of 741 CZs, ADH only use "the 722 CZs that cover the entire mainland United States (both metropolitan and rural areas)" (p. 2132).

4. Check and address errors, e.g.:
	1. Inconsistencies in FIPS codes
	2. Missing counties or duplicated mappings
	3. Population weights which don't sum to 1


## Second, use this to match `adh2013` data with variables for the political outcome $Y_{ct}$

- Data sources of $Y_{ct}$
	- Use `replication/outcomes-data/aa2021-nospatial`, sourced from [Replication Data for: Partisanship & Nationalization in American Elections: Evidence from Presidential, Senatorial, & Gubernatorial Elections in the U.S. Counties, 1872-2020](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DGUMFI), the replication package of [Amlani and Algara (2021)](https://doi.org/10.1016/j.electstud.2021.102387), to construct 1950-2020 political outcomes in presidential, senatorial, and gubernatorial elections. We will use this for pre-trends before 1990, trends during the 1990-2007 shock, and post-shock outcomes. This will be our primary source of outcomes.
- For now, define $Y_{ct}$ as the Republican presidential vote margin


## Third, prepare Conley spatial HAC SEs.

- Get county centroids from `replication/ftz2024/crosswalks/County-CD-centroid-lat-lon/lat_lon_coordinates_county_csv/counties_1990_xy.csv`. 
- Map counties to 1990 CZs
- Compute population-weighted CZ centroids using 1990 county populations in `replication/cz-data/cz-198090.xls`, sourced from [https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas](https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas)
	- $\text{CZ centroid} = \sum_{i\in c}w_i\cdot(\text{lat}_i,\text{lon}_i),\quad w_i=\frac{\text{pop}_{i,1990}}{\sum_{j\in c}\text{pop}_{j,1990}}$
- Calculate [Conley (1999)](https://doi.org/10.1016/S0304-4076(98)00084-0) spatial HAC SEs. For reference, recall:

Homoskedastic:
$$SE(\hat{\beta}) = \sqrt{\frac{\hat{\sigma}^2}{\sum (x_i - \bar{x})^2}}$$

Heteroskedastic (HC0):
$$SE(\hat{\beta}) = \sqrt{\frac{\sum (x_i - \bar{x})^2 \hat{\varepsilon}_i^2}{[\sum (x_i - \bar{x})^2]^2}}$$

Cluster-robust:
$$SE(\hat{\beta}) = \sqrt{\frac{\sum_g [\sum_{i \in g} (x_i - \bar{x}) \hat{\varepsilon}_i]^2}{[\sum (x_i - \bar{x})^2]^2}}$$

Conley:
$$SE(\hat{\beta}) = \sqrt{\frac{\sum_i \sum_j K(d_{ij}) (x_i - \bar{x})(x_j - \bar{x}) \hat{\varepsilon}_i \hat{\varepsilon}_j}{[\sum (x_i - \bar{x})^2]^2}}$$

## Fourth, run the event study
- Note that potential controls $X_{ct}$ from 1990 can be found in the ADH 2013 replication package.


### Fifth, fix all of the following errors
#### Coding and statistical errors
- If I recall correctly, extending out to 1952 induces near-singularity. That is why the starting point had to be pushed back to 1972
	- Diagnostics for rank, condition number, dropped variables, and collinearity (which I also mandate below) should be ran on both 1952-start and 1972-start panels.
- Why such absurdly low vote-retention rates? 0.60 in 1972-1988 and 2000-2008, and about 0.157 in 2012-2020
	- `retained_vote_share` is computed after the many-to-many crosswalk join, so the denominator double- or multi-counts source-county votes when a county has multiple crosswalk rows?
	- Repair matching algorithm
- Conley VCOV (variance-covariance matrix) was not positive definite, had to be repaired (i.e., numerically regularized, I suppose?)
	- `vcov_fix = TRUE` is also too casual here. The fixest documentation says non-positive-definite repair may signal a problem with the asymptotic approximation, and the code does not save eigenvalues, repair flags, the minimum eigenvalue, or severity of the correction. ([Lrberge](https://lrberge.github.io/fixest/reference/vcov_conley.html))
	- Inspect role of interacted controls in near-singularity
- SEs are spatial-only Conley, not spatial-temporal HAC
	- It does not clearly implement a temporal HAC kernel; residuals in year 1 may clearly affect residuals in year 2. 
	- `fixest::vcov_conley()` computes a VCOV robust to spatial correlation within a distance cutoff with a uniform kernel; it is not, by itself, a serial-correlation HAC estimator. fixest has separate NW/DK tools (see [HAC VCOVs](https://lrberge.github.io/fixest/reference/vcov_hac.html)), but this code does not combine spatial and temporal dependence.
	- For now, implement alternative SEs
		- Compare Conley SEs with heteroskedastic, CZ-clustered, state-clustered, Newey-West / Driscoll-Kraay, and perhaps even alternative spatial SEs.
- Model rank, collinearity, dropped variables, and condition numbers are not saved.
	- Save collinearity diagnostics for each specification.
	- Record dropped variables from `fixest`.
	- You may find it to create a `output/diagnostics/pipeline_manifest.json` with config, sources, checksums, pass/fail status, etc. 
	- Create another manifest listing included/omitted controls per spec
- The panel is not balanced
	- The expected size is 722 CZs × 13 years = 9,386; the status file reports 9,093 observations, and missing_cz_year_outcomes.csv has 293 missing CZ-years.
	- Add an option `CONFIG$require_balanced_panel = TRUE` such that, regardless of if it is `TRUE` or `FALSE`, any imbalance is reported
- M5 weights are renormalized within source county when positive weights exist
	- Is this defensible?
- M5 undefined weights must not be silently converted to zero.
	- The code converts undefined M5 weights into zero raw-weight sums through `sum(raw_weight, na.rm = TRUE)`. 
		- Preserve undefined/missing weights and report them.
	- For source counties with undefined M5/M6 denominators, implement a clearly documented fallback policy. Add a config option: `crosswalk_missing_weight_policy = c("fail", "fallback_m4", "fallback_m2", "identity_if_same_fips")`. Default should be "fail" until we choose a policy.
	- Add diagnostics by decade/source county: raw weight sum, normalized weight sum, number of targets, missing/undefined status, fallback used.
- M5-specificity is hardcoded.
	- `CONFIG$crosswalk_weight` is unused. The code looks configurable but is not.
	- Diagnostics and output names assume M5.
	- Have to actually use `CONFIG$crosswalk_weight = "m5_weight"` instead of just having `read_ftz_crosswalk()` hardcode `m5_weight`
		- Replace hardcoded `m5_weight `in `read_ftz_crosswalk()` with `CONFIG$crosswalk_weight`.
		- Support at least `m5_weight` and `m6_weight`.
		- Rename generic output columns to `raw_weight`, `weight_sum_raw`, and `crosswalk_weight` rather than `weight_m5`.
		- Make diagnostics/output filenames include the selected weight.
- Original two-party vote totals jump to about 793 million in 2012, 800 million in 2016, and 974 million in 2020, while bridged totals remain around plausible presidential-election magnitudes
	- Suggests duplicated rows, wrong filtering, or a source-data interpretation problem before crosswalking
	- Code appears not to deduplicate or validate county-year rows before summing votes
	- Real error:
	- ```R
		county_pres %>%
		  left_join(crosswalks, relationship = "many-to-many") %>%
		  group_by(year) %>%
		  summarise(original_two_party_votes = sum(twoparty_votes))
		```
	- After the many-to-many join, source-county votes are repeated once for each target county-part. So the “original” denominator is inflated.
	- The retained share should instead be computed using a pre-join source county-year table as the denominator.
- The target controlled specification is only partially implemented.
	- The config lists six baseline controls, but only three are interacted.
	- Why could this be? Is there a reason the other three couldn't have been interacted?
	- The interacted-control model uses these three controls: manufacturing share, college share, and foreign-born share
- `02_build_analysis_data.R`, `03_estimate_event_study.R`, and `04_export_outputs.R` execute immediately when sourced. 
	- That is okay for a quick script, but less maintainable than functions with explicit inputs/outputs.
	- Convert `02_build_analysis_data.R`, `03_estimate_event_study.R`, and `04_export_outputs.R` into function-based scripts that do not execute immediately when sourced.
	- Keep `run_pipeline.R` as the only script with side effects.
- The county-year outcome data are not sufficiently validated before use. 
	- There should be explicit checks for one row per county-year, nonduplicated FIPS, vote totals, and weight sums. For example:
		- count(year, county_fips) == 1
		- twoparty_votes == rep_votes + dem_votes
		- nonnegative votes
		- complete FIPS coverage
		- state/year vote totals
		- national vote totals before bridging
	- If the duplicates are exact duplicates, explicitly `distinct()` them. If they represent something substantive, aggregate correctly only after understanding the source structure.
- Many-to-many joins are allowed without sufficient post-join validation.
- The pipeline records diagnostics but does not fail when diagnostics are unacceptable. 
- Add a pipeline-level validation object. The pipeline should stop before estimation if fatal diagnostics fail.
	- Crosswalk validation gates
		- One and only one source county-year row before crosswalk.
		- Weight sums by source county must equal 1 within tolerance, or else the county must be explicitly marked as undefined and handled by a documented fallback. Track the number of such counties.
		- Report missing/undefined weights by state, year, county, vote share, and CZ.
		- Estimation should stop if bridged vote retention falls below a chosen threshold after using the correct pre-join denominator.
	- Outcome validation gates
		- Check `rep_votes + dem_votes == twoparty_votes` within tolerance.
		- Check national and state two-party totals by year before and after bridging.
		- Check duplicates by `year × county_fips`.
		- Save a county-year duplicate report.
	- Concrete defaults should include the following:
		- min_retained_vote_share = 0.98
		- require_no_duplicate_county_years = TRUE
		- require_no_vcov_repair_for_main = TRUE


#### Table issues:
- The LaTeX table is manually assembled with strings. This is brittle.
	- The table does not need to be shaded. Use the standard methods (namely `threeparttable`, `kableExtra`, `booktabs`, and perhaps `longtable`) to construct a non-shaded table.
- `latex_escape()` is defined but not actually used for table content.
	- Delete if not needed
- The CSV table contains LaTeX `\makecell` strings, so it is not a clean machine-readable output.
- `render_table_pdf()` hardcodes `tab_event_study_rep_margin_two_specs.tex` instead of using the passed input filename.
- The table does not report the units of ADH exposure.
#### Figure issues:
- The line connects across the omitted 1988 reference year, but 1988 is not plotted as a zero coefficient. That can visually mislead.
	- Plot two separate trends, a pre- and a post-trend.
- Facet order is odd: “Interacted controls” appears above “Minimal.”
- The shaded region begins in 1990 and extends to 2020, but the substantive interpretation distinguishes shock years, short-run, medium-run, and long-run horizons. The shading should be more explicit.
	- Draw either shaded regions with labels in a legend or vertical, colored lines with adjacent labels indicating the ADH exposure period, the Great Recession/Obama period, Trump-era politics, and COVID-era politics
- The $y$-axis does not state the exposure unit.
- The figure should not be presented before the crosswalk/vote-retention issues are fixed.


## Sixth, implement future plans:

### Pre-trend
- Inspect pre-trends of the outcomes variables in `replication/outcomes-data/aa2021-nospatial`

### Robustness checks
- Different SEs
	- Different Conley SEs
		- Test out lower Conley cutoffs. Test for bandwidth sensitivity
		- Perhaps use the `SpatialInference` R package for the bandwidth selection method described in [Lehner (2026)](https://arxiv.org/html/2603.03997v1).
- Incorporate serial dependence across election years.
	- May need to use [Kelejian and Prucha (2006)](https://doi.org/10.1016/j.jeconom.2006.09.005)
- Run specifications with/without instrument and run weak instrument tests (medium-priority)
	- $\text{Exposure}_c$, the ADH measure of exposure, is an endogenous regressor!! 
		- ADH’s Bartik instrument for their exposure measure is Chinese import growth in other high-income countries. Will likely be far weaker for our longer-run political outcomes than their shorter-run labor outcomes.
	- How to test?
		- One instrument --> first-stage $F$-stat is natural, test if industry shares predict exposure
		- But it's a Bartik instrument! [Goldsmith-Pinkham, Sorkin, and Swift (2020)](https://doi.org/10.1257/aer.20181047), a single industry share likely correlated with the error term may drive this. Might have to do some Rotemberg weight funkiness. [Borusyak, Hull, and Jaravel (2025)](https://doi.org/10.1257/jep.20231370) could be a helpful guide here too.
- Test for Bartik instrument flaws (low)
	- Presence of flaws can be tested by replicating Sec. A.5 of the [Online Appendix](https://www-aeaweb-org.ezproxy.library.wisc.edu/content/file?id=12670) for [Goldsmith-Pinkham, Sorkin, and Swift (2020)](https://doi.org/10.1257/aer.20181047).
	- Additional sensitivity can be tested using [Apfel (2024)](https://doi.org/10.1093/jrsssa/qnad148)
- Test for sensitivity to commuting zone definition
	- Commuting zone definitions are sensitive, making empirical results sensitive
	- Replicate [Foote, Kutzbach, and Vilhuber (2021)](https://doi.org/10.1080/00036846.2020.1841083) using https://github.com/larsvilhuber/MobZ i.e., https://larsvilhuber.github.io/MobZ/ or https://zenodo.org/records/4072428
- Test for crosswalk flaws
	- Compare the sensitivity of results to use of different weights (e.g., M5 vs. M6) in step 1 of crosswalk.
	- Any way to check attenuation from crosswalk? 
	- Check robustness of results to other crosswalks e.g., from https://www.ddorn.net/data.htm
	- Turn county crosswalk from decadal to annual
		- It is not possible to build a genuine year-by-year chain from current counties to 2023, 2022, etc. using `replication/ftz2024`Instead, we have had to use a coarse election-year-to-decade rule:
			- year < 1980 -> 1970
			- year < 1990 -> 1980
			- year < 2000 -> 1990
			- year < 2010 -> 2000
			- year < 2020 -> 2010
			- else -> 2020
- Address the flaws of ADH 2013 data
	- Ex: Sectoral/product aggregation bias (see [Wang et al., 2018](https://www.nber.org/system/files/working_papers/w24886/w24886.pdf)), temporal aggregation bias (see [Rothwell, 2017](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2920188)), inconsistent industry coding and missing firm reorganization (see [Bloom et al., 2024](https://www.nber.org/system/files/working_papers/w33098/w33098.pdf)).
	- Reconstruct exposure measure
		- E.g., Replace static exposure with time-varying exposure. 
			- Single static 1990-2007 exposure with election-year dummies could work better if we instead used time-varying exposure by subperiod, namely 1991-1999 and 2000-2007
	- Relevant data from https://www.ddorn.net/data.htm to reconstruct a better exposure
		- All files from [C] Industry Codes and [D] Industry Trade Exposure


### More outcomes and mechanisms
- Run specifications where  $Y_{ct}$, a CZ-level political outcome in year $t$ is either Republican vote share, Democratic vote share, two-party share, Republican margin, and swings across elections.
- Robustness to different data source for electoral outcomes
	- Use `replication/outcomes-data/pres_elections-2000-2024`, sourced from [County Presidential Election Returns 2000-2024](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FVOQCHQ), for county-level political outcomes in presidential elections from 2000-2024.
		- Our "First" step in the Process would then also have to map 2024 counties to 2020 counties using [Lutz (2025)](https://chandlerlutz.github.io/files/bridging_the_geographic_divide_crosswalks_across_space_and_time.pdf).
			- The only [substantial changes to counties](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html) made this period were in Connecticut in 2022. [Lutz (2025)](https://chandlerlutz.github.io/files/bridging_the_geographic_divide_crosswalks_across_space_and_time.pdf) provides a county-level crosswalk of this. Directly replicating the code in the README of `replication/lutz2025/geolinkr-main`, sourced from [https://github.com/ChandlerLutz/geolinkr](https://github.com/ChandlerLutz/geolinkr), is sufficient for this. Note how his code reads in external data via URLs. 
			- **Weighting rule**: The README says he uses "the number of housing units in each census tract as weights."
- Build outcome variable for election turnout
	- [National Neighborhood Data Archive (NaNDA): Voter Registration, Turnout, and Partisanship by County, United States, 2004-2022 (ICPSR 38506)](https://www.icpsr.umich.edu/web/ICPSR/studies/38506) is very promising
		- Relevant county-level variables: Registered voters, ballots cast, voting population (specifically CVAP), voter registration, voter turnout, registered voter turnout 
- Build outcomes that provide a better picture of the full political effect
	- Motivation: [Autor et al. (2020)](https://doi.org/10.1257/aer.20170011) find the China shock’s main political effects to be a rightward shift in media consumption and electoral outcomes + a loss of moderates. 
	- Partisanship
		- [National Neighborhood Data Archive (NaNDA): Voter Registration, Turnout, and Partisanship by County, United States, 2004-2022 (ICPSR 38506)](https://www.icpsr.umich.edu/web/ICPSR/studies/38506) is very promising
			- Relevant county-level variables: Democratic and Republican partisanship indices
	- Polarization
		- [American National Election Studies (ANES)](https://electionstudies.org/data-center/) data measures polarization and other political attitudes; read the questions it asks [here](https://electionstudies.org/data-tools/anes-continuity-guide/#iii-c-economic-issues). Sample sizes per CZ are probably tiny, however. Don't prioritize it. 
	- Data on media consumption?
	- Relevant political outcomes from https://www.ddorn.net/data.htm:
		- [[H1]](https://www.ddorn.net/data/cfavg_2002_2016.zip) Liberal, moderate and conservative legislators in U.S. Congress, 2002-2016.
		- [[H2]](https://www.ddorn.net/data/teaparty_2010_2016.zip) Members of the Tea Party, Liberty and Freedom Caucuses, 2010-2016
- Test mechanism, which should be very hard to genuinely determine. Could be any one of:
	- Displaced manufacturing workers becoming more protectionist, conservative, Republican, etc.
	- Migration
		- Could use [SOI tax data](https://www.irs.gov/statistics/soi-tax-stats-data-by-geographic-area) to allow migration as a mediator
	- CZ composition changes
		- Could find mediators from the Census Bureau’s [PEP](https://www.census.gov/programs-surveys/popest.html) or [ACS](https://www.census.gov/programs-surveys/acs/data.html), the NCI’s [SEER](https://seer.cancer.gov/popdata/), IPUMS [NHGIS time series](https://www.nhgis.org/data-availability), or [other public data sources](https://libguides.brown.edu/census/histmicro)
		- Section [E] Local Labor Market Geography of https://www.ddorn.net/data.htm could be useful for merging these
	- Workers persuading each other
	- Elite rhetoric
		- Could try and reconstruct measures from [Autor et al. (2020)](https://doi.org/10.1257/aer.20170011) using publicly-available data
	- Changes in turnout
	- Better party sorting of voters
	- Relevant data from https://www.ddorn.net/data.htm, e.g., for baseline manufacturing mix or tradable exposure
		- [[F1]](https://www.ddorn.net/data/cbp_czone_merged.zip) CBP county-level employment by industry, 1988, 1991, 1999, 2007 and 2011.
	- Did a political change occur for majority-white + trade-exposed CZs vs. majority-white + not trade-exposed CZs? Or did it only occur for majority-white CZs vs. majority-minority CZs?
	- Does media consumption (e.g., Fox News viewership) predict political change?

### Pretty stuff for the paper
- Make maps
	- 1990 census tract shapefiles in `replication/spatial-data/Census_Tracts_in_1990`, sourced from https://catalog.data.gov/dataset/census-tracts-in-1990. 
	- 1990 county shapefiles at `replication/spatial-data/counties-1990`, sourced from https://geo.btaa.org/catalog/stanford-pb817xw6983

### Additional literature review
- https://en.wikipedia.org/wiki/China_shock
- [Partisan Popularity & Protectionism: The Political Economy Impact of the China Shock on Local Labor Markets](https://dataspace.princeton.edu/handle/88435/dsp012z10wt48b)