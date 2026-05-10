# Bartik / shift-share identification memo

This project estimates reduced-form event-study relationships between CZ-level ADH China import exposure and political outcomes. The ADH exposure variable is a shift-share object: local baseline industry composition is combined with national or foreign import-growth shocks.

## Required assumptions

A causal reading requires high-exposure CZs not to have been on different political trajectories for reasons unrelated to the China shock after conditioning on fixed effects and baseline-control-by-year interactions. For the ADH-style instrument, one can emphasize exogeneity of foreign import-growth shifts or exogeneity of baseline industry shares; both routes require substantive defense.

## Project-specific threats

- Initial industry shares may be correlated with prior political realignment, union decline, automation, racial composition, religiosity, education, or local media markets.
- If only a few high-shock industries drive exposure, the estimates may capture politics of those industries rather than a generic China-shock effect.
- Migration and turnout can make CZ-level political outcomes change even if individual beliefs do not change.
- Candidate supply and elite rhetoric may translate material shocks into political outcomes; the ADHM 2020 mechanism outputs are diagnostic for this channel.

## Diagnostics written by the pipeline

- `bartik_first_stage_diagnostics.csv` and `fig_bartik_first_stage.png`: first-stage strength of the ADH other-country instrument.
- `bartik_balance_correlations.csv`: exposure/instrument correlations with baseline controls.
- `bartik_pretrend_placebos.csv`: whether exposure/instrument predict pre-1988 Republican-margin trends.
- `bartik_preperiod_workfile_diagnostics.csv`: diagnostics using ADH's preperiod workfile.
- `bartik_industry_shift_summary.csv`: industry-level import-shift summaries from ADH public trade data.
- `bartik_rotemberg_data_availability.csv`: exact Rotemberg weights are a feasible future reconstruction, not a completed diagnostic here.

