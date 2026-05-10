# Bartik / shift-share identification memo

This project estimates reduced-form event-study relationships between CZ-level ADH China import exposure and political outcomes. The ADH exposure variable is a shift-share object: local baseline industry composition is combined with national or foreign import-growth shocks.

## Interpretation

For the OLS event-study coefficients to be interpreted causally, high-exposure CZs must not have been on different political trajectories for reasons unrelated to the China shock after conditioning on fixed effects and baseline-control-by-year interactions. For the ADH-style instrument, one must additionally defend either exogeneity of the foreign import-growth shifts or exogeneity of the initial industry shares, depending on the preferred shift-share identification argument.

## Diagnostics written by the pipeline

- `bartik_first_stage_diagnostics.csv`: first-stage strength of the ADH other-high-income-country instrument for ADH exposure.
- `bartik_balance_correlations.csv`: correlations of exposure and instrument with baseline controls.
- `bartik_pretrend_placebos.csv`: whether exposure or the instrument predict pre-1988 Republican-margin trends.
- `bartik_rotemberg_data_availability.csv`: explains why exact GPSS Rotemberg weights are not computed from the currently attached public ADH workfiles.

## How to use in the paper

Treat these diagnostics as evidence about the plausibility and limits of the identifying assumptions, not as proof. If a small number of industries dominate a future reconstructed Rotemberg-weight diagnostic, the political estimates should be interpreted as exposure to those high-weight industries rather than a generic China-shock effect.
