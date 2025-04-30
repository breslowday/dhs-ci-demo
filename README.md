# README: dhs-ci-demo

Demo of comparison between state-level estimates for regions 1 through 4 and state-level estimates for regions 1, 2, and a super-region of 3+4

This repo uses the sample children's recode file `zzkr62dt.zip` from [DHS Model Datasets](https://www.dhsprogram.com/data/Download-Model-Datasets.cfm) in Stata format.

The goal of this repository is to create estimates of the following variable:

**Measles vaccine received (`h9`), at least 1 dose, in children aged 12-23 months, from either source (recall or card).**

1.  Create estimates for all 4 states, reported as 4 separate measures, one for each state, saved in `outputs/ZZ6_estimates_indiv_date.csv`
2.  Create estimates for all 4 states, reported as State 1, State 2, and "State 3 + 4", saved in `outputs/ZZ6_estimates_combined_date.csv`

To view already-made estimates, look in the `outputs` folder. The file titled `…indiv…` has one estimate for **each** ADM1 region. The file titled `…combined…` has one estimate each for regions 1 ann 2, and a **combined** estimate for regions 3 and 4.

To re-calculate estimates, run `run_this.R`
