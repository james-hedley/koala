
# koala

<!-- badges: start -->
<!-- badges: end -->

**KOALA** (Kidney Only ALlocation Algorithm) is an R package with useful functions applicable to the Australian deceased donor kidney allocation algorithm

## Installation

You can install koala like this:

``` r
# install.packages("remotes") # if not already installed
remotes::install_github("james-hedley/koala")
```

## Example
`run_koala()` requires four input datasets:
1) `waitlist` – one row per patient. Must include columns:
  patient_id, patient_waityears, patient_state, patient_age, patient_bloodgroup, patient_pra, 
  patient_epts, patient_hla_mismatch_mean, patient_hla_mismatch_sd, patient_national_urgent, 
  patient_state_urgent, patient_prior_donor, patient_kidney_after_other_organ, patient_spk
2) `donors` – one row per donor. Must include columns: 
 donor_id, donor_state, donor_bloodgroup, donor_age, donor_hypertension, donor_diabetes, 
 donor_creatinine, donor_strokedeath, donor_height, donor_weight, donor_dcd, donor_kdpi, 
 donor_kidneys, donor_pancreas, donor_living
3) `crossmatch` – one row per patient-donor pair. Must include columns: 
  patient_id, donor_id, hla_mismatch, unacceptable_antigens 
4) `state_debts` – 25 rows (all pairs of transplant jurisdictions). Must include columns: 
  from_state, to_state, net_debt

For full details on the required inputs, see the function documentation: `?run_koala`

To rank patients on the waitling list donor, run:
``` r
data(waitlist_example)
data(donor_example)
data(crossmatch_example)
data(state_debts_example)

ranked_list <- run_koala(
  waitlist = waitlist_example,
  donors = donor_example,
  crossmatch = crossmatch_example,
  state_debts = state_debts_example
  )
  
ranked_list
```
This creates a dataset with one row per waitlisted patient, per donor. For each donor, the patients are ranked according to KOALA.

