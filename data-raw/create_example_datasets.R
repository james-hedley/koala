# Project: Create example input datasets for run_koala function
# Created by: James Hedley
# Date created: 9th March 2026


# Load libraries ----
library(tidyverse)
library(stringr)
library(truncnorm)
library(scales)
library(usethis)


# Set random number seed
set.seed(3216)


# Simulate 1 donor
donor_example <- tibble(donor_id = paste0("D", str_pad(1, width = 4, pad = "0"))) %>%
  mutate(
    donor_state = sample(n(), x = c("NSW", "VIC", "QLD", "SA", "WA"), prob = c(0.3, 0.28, 0.2, 0.14, 0.08), replace = TRUE),
    donor_bloodgroup = sample(n(), x = c("O", "A", "B", "AB"), prob = c(0.45, 0.37, 0.14, 0.04), replace = TRUE),
    donor_age = rnorm(n(), mean = 50, sd = 5),
    donor_hypertension = (runif(n()) <= 0.1) * 1,
    donor_diabetes = (runif(n()) <= 0.1) * 1,
    donor_creatinine = rlnorm(n(), meanlog = 100, sd = 10),
    donor_strokedeath = (runif(n()) <= 0.1) * 1,
    donor_height = rtruncnorm(n(), mean = 170, sd = 10),
    donor_weight = rtruncnorm(n(), mean = 70, sd = 5),
    donor_dcd = (runif(n()) <= 0.3) * 1,
    donor_kdpi = floor(runif(n(), min = 0, max = 100)) + 1,
    donor_kidneys = if_else(runif(n()) <= 0.8, 2, 1),
    donor_pancreas = if_else(runif(n()) <= 0.2, 1, 0),
    donor_living = 0,
  )


# Simulate current waiting list
waitlist_example <- tibble(patient_id = paste0("P", str_pad(1:1354, width = 4, pad = "0"))) %>%
  mutate(
    patient_waityears = rtruncnorm(n(), mean = 2.5, sd = 1, a = 0, b = 10),
    patient_state = sample(n(), x = c("NSW", "VIC", "QLD", "SA", "WA"), prob = c(0.3, 0.28, 0.2, 0.14, 0.08), replace = TRUE),
    patient_age = rtruncnorm(n(), mean = 55, sd = 10, a = 0, b = 90),
    patient_bloodgroup = sample(n(), x = c("O", "A", "B", "AB"), prob = c(0.45, 0.37, 0.14, 0.04), replace = TRUE),
    patient_pra = if_else(runif(n()) <= 0.6, 0, runif(n(), min = 1, max = 100)),
    patient_epts = floor(runif(n(), min = 0, max = 100)) + 1,
    patient_hla_mismatch_mean = rtruncnorm(n(), mean = 12, sd = 1, a = 0, b = 17),
    patient_hla_mismatch_sd = sqrt(rlnorm(n(), meanlog = log(1), sd = 0.05)),
    patient_national_urgent = if_else(runif(n()) <= 0.0001, 1, 0),
    patient_state_urgent = if_else(runif(n()) <= 0.0001, 1, 0),
    patient_prior_donor = if_else(runif(n()) <= 0.0001, 1, 0),
    patient_kidney_after_other_organ = if_else(runif(n()) <= 0.001, 1, 0),
    patient_spk = if_else(runif(n()) <= 0.05, 1, 0)
  )


# Simulate donor-patient cross-match (HLA mismatch score and unacceptable antigens)
crossmatch_example <- waitlist_example %>%
  select(patient_id, patient_hla_mismatch_mean, patient_hla_mismatch_sd, patient_pra) %>%
  cross_join(donor_example %>% select(donor_id)) %>%
  mutate(hla_mismatch = rnorm(n(), mean = patient_hla_mismatch_mean, sd = patient_hla_mismatch_sd),
         rescaled_pra = rescale(patient_pra, from = c(0, 100), to = c(0.1, 99.9)), # Ensure even PRA 0 can have unacceptable antigens, or PRA 100 can have no unacceptable antigens
         unacceptable_antigens = if_else(runif(n()) <= rescaled_pra/100, 1, 0)) %>%
  select(patient_id, donor_id, hla_mismatch, unacceptable_antigens)



# Simulate current state debts
state_debts_example <- tibble(from_state = rep(c("NSW", "VIC", "QLD", "SA", "WA"), times = 5),
                      to_state =  rep(c("NSW", "VIC", "QLD", "SA", "WA"), each = 5)) %>%
  mutate(
    net_debt = floor(runif(n(), min = -5, max = 6))
  ) %>%
  # Ensure debts are symmetrical between states
  mutate(state_pair = paste0(pmin(from_state, to_state), pmax(from_state, to_state))) %>%
  group_by(state_pair) %>%
  mutate(net_debt = case_when(
    from_state == to_state ~ 0,
    row_number() == 1 ~ net_debt,
    row_number() == 2 ~ -lag(net_debt))) %>%
  ungroup() %>%
  select(-state_pair)



# Save input datasets
use_data(waitlist_example, donor_example, crossmatch_example, state_debts_example, overwrite = TRUE)






