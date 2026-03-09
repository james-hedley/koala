#' @importFrom dplyr %>% arrange case_when if_else left_join mutate row_number select everything group_by ungroup join_by desc n
#' @importFrom tidyr crossing
#' @importFrom stats runif
#'
#' @name run_koala
#' @title Australian Kidney Only ALlocation Algorithm (KOALA)
#'
#' @description Function to apply the Australian deceased donor kidney allocation algorithm. run_koala() requires four input datasets.
#' @param waitlist A dataframe/tibble with one row per patient, and columns for each patient characteristic:
#'   \describe{
#'     \item{patient_id}{Unique identifier for each waitlisted patient}
#'     \item{patient_waityears}{Total time on the waiting list in years (as a decimal, not rounded)}
#'     \item{patient_state}{Australian state where the patient is waitlisted. Options are: NSW, VIC, QLD, SA, WA}
#'     \item{patient_age}{Patient age in years}
#'     \item{patient_bloodgroup}{ABO blood group of the patient}
#'     \item{patient_pra}{Panel reactive antibody (PRA). Decimal 0-100}
#'     \item{patient_epts}{Estimated Post-Transplant Survival (EPTS) score. Integer 1-100}
#'     \item{patient_hla_mismatch_mean}{Mean HLA mismatch score}
#'     \item{patient_hla_mismatch_sd}{Standard deviation of HLA mismatch score}
#'     \item{patient_national_urgent}{Indicator for nationally urgent status. 0 or 1}
#'     \item{patient_state_urgent}{Indicator for state urgent status. 0 or 1}
#'     \item{patient_prior_donor}{Indicator for prior living donor status. 0 or 1}
#'     \item{patient_kidney_after_other_organ}{Indicator for kidney-after-other-organ transplant. 0 or 1}
#'     \item{patient_spk}{Indicator for simultaneous pancreas–kidney transplant. 0 or 1}
#'   }
#'
#' @param donors A dataframe/tibble with one row per donor, and columns for each donor characteristic:
#'   \describe{
#'     \item{donor_id}{Unique identifier for each donor}
#'     \item{donor_state}{Australian state where the donor was recovered. Options are: NSW, VIC, QLD, SA, WA}
#'     \item{donor_bloodgroup}{ABO blood group of the donor}
#'     \item{donor_age}{Donor age in years}
#'     \item{donor_hypertension}{Donor history of hypertension. 0 or 1}
#'     \item{donor_diabetes}{Donor history of diabetes. 0 or 1}
#'     \item{donor_creatinine}{Terminal serum creatinine level}
#'     \item{donor_strokedeath}{Death due to stroke. 0 or 1}
#'     \item{donor_height}{Donor height in cm}
#'     \item{donor_weight}{Donor weight in kg}
#'     \item{donor_dcd}{Donation after circulatory death (DCD). 0 or 1}
#'     \item{donor_kdpi}{Kidney Donor Profile Index (KDPI). Integer 1-100}
#'     \item{donor_kidneys}{Number of kidneys available for allocation. 1 or 2}
#'     \item{donor_pancreas}{Binary indicator for pancreas availability. 0 or 1}
#'     \item{donor_living}{Living donor (altruistic or end of kidney exchange chain). 0 or 1}
#'   }
#'
#' @param crossmatch A dataframe/tibble with one row per patient-donor pair, and columns for:
#'   \describe{
#'     \item{patient_id}{Unique identifier for each waitlisted patient}
#'     \item{donor_id}{Unique identifier for each donor}
#'     \item{hla_mismatch}{HLA mismatch score (ABDRDQ score) for this patient-donor pair. Calculated as: sqrt(1 * A_mismatches + 1.5 * B_mismatches + 3 * DR_mismtaches + 3 * DQ_mismatches)}
#'     \item{unacceptable_antigens}{Presence of any donor HLA antigens that would be unnaceptable for this patient. 0 or 1}
#'   }
#'
#' @param state_debts A dataframe/tibble with 25 rows, one for each ordered pair of the 5 Australian transplant jurisdictions (NSW/ACT, VIC/TAS, QLD, WA, SA/NT).
#'   \describe{
#'     \item{from_state}{State that owes kidneys. Option are NSW, VIC, QLD, SA, WA}
#'     \item{to_state}{State that is owed kidneys. Option are NSW, VIC, QLD, SA, WA}
#'     \item{net_debt}{Net number of kidneys owed. Integer, can be negative. Must by symmetric, e.g. if NSW -> VIC net debt is +2, then VIC -> NSW net debt must be -2. if from_state = to_state then net_debt must be 0.}
#'   }
#'
#' @param hla_age_scaling_max Maximum HLA points multiplier (applied to youngest patients). See HLA-age scaling below for details.
#' @param hla_age_scaling_min Minimum HLA points multiplier (applied to oldest patients). See HLA-age scaling below for details.
#' @param hla_age_scaling_mid Mid-point (inflection point) of age-scaling multiplier. See HLA-age scaling below for details.
#' @param hla_age_scaling_slope Slope (rate of decline) of age-scaling factor. See HLA-age scaling below for details.
#'
#' @param pra_bonus_base PRA where the rate of decline changes from linear to exponential.
#' @param pra_max Maximum PRA bonus applied to someone with PRA 100.
#' @param pra_decay Exponential rate of change (decay) in the PRA bonus
#'
#' @param living_donor_kdpi If donor is a living donor (altruistic or end of kidney exchange chain), then assume this is their KDPI for prognosis matching.
#' @param paediatric_kdpi_max If donor is padeiatric (<18 years old), then their KDPI is capped at this maximum for prognosis matching.
#' @param prognosis_match_max Maximum prognosis matching points possible.
#'
#' @param national_urgent_bonus Bonus points for patients with national urgent status. See urgent/priority bonus points below for details.
#' @param state_urgent_bonus Bonus points for patients with state urgent status. See urgent/priority bonus points below for details.
#' @param prior_donor_bonus Bonus points for patients who have previously donated a kidney. See urgent/priority bonus points below for details.
#' @param kidney_after_other_organ_bonus Bonus points for patients who needed a multi-organ transplant including a kidney, but accepted a transplant without a kidney. See urgent/priority bonus points below for details.
#'
#' @param shipping_threshold_base The default shipping threshold (i.e. points required to justify interstate shipping when net debt is 0). See Shipping threshold below for details.
#' @param shipping_threshold_max Maximum possible shipping threshold. See Shipping threshold below for details.
#' @param state_payback_rate  The amount the threshold increases for every extra kidney the patient's state owes the donor's state. See Shipping threshold below for details.
#'
#' @param samestate_bonus Bonus points given to patients in the same state as the donor. Avoids shipping for marginal gain by ensuring kidneys are only shipped interstate to patients who have an allocation score at least this much higher than the next same-state patient.
#'
#' @param bldgrp_a_ab_threshold A donors to AB patients are allowed if pre-bloodgroup points (HLA match, PRA bonus, prognosis match) >= this threshold.
#' @param bldgrp_o_b_threshold O donors to B patients are allowed if pre-bloodgroup points (HLA match, PRA bonus, prognosis match) >= this threshold.
#' @param bldgrp_compatibile_threshold All ABO compatible transplants are allowed if pre-bloodgroup points (HLA match, PRA bonus, prognosis match) >= this threshold.
#'
#' @param spk_threshold Points threshold for kidney-only patients to be prioritised above pancreas/SPK patients. See Simultaneous pancreas-kidney (SPK) rules below for details.
#' @param spk_bonus Bonus points given to kidney-only patients who also need a pancreas. See Simultaneous pancreas-kidney (SPK) rules below for details.
#'
#' @details
#' Technical specifications of the new algorithm are available here:\cr
#' \href{https://tsanz.com.au/storage/Advisory_Committees_and_Working_Groups/New-Allocation-Algorithm-Summary-for-Website.pdf#page=19}{KOALA technical specifications}.
#'
#'
#' \strong{HLA-age scaling}\cr
#' HLA-age scaling multiplier = ((max - min) * exp(-(age / mid)^slope) + min).\cr
#' HLA-matching points are multiplied by this age-scaling multiplier, to reflect that HLA-matching is more important for younger patients, and less important (but still important) for older patients.
#'
#' \strong{PRA bonus}\cr
#' Bonus points given to patients based on their sensitisation:\cr
#' PRA bonus = base * (pra/100) + (max - base) * decay^(100-pra).
#'
#' \strong{Prognosis matching}\cr
#' Prognosis matching points = max * ((epts^2 - 101 * epts - 100 * abs(epts - kdpi) + 10000)/9900)
#'
#' \strong{Urgent/priority bonuses}\cr
#' Only one priority bonus can be applied, to each patient (the maximum of national_urgent_bonus, state_urgent_bonus, prior_donor_bonus, and kidney_after_other_organ_bonus).
#'
#' \strong{Shipping threshold}\cr
#' Kidneys are first allocated to the 'national' waitlist, which includes:
#' - patients in the same state as the donor,
#' - interstate patients with a pre-shipping score (waiting time, HLA match, PRA bonus, prognosis match, urgent/priority bonus) >= the shipping threshold.\cr
#' Threshold = base - state_payback_rate * net_debt).\cr
#' Capped at 'max'. 'net_debt' is the net number of kidneys owed from the patient's state to the donor's state.\cr
#'
#' Then kidneys are allocated on 'interstate utilisation' to all remaining patients
#'
#' \strong{Simultaneous pancreas-kidney (SPK) rules}\cr
#' - If the donor has a pancreas and only one kidney, then kidneys are first allocated to kidney-only patients with pre-SPK points (waiting time, HLA match, PRA bonus, prognosis match, urgent/priority bonus, same-state bonus) >= the SPK threshold.
#'    If any of these patients also need a pancreas, they are given bonus points (SPK bonus), but only if they accept the pancreas along with the kidney.
#' - Then, one kidney is offered to the SPK list.
#' - And finally, any remaining kidneys are offered to all remaining kidney-only patients.\cr
#' This function assumes that any kidney offered together with the pancreas to the SPK list will be accepted, so all kidney-only patients with pre-spk points below the threshold will have one less kidney available for allocation.
#'
#' @return A dataframe/tibble with one row per waitlisted patient per donor.
#' For each donor, the waitlisted patients are ranked according to KOALA, with columns for their total score and individual score components.
#' All waitlisted patients are included in the ouptut, regardless of compatibility. E.g. for a bloodgroup AB donor, bloodgroup O patients are included in the ranked list, but are ranked below all ABO compatible patients.
#'
#' @examples
#' # Load example datasets
#' data(waitlist_example)
#' data(donor_example)
#' data(crossmatch_example)
#' data(state_debts_example)
#'
#' # Generate a ranked list using KOALA
#'ranked_list <- run_koala(
#' waitlist = waitlist_example,
#' donors = donor_example,
#' crossmatch = crossmatch_example,
#' state_debts = state_debts_example
#' )
#'
#' @export
run_koala <- function(waitlist, donors, crossmatch, state_debts,
                      hla_age_scaling_max = 4.5,
                      hla_age_scaling_min = 0.5,
                      hla_age_scaling_mid = 45,
                      hla_age_scaling_slope = 2.2,
                      pra_bonus_base = 1,
                      pra_max = 30,
                      pra_decay = 0.8,
                      paediatric_kdpi_max = 20,
                      living_donor_kdpi = 20,
                      prognosis_match_max = 3,
                      national_urgent_bonus = 15,
                      state_urgent_bonus = 12,
                      prior_donor_bonus = 10,
                      kidney_after_other_organ_bonus = 12,
                      shipping_threshold_base = 12,
                      shipping_threshold_max = 15,
                      state_payback_rate = 0.5,
                      samestate_bonus = 1,
                      bldgrp_a_ab_threshold = 5,
                      bldgrp_o_b_threshold = 12,
                      bldgrp_compatibile_threshold = 18,
                      spk_threshold = 15,
                      spk_bonus = 2) {

  ranked_list <- waitlist %>%
    crossing(donors %>% mutate(donor_seq = row_number())) %>%
    left_join(crossmatch) %>%
    left_join(state_debts, by = join_by("donor_state" == "from_state", "patient_state" == "to_state")) %>%
    mutate(
      waityears_points = patient_waityears,
      hla_adjust_raw = (patient_hla_mismatch_mean - hla_mismatch) / patient_hla_mismatch_sd,
      hla_age_scaling = (hla_age_scaling_max - hla_age_scaling_min) *
        exp(-(patient_age / hla_age_scaling_mid)^hla_age_scaling_slope) + hla_age_scaling_min,
      hla_adjust = hla_adjust_raw * hla_age_scaling,
      hla_match_points = hla_adjust,
      pra_bonus_points = pra_bonus_base*(patient_pra/100) + (pra_max - pra_bonus_base) * pra_decay^(100-patient_pra),
      adjusted_kdpi = case_when(
        donor_living == 1 ~ living_donor_kdpi,
        donor_age < 18 ~ pmin(donor_kdpi, paediatric_kdpi_max),
        TRUE ~ donor_kdpi
      ),
      prognosis_match_points = prognosis_match_max * ((patient_epts^2 - 101*patient_epts - 100*abs(patient_epts - adjusted_kdpi) + 10000)/9900),
      samestate = if_else(patient_state == donor_state, 1, 0),
      samestate_points = if_else(patient_state == donor_state, samestate_bonus, 0),
      urgent_priority_points = pmax(
        if_else(patient_national_urgent == 1, national_urgent_bonus, 0),
        if_else(patient_state_urgent == 1 & samestate == 1, state_urgent_bonus, 0),
        if_else(patient_prior_donor == 1, prior_donor_bonus, 0),
        if_else(patient_kidney_after_other_organ == 1, kidney_after_other_organ_bonus, 0)
      ),
      pre_shipping_points = waityears_points + hla_match_points + pra_bonus_points + prognosis_match_points + urgent_priority_points,
      shipping_threshold = pmin(shipping_threshold_max, shipping_threshold_base - state_payback_rate * net_debt),
      shipping_priority = case_when(
        samestate == 1 ~ 1,
        pre_shipping_points >= shipping_threshold ~ 1,
        TRUE ~ 0
      ),
      interstate_utilisation = if_else(samestate == 0 & pre_shipping_points < min(shipping_threshold), 1, 0),
      bloodgroup_identical = if_else(patient_bloodgroup == donor_bloodgroup, 1, 0),
      bloodgroup_compatible = case_when(
        patient_bloodgroup == donor_bloodgroup ~ 1,
        patient_bloodgroup == "AB" ~ 1,
        patient_bloodgroup == "A" & donor_bloodgroup %in% c("O", "A") ~ 1,
        patient_bloodgroup == "B" & donor_bloodgroup %in% c("O", "B") ~ 1,
        patient_bloodgroup == "O" & donor_bloodgroup %in% c("O") ~ 1,
        TRUE ~ 0
      ),
      pre_bloodgroup_points = hla_match_points + pra_bonus_points + prognosis_match_points,
      bloodgroup_priority = case_when(
        bloodgroup_identical == 1 ~ 1,
        patient_national_urgent == 1 & bloodgroup_compatible == 1 ~ 1,
        donor_bloodgroup == "A" & patient_bloodgroup == "AB" & pre_bloodgroup_points >= bldgrp_a_ab_threshold ~ 1,
        donor_bloodgroup == "O" & patient_bloodgroup == "B" & pre_bloodgroup_points >= bldgrp_o_b_threshold ~ 1,
        bloodgroup_compatible == 1 & pre_bloodgroup_points >= bldgrp_compatibile_threshold ~ 1,
        TRUE ~ 0
      ),
      pre_spk_points = waityears_points + hla_match_points + pra_bonus_points + prognosis_match_points + urgent_priority_points + samestate_points,
      kidneys_available = case_when(
        donor_pancreas == 0 ~ donor_kidneys,
        pre_spk_points >= pmax(0, spk_threshold ~ donor_kidneys),
        TRUE ~ donor_kidneys - 1
      ),
      spk_points = if_else(donor_pancreas == 1 & patient_spk == 1 & pre_spk_points >= spk_threshold, spk_bonus, 0),
      tiebreaker_points = runif(n()),
      points = waityears_points + hla_match_points + pra_bonus_points + prognosis_match_points + urgent_priority_points + samestate_points + spk_points
    ) %>%
    arrange(donor_seq,
            unacceptable_antigens, desc(bloodgroup_priority), desc(bloodgroup_compatible),
            desc(shipping_priority),
            desc(points),
            desc(urgent_priority_points), desc(samestate), desc(bloodgroup_identical), desc(waityears_points),
            desc(hla_match_points), desc(pra_bonus_points), desc(prognosis_match_points), desc(tiebreaker_points)) %>%
    group_by(donor_seq) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(kidney_offer = if_else(rank <= kidneys_available & unacceptable_antigens == 0 & bloodgroup_compatible == 1, 1, 0)) %>%
    select(donor_seq, donor_id, donor_state, donor_bloodgroup, donor_pancreas,
           rank, kidney_offer, patient_id, patient_state, patient_bloodgroup, unacceptable_antigens,
           points,
           urgent_priority_points,
           waityears_points,
           hla_match_points,
           pra_bonus_points,
           prognosis_match_points,
           samestate_points,
           spk_points,
           net_debt, pre_shipping_points, shipping_threshold, interstate_utilisation,
           everything())

  return(ranked_list)
}
