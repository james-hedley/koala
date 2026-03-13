#' Example waitlist dataset
#'
#' @format A tibble with 1,354 rows and 14 columns:
#'   \describe{
#'     \item{patient_id}{Unique identifier for each waitlisted patient}
#'     \item{patient_waityears}{Total time on the waiting list in years (as a decimal, not rounded)}
#'     \item{patient_state}{Australian state where the patient is waitlisted. Options are: NSW, VIC, QLD, SA, WA}
#'     \item{patient_age}{Patient age in years}
#'     \item{patient_bloodgroup}{ABO blood group of the patient}
#'     \item{patient_pra}{Panel reactive antibody (PRA) value. Decimal 0-100}
#'     \item{patient_epts}{Estimated Post-Transplant Survival (EPTS) score. Integer 1-100}
#'     \item{patient_hla_mismatch_mean}{Mean HLA mismatch score}
#'     \item{patient_hla_mismatch_sd}{Standard deviation of HLA mismatch score}
#'     \item{patient_national_urgent}{Indicator for nationally urgent status. 0 or 1}
#'     \item{patient_state_urgent}{Indicator for state urgent status. 0 or 1}
#'     \item{patient_prior_donor}{Indicator for prior living donor status. 0 or 1}
#'     \item{patient_kidney_after_other_organ}{Indicator for kidney-after-other-organ transplant. 0 or 1}
#'     \item{patient_spk}{Indicator for simultaneous pancreas–kidney transplant. 0 or 1}
#'   }
#' @source Simulated data
"waitlist_example"


#' Example donor dataset
#'
#' @format A tibble with 1 rows and 14 columns:
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
#'     \item{donor_living}{Living donor (altruistic or end of kidney exchange chain. 0 or 1)}
#'   }
#' @source Simulated data
"donor_example"


#' Example crossmatch dataset
#'
#' @format A tibble with 1,354 rows and 4 columns:
#'   \describe{
#'     \item{patient_id}{Unique identifier for each waitlisted patient}
#'     \item{donor_id}{Unique identifier for each donor}
#'     \item{hla_mismatch}{HLA mismatch score (ABDRDQ score) for this patient-donor pair. Calculated as: sqrt(1 x A_mismatches + 1.5 x B_mismatches + 3 x DR_mismtaches + 3 x DQ_mismatches)}
#'     \item{unacceptable_antigens}{Presence of any donor HLA antigens that would be unnaceptable for this patient. 0 or 1}
#'   }
#' @source Simulated data
"crossmatch_example"


#' Example state_debts dataset
#'
#' @format A tibble with 5 rows and 2 columns:
#'   \describe{
#'     \item{state}{State. Options are NSW, VIC, QLD, SA, WA}
#'     \item{net_debt}{Net number of kidneys owed. Integer, can be negative.}
#'   }
#' @source Simulated data
"state_debts_example"
