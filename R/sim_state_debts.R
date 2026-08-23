#' @importFrom dplyr tibble
#'
#' @name sim_state_debts
#' @title Simulate state kidney debts
#'
#' @description Function to simulate number of kidneys owing/owed to each OrganMatch lab state (NSW, VIC, QLD, SA, WA). Samples from a uniform distribution, but ensures that sum of debts across all states is zero.
#' @param min_debt Minimum possible debt (should be negative to allow at least some states to be owed kidneys)
#' @param max_debt Maximum possible debt (should be positive to allow at least some states to owe kidneys)
#'
#' @return A tibble with 5 rows (one for each state) and 2 columns:
#'   \describe{
#'     \item{state}{OrganMatch lab state: NSW, VIC, QLD, SA, or WA}
#'     \item{debt}{Simulated net kidney debt for that state.}
#'   }
#'
#' @examples
#' # Assume states can be owed at most 6 kidneys (min_debt = -6), and can owe at most 6 kidneys (max_debt = 6)
#' state_debts <- sim_state_debts(min_debt = -6, max_debt = 6)
#'
#' @export
sim_state_debts <- function(min_debt, max_debt) {

  states <- c("NSW", "VIC", "QLD", "SA", "WA")
  n_states <- length(states)

  # Function to safely sample when the min value = max value
  safe_sample <- function(x) {
    if (length(x) == 1) return(x)
    sample(x, 1)
  }

  # Initialise vector of debts (start with 0 for each state)
  debts <- integer(n_states)

  # Target debt to ensure debts across all states sums to zero
  remaining_target <- 0
  for (i in seq_len(n_states - 1)) {
    remaining_n <- n_states - i
    lower <- max(min_debt, remaining_target - remaining_n * max_debt)
    upper <- min(max_debt, remaining_target - remaining_n * min_debt)
    debts[i] <- safe_sample(lower:upper)
    remaining_target <- remaining_target - debts[i]
  }
  debts[n_states] <- remaining_target

  tibble(state = states, debt = debts)
}
