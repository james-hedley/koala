#' @importFrom dplyr %>% tibble left_join mutate join_by
#'
#' @name get_state_net_debts
#' @title Get state net debts
#'
#' @description Function to calculate net kidney debts between each pair of OrganMatch lab states, from a given set of state kidney debts.
#' @param state_debts A dataframe/tibble with 5 rows (one for each state) and 2 columns:
#'   \describe{
#'     \item{state}{OrganMatch lab state: NSW, VIC, QLD, SA, or WA}
#'     \item{debt}{Number of kidneys that state owes to the national pool. A positive debt means that state owes kidneys, a negative means that state is owed kidneys.}
#'
#' @return A dataframe/tibble with 20 rows, one for each state paired with each other state (including itself), and 3 columns:
#'  \describe{
#'     \item{from_state}{The state kidneys are coming from: NSW, VIC, QLD, SA, or WA}
#'     \item{to_state}{The state kidneys are going to: NSW, VIC, QLD, SA, or WA}
#'     \item{net_debt}{The net number of kidneys the from_state owes to the to_state. Positive means the from_state owes kidneys, negative means the from_state is owed kidneys. If from_state is same as to_state, then net_debt is always 0.
#'
#' @examples
#' state_debts <- sim_state_debts(min_debt = -6, max_debt = 6) # Simulate state debts from a uniform distribution, ensuring sum of debts across all states is zero.
#' state_net_debts <- get_state_net_debts(state_debts)
#' state_net_debts
#'
#' @export
get_state_net_debts <- function(state_debts) {
  tibble(from_state = rep(c("NSW", "VIC", "QLD", "SA", "WA"), times = 5),
         to_state =  rep(c("NSW", "VIC", "QLD", "SA", "WA"), each = 5)) |>
    left_join(state_debts |> rename(from_debt = debt), by = join_by(from_state == state)) |>
    left_join(state_debts |> rename(to_debt = debt), by = join_by(to_state == state)) |>
    mutate(net_debt = from_debt - to_debt) |>
    select(-c(from_debt, to_debt))
}
