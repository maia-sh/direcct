# Create screening functions ----------------------------------------------

# Returns list of inclusion counts and filtered dataframe
count_filter <- function(data, vars) {

  counts <-
    tibble(name = as.character(),
           value = as.logical(),
           n = as.integer()
    )

  for (var in vars) {
    counts <-
      data %>%
      count(.data[[var]]) %>%
      pivot_longer(-n) %>%
      add_row(counts, .)

    data <- filter(data, .data[[var]])
  }

  list(data = data, counts = counts)

}

# Report summary counts from screening
report_n <- function(counts, var, condition) {
  n <-
    counts %>%
    filter(name == var & value == condition) %>%
    pull(n)

  # If empty, count is 0
  if (rlang::is_empty(n)){n <- 0}

  n
}


# Screen trials -----------------------------------------------------------

# Screening criteria, in order
# Note: first 3 criteria alternatively encoded as `is_eligible_study`
screening_criteria <- c(
  "is_reg_2020",             # Must have 2020+ registration (auto)
  "is_intervention",         # Must be intervention/prevention (auto)
  "is_not_withdrawn",        # Must not be withdrawn (auto)
  "is_rcd_cutoff_1",         # Must be complete by cutoff (auto)
  "is_clinical_trial_manual",# Must be clinical trial (manual)
  "is_covid_manual",         # Must be covid intervention/prevention (manual)
  "is_not_withdrawn_manual", # Must not be withdrawn (manual)
  "is_not_dupe"              # Must not be duplicate
)

screened_trials <-
  all_trials %>%

  # is_dupe needs to be is_not_dupe
  # TODO: could change in back-end
  mutate(is_not_dupe = if_else(is_dupe, FALSE, TRUE, missing = TRUE)) %>%
  count_filter(screening_criteria)

trials <-
  screened_trials$data %>%

  # Target enrollment should be numeric
  mutate(target_enrollment = as.numeric(target_enrollment))

screening_counts <-
  screened_trials$counts %>%
  add_row(name = "ictrp_all", value = TRUE, n = nrow(all_trials), .before = 1)


# Report screening counts -------------------------------------------------

n_ictrp <- report_n(screening_counts, "ictrp_all", TRUE)
n_reg_2020 <- report_n(screening_counts, "is_reg_2020", TRUE)
n_reg_2020_ex <- report_n(screening_counts, "is_reg_2020", FALSE)
n_intervention <- report_n(screening_counts, "is_intervention", TRUE)
n_obs_ex <- report_n(screening_counts, "is_intervention", FALSE)
n_not_withdrawn <- report_n(screening_counts, "is_not_withdrawn", TRUE)
n_withdrawn_ex <- report_n(screening_counts, "is_not_withdrawn", FALSE)
n_rcd_cutoff_1 <- report_n(screening_counts, "is_rcd_cutoff_1", TRUE)
n_rcd_cutoff_1_ex <- report_n(screening_counts, "is_rcd_cutoff_1", FALSE)
n_clinical_trial_manual <- report_n(screening_counts, "is_clinical_trial_manual", TRUE)
n_clinical_trial_manual_ex <- report_n(screening_counts, "is_clinical_trial_manual", FALSE)
n_covid_manual <- report_n(screening_counts, "is_covid_manual", TRUE)
n_covid_manual_ex <- report_n(screening_counts, "is_covid_manual", FALSE)
n_not_withdrawn_manual <- report_n(screening_counts, "is_not_withdrawn_manual", TRUE)
n_withdrawn_manual_ex <- report_n(screening_counts, "is_not_withdrawn_manual", FALSE)
n_not_dupe <- report_n(screening_counts, "is_not_dupe", TRUE)
n_dupe_ex <- report_n(screening_counts, "is_not_dupe", FALSE)

# Return screening info ---------------------------------------------------

rm(screening_criteria, screened_trials)
