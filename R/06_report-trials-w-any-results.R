source(here::here("R", "environment.R"), echo = FALSE)

# Create single csv of all trials with any result

# This dataset includes all trials in the [ICTRP COVID-19 trials dataset](https://www.who.int/clinical-trials-registry-platform) for which we found *any* result.
# Each row represents a unique trial which may have multiple registrations and/or results.
# The dataset does excludes trials *not* in the ICTRP COVID-19 trials dataset, even if results were found. For example, results for which we found no trial registration number (TRN) or only a TRN not in the ICTRP dataset (e.g., NCT04323527).
# The dataset is *not* limited to trials or results included in the DIRECCT analysis population.
# - I.e., trials are not limited to trials which passed the DIRECCT automated and manual screening criteria as valid COVID-19 intervention or prevention clinical trials, that are not withdrawn, and completed per the registry by June 30, 2020.
# - I.e., results are not limited to full-results as registry summary results, preprints, or journal articles.
# - Additional filters can be applied using the inclusion/exclusion criteria columns (prefixed with `is_` or `has_`)

# Since we did not systematically search for protocols, protocols may be captured as a `pub_type` "other" or under the `comments`. Additional trials which did not have results (and therefore are not included in this dataset) may have protocols noted under `comments`.
#
# **Codebook**
# We are in the process of developing a more detailed codebook, so the following is a summary.
# `id`: unique trial id within our database. Please keep this id column!
# columns between `id` and `trn_1`: trial information, including from ICTRP, individual registry indicated in `source_register`, automated and manual screening, and inclusion/exclusion criteria (prefixed with `is_` or `has_`)
# `trn_#` and `registry_#`: trial registration number and registry, suffixed by number
# `doi_#`, `pmid_#`, `cord_id_#`, `data_publication_#`, `date_completion_#`, `comments_results_#`: results publication information, suffixed by number

# Load reporting data -----------------------------------------------------

all_trials <- read_csv(here("data", "reporting", "trials.csv"))
all_registrations <- read_csv(here("data", "reporting", "registrations.csv"))
all_results <- read_csv(here("data", "reporting", "results.csv"))

# Get all trials with any results -----------------------------------------

trials_w_results <-
  semi_join(all_trials, all_results, by = "id")

# Get registrations -------------------------------------------------------
# Registrations for all trials with any result
# 1 row per trial (wide)

registrations <-
  all_registrations %>%

  # Limit to registrations with results
  semi_join(trials_w_results, by = "id") %>%
  select(-source) %>%

  # Pivot to 1 row per trial (multiple registrations)
  pivot_wider(id, names_from = n_trn, values_from = c(trn, registry))


# Get results -------------------------------------------------------------
# Results for all trials with any result
# 1 row per trial (wide)

results <-
  all_results %>%
  # Limit to results in trials dataset
  semi_join(trials_w_results, by = "id") %>%

  select(-trn, -search_type, -search_engine, comments_results = comments) %>%

  # Add result number by trial
  group_by(id) %>%
  mutate(n_result = row_number()) %>%
  ungroup() %>%

  # Pivot to 1 row per trial (multiple results)
  pivot_wider(id,
              names_from = n_result,
              values_from = c(pub_type,
                              doi,
                              pmid,
                              cord_id,
                              url,
                              date_publication,
                              date_completion,
                              comments_results
              )
  ) %>%

  # Reorder columns so that grouped by result number
  relocate(any_of(ends_with(str_c(1:5))))


# Combine trials, registrations, and results ------------------------------
trials_w_results_combined <-
  trials_w_results %>%
  left_join(registrations, by = "id") %>%
  left_join(results, by = "id")

write_csv(trials_w_results_combined, here("data", "reporting", "trials-w-results.csv"))
