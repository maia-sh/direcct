# Clean and combine results

source(here::here("R", "environment.R"), echo = FALSE)

# Get raw results data ----------------------------------------------------

# Manual results: From manual trial screening and subsequent search. Available in "data-raw".
# Manual verified results: From manual results search of auto-detected results. Available in "data-raw". Manually verified results that had a trial id were included in the trial search and thus appear in manual results, as appropriate. However, a few results had no apparent trial id and should be checked against the manual results (in case they were later found to be associated to a trial with an id) and if not joined in without a trial id.
#
# Note: Additional trials discovered via manual results verification are NOT included in the trials dataset, since we per protocol consider trials only from the ICTRP COVID dataset. We do capture these additional results, which may be linked to trials in later rounds, if trials added to ICTRP COVID dataset.

manual_results <-
  read_csv(latest("manual-results.csv", here("data", "raw"))) %>%

  # Remove duplicated result with incorrect completion date
  # NOTE: could also do this is prior cleaning of manual results
  filter(!(doi == "10.1038/s41467-020-19056-6" & !is.na(date_completion)))

manual_verified_results <-
  read_csv(latest("manual-verified-results.csv", here("data", "raw")))

# We also need to get trial ids to add to results
trials_ids <-
  # read_csv(here("data", "trials-ids.csv"))
  read_rds(here("data", "processed", "cleaned-trials.rds")) %>%
  select(id, trialid)

# If a result without a TRN from manual results verification was later found via trial results search and thus associated with a trial id, we do not need to join this result in
# Note: None of the results without TRN found in trial results search
results_no_trn <-
  manual_verified_results %>%
  filter(is.na(trn_1)) %>%
  anti_join(manual_results, by = "doi") %>%
  anti_join(manual_results, by = "pmid") %>%
  select(-starts_with("trn"), -title) %>%
  mutate(search_type = "results_check",
         trn = "no TRN found") %>%

  # Date should be 1st of the month, if not specified
  mutate(
    date_publication = if_else(
      str_detect(date_publication, "202[01]-[01][0-9]$"),
      str_replace(date_publication, "$", "-01"),
      date_publication),
    date_publication = as.Date(date_publication)
  )

# 2021-02-11: Change `id_trial` to `id` to keep consistent name across database
results <-
  manual_results %>%

  # Add in database trial identifiers
  # Note: could remove TRNs for trials in database
  right_join(trials_ids, ., by = "trialid") %>%
  # rename(id_trial = id#, trn = trialid, everything()
  # ) %>%
  mutate(trn = if_else(is.na(id), trialid, "see `trials`"), .after = id) %>%
  select(-trialid) %>%
  bind_rows(results_no_trn)

# NOTE: could add result unique id

write_rds(results, here("data", "processed", "cleaned-results.rds"))
# write_csv(results, here("data", "results.csv"))
