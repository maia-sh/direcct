source(here::here("R", "environment.R"), echo = FALSE)

# Deduplicate registrations/trials

# Some (n = 35) trials in our database are duplicates based on cross-registrations we identified that weren't deduplicated prior to entering the database, so we will deduplicate now.
# We identify duplicates based on the registrations, i.e., the same registration is associated with multiple trials in our database. We then identify the primary trial in our {trials} database based on a registry preference order. We flag duplicated in {trials} and tidy {registrations} and {results} to map to primary trial ids only. We exclude duplicate trial ids for analysis.
#
# For {trials}, we add two columns to flag `is_dupe` (TRUE for non-primary duplicates) and the `dupe_primary` (the primary id of non-primary duplicates). For analyses, we exclude `is_dupe`.
# For {registrations}, we remove duplicate registrations to duplicate trial ids and re-assign additional registrations associated with a duplicate trial id only, to the primary trial id.
# For {results}, we re-assign any results associated with a duplicate trial id, to the primary trial id.
#
# Note: Since we exclude duplicates from analysis, this means that a duplicate id which had previously been included in the analysis population (whereas the primary id was not) will then be excluded. This is because the scraped registry data does not necessarily match across registries, and a trial may be excluded on the basis of a specific registry (e.g., EUCTR does not display completion dates until completion).
# To handle this, we considered an alternative approach for selecting the primary trial id, based on incorporating some manual changes to the registry preference order, documented in the script (commented out). These manual changes come from our manual check of the duplicates. We check the duplicates for whether the more informative version (e.g., completion date was available and complete, and hence trial was searched) would be included as the primary trial id. In other words, if `is_analysis_pop_cutoff_1` is TRUE for one id and FALSE for the other, we would like to include the primary trial id where `is_analysis_pop_cutoff_1` == TRUE.
# For example, this seems to be the case for one trial: based on registry preferences, we retain "tri00650" (EUCTR2020-001236-10) and remove the duplicate "tri03742" (NL8491). However, "tri03742" was previously in our analysis population whereas "tri00650" is not.
# We currently (2021-02-09) chose to use registry preference order only, but may add manual changes later.


# Get cleaned data --------------------------------------------------------

trials <- read_rds(here("data", "processed", "cleaned-trials.rds"))
registrations <- read_rds(here("data", "processed", "cleaned-registrations.rds"))
results <- read_rds(here("data", "processed", "cleaned-results.rds"))

# Create a registry preference order --------------------------------------

# We are interested in "ClinicalTrials.gov" > "EU Clinical Trials Register" > "ISRCTN" > other (remaining registries sorted alphabetically)

registry_order <-
  c(
    # Ordered
    "ClinicalTrials.gov",
    "EudraCT",
    "ISRCTN",

    # Unordered (alphabetical)
    "ANZCTR",
    "ChiCTR",
    "CRiS",
    "CTRI",
    "DRKS",
    "IRCT",
    "JapicCTI",
    "JMACCT",
    "jRCT",
    "LBCTR",
    "NTR",
    "PACTR",
    "ReBec",
    "REPEC",
    "RPCEC",
    "SLCTR",
    "TCTR",
    "UMIN-CTR"
  )

# Identify duplicate registrations ----------------------------------------

# I.e., the same registration is associated with multiple trials in our database

duplicate_registrations <-
  janitor::get_dupes(registrations, trn) %>%
  select(trn, id)

# Deduplicate trials ------------------------------------------------------

# Based on trials identified with duplicate registrations, get duplicate trials, identify primary trial id, and add info about duplicates.
# Also, create duplicate lookup table

duplicate_trials <-
  trials %>%
  select(id, trialid) %>%

  # Subset duplicate trials and join in duplicate registration info
  semi_join(duplicate_registrations, by = "id") %>%
  left_join(duplicate_registrations, by = "id") %>%

  # Create a new registry column with registry names that match `registry_order`,
  # since `source_register` matches ICTRP instead
  mutate(trialid_registry = ctregistries::which_registries(trialid)) %>%

  # Convert trialid to a leveled factor based on registry preference order
  mutate(trialid_registry = factor(trialid_registry, levels = registry_order)) %>%

  # We want the highest preference registry for each trial, so group by `trn` and then arrange by `registry_order` and add rank
  group_by(trn) %>%
  arrange(trialid_registry, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%

  # We may decide to manually re-order some trials
  # mutate(rank = case_when(
  #   id == "tri00650" & trialid == "EUCTR2020-001236-10-NL" ~ as.integer(2),
  #   id == "tri03742" & trialid == "NL8491" ~ as.integer(1),
  #   TRUE ~ rank
  # )) %>%

  # Duplicate if rank isn't first
  mutate(is_dupe = if_else(rank >= 2, TRUE, FALSE))

# Get the primary trial id
# Note: This could probably be done within the pipe
primary_id <-
  duplicate_trials %>%
  group_by(trn) %>%
  filter(rank == 1) %>%
  distinct(dupe_primary = id)

# Join primary trial id into duplicate_trials
duplicate_trials <-
  duplicate_trials %>%
  left_join(primary_id, by = "trn") %>%
  mutate(dupe_primary = if_else(!is_dupe, NA_character_, dupe_primary))

# For deduping {trials}, {registrations}, and {results},
# we want info on duplicates, so limit to dupe rows and info columns
duplicate_trial_lookup <-
  duplicate_trials %>%
  filter(is_dupe) %>%
  select(id, is_dupe, dupe_primary)

# Add duplicate info to trials
deduped_trials <-
  trials %>%
  left_join(duplicate_trial_lookup, by = "id") %>%

  # Remove extraneous registration info from {trials}
  # Keep trialid and source_register because this shows where registry info from, in case `n_trn` order is not exact match in {registries}

  select(-starts_with("cross_reg"))

write_rds(deduped_trials, here("data", "processed", "deduped-trials.rds"))

# Also write duplicate_trials to csv for ND's deduping for next round
write_csv(duplicate_trials, here("data", "processed", "duplicate-trials.csv"))

# Deduplicate registrations -----------------------------------------------

# We now clean up registrations. We want to remove duplicate registrations to duplicate trial ids. In addition, some duplicate trial ids have additional registrations that are not associated to the primary trial id. We want to re-associate them to the primary id before removing registrations to the secondary id. We do this by replacing `id` with `dupe_primary` for all registrations associated with a duplicate trial id. Having remove the duplicate trial ids, we then remove duplicate registrations numbers.
#
# For example "tri00706" is a dupe for "tri02570", and both have "NCT04345276". However, "tri00706" also has "ChiCTR2000031734". We need to reassociate "ChiCTR2000031734" with "tri02570".
# This also appears to occur for duplicate trial ids "tri02901", "tri03724", and "tri03742".
#
# example_reg_dup_id <-
#   registrations %>%
#   filter(trn == "NCT04345276" | id == "tri00706" | id == "tri02570")

deduped_registrations <-
  registrations %>%

  # Join in dupe info
  left_join(duplicate_trial_lookup, by = "id") %>%

  # If id is_dupe, then replace id with dupe_primary
  mutate(id = if_else(is_dupe, dupe_primary, id, missing = id)) %>%

  select(-is_dupe, -dupe_primary) %>%

  # Check duplicate trns which should now have the same id
  # Note: this may be possible with assertr but seems not group aware
  # group_by(trn) %>% assertr::assert(is_uniq, id)
  # janitor::get_dupes(trn) %>%
  # Another check is that distinct(trn, id) gives same output as distinct(trn)

  # Remove duplicates
  distinct(trn, .keep_all = TRUE) %>%

  # `n_trn` is no longer correct so reassign. Also, apply
  # Convert trialid to a leveled factor based on registry preference order
  mutate(registry = factor(registry, levels = registry_order)) %>%

  # We want to number TRNs based on registry preference order for each trial, so group by `id` and then arrange by `registry_order` and add `n_trn`
  group_by(id) %>%
  arrange(registry, .by_group = TRUE) %>%
  mutate(n_trn = row_number()) %>%
  ungroup()

# Deduplication worked
# registrations %>%
#   filter(id == "tri00706" | id == "tri02570")
# deduped_registrations %>%
#   filter(id == "tri00706" | id == "tri02570")

write_rds(deduped_registrations, here("data", "processed", "deduped-registrations.rds"))

# Deduplicate results -----------------------------------------------------

# For results, we re-assign any results associated with a duplicate trial id, to the primary trial id.
# For example, reassign result from "tri02901" to "tri02448"
deduped_results <-
  results %>%
  # semi_join(duplicate_trial_lookup, by = c("id_trial" = "id")) %>%
  # Join in dupe info
  left_join(duplicate_trial_lookup, by = c("id_trial" = "id")) %>%

  # If id is_dupe, then replace id with dupe_primary
  mutate(id_trial = if_else(is_dupe, dupe_primary, id_trial, missing = id_trial)) %>%

  select(-is_dupe, -dupe_primary)

write_rds(deduped_results, here("data", "processed", "deduped-results.rds"))
