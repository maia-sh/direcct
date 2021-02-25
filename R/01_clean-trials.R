source(here::here("R", "environment.R"), echo = FALSE)

# Get raw trials data -----------------------------------------------------

# ICTRP: pre-processed and sourced from Python repo
# Trial registries: pre-processed and sourced from Python repo
# Manual trials: From manual trial screening. currently manually included in "data-raw"

ictrp_data <-
  read_csv("https://raw.githubusercontent.com/ebmdatalab/covid19_results_reporting/master/data/cleaned_ictrp_29June2020.csv") %>%

  # Check for duplicate rows
  assert_rows(col_concat, is_uniq, everything()) %>%

  # Change index for more informative id
  select(-index) %>%
  mutate(id = str_c("tri",str_pad(row_number(), width = 5, side = "left", pad = 0)),
         .before = everything())


registry_data <-
  read_csv("https://raw.githubusercontent.com/ebmdatalab/covid19_results_reporting/master/data/registry_data/registry_data_clean.csv") %>%

  select(
    trialid = trial_id,
    trial_status,
    pcd,
    scd,
    rcd = relevant_comp_date,
    tabular_results,
    potential_other_results
  ) %>%

  # Deduplicate trialid
  distinct(trialid, .keep_all = TRUE) %>%

  mutate(registry_scraped = TRUE)

manual_trials <-
  read_csv(latest("manual-trials.csv", here("data","raw"))) %>%

  # Drop `source_register` since same as original ICTRP trials dataset
  select(-source_register)


# Combine datasets --------------------------------------------------------

trials <-

  ictrp_data %>%

  # EUCTR ids in registry dataset do NOT have "EUCTR" prefix or country code, so temporarily edit trialid in ICTRP data for joining
  mutate(trialid_euctr = if_else(
    str_detect(trialid, "EUCTR20\\d{2}\\W*0\\d{5}\\W*\\d{2}"),
    str_extract(trialid, "(?<=EUCTR)(.+)(?=-\\w{2})"),
    trialid
  )) %>%

  # Some (n = 39) ids from registry dataset do not join in. This is because they are cross-registrations and are captured under ids. Registry scraping script has been corrected so future scrapes should not include these duplicates.
  # anti_join(registry_data, ., by = c("trialid" = "trialid_euctr")) %>%

  # Join registry data into trials dataset
  left_join(registry_data, by = c("trialid_euctr" = "trialid")) %>%
  select(-trialid_euctr) %>%

  # Check for duplication between trial id and registry columns
  assert_rows(col_concat, is_uniq, trialid, source_register) %>%

  # N = 1 additional trial (i.e., not in our ICTRP dataset) is in the manual dataset (because we found a result in the auto-results verification), but we do not include in the trial dataset because per protocol we use ICTRP. We do include associated results in the results database.
  # anti_join(manual_trials, trials, by = "trialid") %>%

  left_join(manual_trials, by = "trialid", suffix = c("_auto", "_manual"))

# Add screening criteria --------------------------------------------------

trials <-
  trials %>%

  # Screening based on automated (ICTRP and registries) data
  mutate(
    is_intervention =
      if_else(study_type %in% c("Interventional", "Prevention"), TRUE, FALSE),

    is_reg_2020 =
      if_else(date_registration >= "2020-01-01", TRUE, FALSE),

    is_not_withdrawn =
      if_else(
        # For ChiCTR use variable from ICTRP. Data from other registries doesn't reliably make it to the ICTRP. Withdrawn trials from ClinicalTrials.gov removed from registry_data.
        !str_detect(public_title, "Cancelled|(Retracted due to)") &

          # For ClinicalTrials.gov, use registries dataset
          !trial_status %in% "Withdrawn" &

          # In ICTRP, withdrawn trials may be indicated as recruitment_status
          !recruitment_status %in% "Withdrawn",
        TRUE, FALSE
      ),

    is_eligible_study =
      if_else(
        is_intervention & is_reg_2020 & is_not_withdrawn,
        TRUE, FALSE
      ),

    is_rcd_cutoff_1 =
      if_else(
        rcd_auto < "2020-07-01",
        TRUE, FALSE,
        missing = FALSE
      ),

    has_auto_reg_results_cutoff_1 =
      if_else(
        tabular_results == 1 | potential_other_results == 1,
        TRUE, FALSE
      ),

    is_searched_cutoff_1 =
      if_else(
        is_eligible_study & (is_rcd_cutoff_1 | has_auto_reg_results_cutoff_1),
        TRUE, FALSE
      )
  ) %>%

  # # Screening based on manual trials and results search data
  rename(
    is_clinical_trial_manual = clinical_trial,
    is_covid_manual = covid
  ) %>%
  mutate(
    is_not_withdrawn_manual = !withdrawn,
    is_rcd_cutoff_1_manual = if_else(
      rcd_manual < "2020-07-01",
      TRUE, FALSE
    ),

    is_analysis_pop_cutoff_1 = if_else(
      is_eligible_study & is_rcd_cutoff_1 &
        is_clinical_trial_manual & is_covid_manual & is_not_withdrawn_manual,
      TRUE, FALSE
    )
  ) %>%
  select(-withdrawn)

# TODO: remove  trial registration columns in separate script after cleaning registrations, or save out columns separately
# trialid, source_register, starts_with("cross_reg_")

write_rds(trials, here("data", "processed", "cleaned-trials.rds"))
# write_csv(trials, here("data", "trials.csv"))

# Save out ids and trialids as lookup table for results
# trials %>%
#   select(id, trialid) %>%
#   write_csv(here("data", "trials-ids.csv"))
