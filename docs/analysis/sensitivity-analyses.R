# Sensitivity analysis: latest completion date ----------------------------

# If manual registry CD or publication CD (regardless of whether included publication) is later than auto CD, use that later date.

# Get the latest completion date, indicating source based on auto > manual > publication
# Note: using latest completion date we will have some funkiness like reporting before completion, e.g. tri02934 (NCT04316377) which has a verified completion date in 2025
#
# Alternative: publication > manual (coalesce with wide date data)

all_pub_comp_dates <-
  all_results %>% # or limit to results in analysis?
  filter(!is.na(date_completion)) %>%
  select(id, date_completion) %>%
  group_by(id) %>%
  # mutate(source = row_number(),
  #        source = glue::glue("publication_{source}")
  # ) %>%
  mutate(source = "publication") %>%
  ungroup()

# Get the latest completion dates
latest_comp_dates <-

  # Use only trials included in main analysis (some more will be removed)
  trials %>%
  select(id, auto = rcd_auto, manual = rcd_manual) %>%
  pivot_longer(c(auto, manual),
               names_to = "source",
               values_to = "date_completion"
  ) %>%
  bind_rows(all_pub_comp_dates) %>%
  group_by(id) %>%
  mutate(
    latest_date_completion = max(date_completion),
    source = factor(source,
                    levels = c("auto", "manual", "publication"
                               # "publication_1", "publication_2"
                    )
    )
  ) %>%
  filter(date_completion == latest_date_completion) %>%
  select(-date_completion) %>%
  arrange(source, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

# Report on latest completion dates
# latest_comp_dates %>%
#   count(source)
n_latest_cd_pub <-
  latest_comp_dates %>%
  filter(source == "publication") %>%
  nrow()

n_latest_cd_manual <-
  latest_comp_dates %>%
  filter(source == "manual") %>%
  nrow()

sa_trials_complete <-
  latest_comp_dates %>%
  filter(latest_date_completion <= "2020-06-30")

n_sa_trials_complete <- nrow(sa_trials_complete)

sa_results_complete <-
  results %>%
  semi_join(sa_trials_complete, by = "id") #%>%
  # filter(is_pub_date_cutoff_1 & is_pub_main)

n_sa_trials_complete_results <-
  sa_results_complete %>%
  distinct(id) %>%
  nrow()

p_sa_trials_complete_results <- n_sa_trials_complete_results/n_sa_trials_complete


# Sensitivity analysis: study completion date -----------------------------

# Use study completion date, regardless of whether primary completion date is available
sa_trials_scd <-
  trials %>%
  mutate(
    is_scd_cutoff_1 =
      if_else(
        scd_auto < "2020-07-01",
        TRUE, FALSE,
        missing = FALSE
      )
  ) %>%
  # count(is_rcd_cutoff_1, is_scd_cutoff_1)
  # count(is_rcd_cutoff_1 != is_scd_cutoff_1)
  filter(is_scd_cutoff_1)

n_sa_trials_scd <- nrow(sa_trials_scd)

sa_results_scd <-
  results %>%
  semi_join(sa_trials_scd, by = "id")

n_sa_trials_scd_results <-
  sa_results_scd %>%
  distinct(id) %>%
  nrow()

p_sa_trials_scd_results <- n_sa_trials_scd_results/n_sa_trials_scd

  # # Apply other filter criteria
  # filter(is_reg_2020 &              # Must have 2020+ registration (auto)
  #          is_intervention &          # Must be intervention/prevention (auto)
  #          is_not_withdrawn &         # Must not be withdrawn (auto)
  #          # is_rcd_cutoff_1 &          # Must be complete by cutoff (auto)
  #          is_clinical_trial_manual & # Must be clinical trial (manual)
  #          is_covid_manual &          # Must be covid intervention/prevention (manual)
  #          is_not_withdrawn_manual &  # Must not be withdrawn (manual)
  #          is.na(is_dupe)
  # ) %>%


# Sensitivity analysis: interim results -----------------------------------

sa_results_w_interim <-
  all_results %>%
  filter(is_trial_included & is_pub_date_cutoff_1 &
           str_detect(pub_type, "other", negate = TRUE)
  )

n_sa_results_interim <-
  sa_results_w_interim %>%
  filter(str_detect(pub_type, "interim")) %>%
  nrow()

n_sa_trials_results_interim <-
  sa_results_w_interim %>%
  distinct(id) %>%
  nrow()

p_sa_trials_results_interim <- n_sa_trials_results_interim/n_not_dupe


# Sensitivity analysis: 3 month follow-up ---------------------------------
# Minimum 3-month follow-up

sa_results_3_mo <-
  all_results %>%
  filter(is_trial_included & is_pub_main & date_publication <= "2020-10-01")

n_sa_results_3_mo <- nrow(sa_results_3_mo)

n_sa_trials_results_3_mo <-
  sa_results_3_mo %>%
  distinct(id) %>%
  nrow()

p_sa_trials_results_3_mo <- n_sa_trials_results_3_mo/n_not_dupe


# Additional analysis: additional results ---------------------------------
# Analyze results excluded from per protocol analysis

all_results_for_screening <-
  all_results %>%

  # Join in rcd_auto
  left_join(select(all_trials, id, rcd_auto), by = "id") %>%

  mutate(

    # Is result included in per protocol analysis?
    in_pp_analysis =
      if_else(is_trial_included & is_pub_date_cutoff_1 & is_pub_main, TRUE, FALSE),
    not_in_pp_analysis =
      if_else(!in_pp_analysis | is.na(in_pp_analysis), TRUE, FALSE),

    # Does result have a trn?
    has_trn = if_else(trn != "no TRN found", TRUE, FALSE),

    # Does result have a late/missing completion date?
    has_cd = if_else(!is.na(rcd_auto), TRUE, FALSE),
    has_cd_pp = if_else(rcd_auto <= "2020-06-30", TRUE, FALSE),

    # Does result have a late/missing publication date?
    has_pub_date = if_else(!is.na(date_publication), TRUE, FALSE),
    has_pub_date_pp = if_else(date_publication <= "2020-08-15", TRUE, FALSE)
  )

results_criteria <- c(
  "is_pub_main",
  "not_in_pp_analysis",
  "has_trn",
  "has_cd",
  "has_cd_pp",
  "has_pub_date",
  "has_pub_date_pp"
)

screened_results <-
  all_results_for_screening %>%
  count_filter(results_criteria)

unexplained_results <- screened_results$data

results_screening_counts <-
  screened_results$counts %>%
  add_row(name = "all_results", value = TRUE, n = nrow(all_results), .before = 1)

n_full_results <- report_n(results_screening_counts, "is_pub_main", TRUE)
n_incl_pp_results <- report_n(results_screening_counts, "not_in_pp_analysis", FALSE)
n_excl_pp_results <- report_n(results_screening_counts, "not_in_pp_analysis", TRUE)
n_no_trn <- report_n(results_screening_counts, "has_trn", FALSE)
n_excl_cd_none <- report_n(results_screening_counts, "has_cd", FALSE)
n_excl_cd_late <- report_n(results_screening_counts, "has_cd_pp", FALSE)
n_excl_pub_date_none <- report_n(results_screening_counts, "has_pub_date", FALSE)
n_excl_pub_date_late <- report_n(results_screening_counts, "has_pub_date_pp", FALSE)
n_excl_funky <- report_n(results_screening_counts, "has_pub_date_pp", TRUE)
# tri00042 excluded based on publication which indicated retrospective study

rm(all_results_for_screening, results_criteria, screened_results)


# Additional analysis: cross-registered trials ----------------------------
# Only primary registries were scraped and completion date from this registration was used for the inclusion criteria. Cross-registered trials may have a different completion date that may have changed the inclusion status.
# Get cross-registrations for all trials which were excluded based on completion date.

cd_excl_registrations <-
  all_trials %>%

  # Limit to eligible trials with completion date AFTER cutoff
  filter(is_eligible_study & !is_rcd_cutoff_1) %>%

  semi_join(registrations, ., by = "id")

n_cd_excl_cross_registations <-
  cd_excl_registrations %>%
  filter(n_trn > 1) %>%
  nrow()

n_cd_excl_trials_w_cross_registations <-
  cd_excl_registrations %>%
  filter(n_trn > 1) %>%
  distinct(id) %>%
  nrow()

write_csv(cd_excl_registrations, here("data", "reporting", "cd-excluded-registrations.csv"))
