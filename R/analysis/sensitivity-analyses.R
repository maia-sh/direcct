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

