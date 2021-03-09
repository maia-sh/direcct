# Sensitivity analysis KM: 3 months follow-up -----------------------------

results_3_mo <-
  all_results %>%
  filter(is_trial_included & is_pub_main & date_publication <= "2020-10-01")

pub_comp_dates_3_mo <-
  results_3_mo %>% # limit to results in analysis
  filter(!is.na(date_completion)) %>%
  select(id, date_completion) %>%

  # Select the latest completion date reported for each publication
  group_by(id) %>%
  arrange(desc(date_completion), .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

# Number of trials with completion dates in publication
n_trials_pub_cd_3_mo <- nrow(pub_comp_dates_3_mo)

# Prepare publication dates -----------------------------------------------
# Use the earliest publication date for each trial, for each pub type
# Disregard whether earliest publication date from same pub as latest completion date

results_pub_dates <-
  results_3_mo %>%
  select(id, date_publication_any = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_any, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

article_pub_dates <-
  results_3_mo %>%
  filter(pub_type == "full_results_journal_article") %>%
  select(id, date_publication_article = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_article, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

preprint_pub_dates <-
  results_3_mo %>%
  filter(pub_type == "full_results_preprint") %>%
  select(id, date_publication_preprint = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_preprint, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

summary_pub_dates <-
  results_3_mo %>%
  filter(pub_type == "summary_results") %>%
  select(id, date_publication_summary = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_summary, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

pub_dates_3_mo <-
  results_pub_dates %>%
  full_join(article_pub_dates, by = "id") %>%
  full_join(preprint_pub_dates, by = "id") %>%
  full_join(summary_pub_dates, by = "id") %>%
  full_join(results_w_interim_pub_dates, by = "id")

rm(results_pub_dates, article_pub_dates, preprint_pub_dates, summary_pub_dates)

# Prepare kaplan-meier data -----------------------------------------------

km_data_3_mo <-
  trials %>%
  select(id, date_completion = rcd_auto) %>%
  rows_update(pub_comp_dates_3_mo, by = "id") %>%
  left_join(pub_dates_3_mo, by = "id") %>%
  mutate(

    date_cutoff = lubridate::ymd("2020-08-15"),

    # Create binaries and times for KM analysis
    publication_any = if_else(!is.na(date_publication_any), TRUE, FALSE),
    publication_article = if_else(!is.na(date_publication_article), TRUE, FALSE),
    publication_preprint = if_else(!is.na(date_publication_preprint), TRUE, FALSE),
    publication_summary = if_else(!is.na(date_publication_summary), TRUE, FALSE),

    time_publication_any =
      if_else(publication_any,
              lubridate::as.duration(date_completion %--% date_publication_any)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      ),
    time_publication_article =
      if_else(publication_article,
              lubridate::as.duration(date_completion %--% date_publication_article)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      ),
    time_publication_preprint =
      if_else(publication_preprint,
              lubridate::as.duration(date_completion %--% date_publication_preprint)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      ),
    time_publication_summary =
      if_else(publication_summary,
              lubridate::as.duration(date_completion %--% date_publication_summary)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      )
  )

write_csv(km_data, here("data", "reporting", "kaplan-meier-time-to-pub_3-mo.csv"))
