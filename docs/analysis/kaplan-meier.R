# Prepare completion dates ------------------------------------------------
# TODO: add pp_cd to trials
# Use the latest publication completion date, if available, else registry rcd

pub_comp_dates <-
  results %>% # limit to results in analysis
  filter(!is.na(date_completion)) %>%
  select(id, date_completion) %>%

  # Select the latest completion date reported for each publication
  group_by(id) %>%
  arrange(desc(date_completion), .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

# Number of trials with completion dates in publication
n_trials_pub_cd <- nrow(pub_comp_dates)


# Prepare publication dates -----------------------------------------------
# Use the earliest publication date for each trial, for each pub type
# Disregard whether earliest publication date from same pub as latest completion date

results_pub_dates <-
  results %>%
  select(id, date_publication_any = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_any, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

article_pub_dates <-
  results %>%
  filter(pub_type == "full_results_journal_article") %>%
  select(id, date_publication_article = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_article, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

preprint_pub_dates <-
  results %>%
  filter(pub_type == "full_results_preprint") %>%
  select(id, date_publication_preprint = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_preprint, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

summary_pub_dates <-
  results %>%
  filter(pub_type == "summary_results") %>%
  select(id, date_publication_summary = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_summary, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

# Add interim results for sensitivity analysis
results_w_interim_pub_dates <-
  all_results %>%
  filter(is_trial_included & is_pub_date_cutoff_1 &
           str_detect(pub_type, "other", negate = TRUE)
  ) %>%
  select(id, date_publication_any_w_interim = date_publication) %>%
  group_by(id) %>%
  arrange(date_publication_any_w_interim, .by_group = TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  ungroup()

pub_dates <-
  results_pub_dates %>%
  full_join(article_pub_dates, by = "id") %>%
  full_join(preprint_pub_dates, by = "id") %>%
  full_join(summary_pub_dates, by = "id") %>%
  full_join(results_w_interim_pub_dates, by = "id")

rm(results_pub_dates, article_pub_dates, preprint_pub_dates, summary_pub_dates, results_w_interim_pub_dates)

# Prepare kaplan-meier data -----------------------------------------------

km_data <-
  trials %>%
  select(id, date_completion = rcd_auto) %>%
  rows_update(pub_comp_dates, by = "id") %>%
  left_join(pub_dates, by = "id") %>%
  mutate(

    date_cutoff = lubridate::ymd("2020-08-15"),

    # Create binaries and times for KM analysis
    publication_any = if_else(!is.na(date_publication_any), TRUE, FALSE),
    publication_article = if_else(!is.na(date_publication_article), TRUE, FALSE),
    publication_preprint = if_else(!is.na(date_publication_preprint), TRUE, FALSE),
    publication_summary = if_else(!is.na(date_publication_summary), TRUE, FALSE),
    publication_any_w_interim = if_else(!is.na(date_publication_any_w_interim), TRUE, FALSE),

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
      ),
    time_publication_any_w_interim =
      if_else(publication_any,
              lubridate::as.duration(date_completion %--% date_publication_any_w_interim)/ ddays(1),
              lubridate::as.duration(date_completion %--% date_cutoff)/ ddays(1)
      )
  )

write_csv(km_data, here("data", "reporting", "kaplan-meier-time-to-pub.csv"))

# Do kaplan-meier analyses ------------------------------------------------
# summary(km_fit, times = 100)
# ci_plot$plot +
#   scale_y_reverse()
# library(patchwork)
# km_plot_any + km_plot_article
# Note: not working with pipe


# PUBLICATION ANY

# Trials with PD before CD (negative time)
n_any_pre <- nrow(filter(km_data, time_publication_any < 0))

# Create KM fit
km_fit_any <-
  survfit(Surv(time_publication_any, publication_any) ~ 1,

          # Remove trials with PD before CD (negative time) (n = 8)
          data = filter(km_data, time_publication_any >= 0)
  )

# Plot cumulative incidence
km_plot_any <-
  ggsurvplot(
    fit = km_fit_any,
    conf.int = TRUE,
    risk.table = TRUE,
    cumevents = TRUE,
    cumcensor = TRUE,
    fun = "event", #"pct"
    xlab = "Days",
    ylab = "Cumulative events" #"Overall survival probability"
  )

# PUBLICATION ARTICLE

# Trials with PD before CD (negative time)
n_article_pre <- nrow(filter(km_data, time_publication_article < 0))

# Create KM fit
km_fit_article <-
  survfit(Surv(time_publication_article, publication_article) ~ 1,

          # Remove trials with PD before CD (negative time) (n = 8)
          data = filter(km_data, time_publication_article >= 0)
  )

# Plot cumulative incidence
km_plot_article <-
  ggsurvplot(
    fit = km_fit_article,
    conf.int = TRUE,
    risk.table = TRUE,
    cumevents = TRUE,
    cumcensor = TRUE,
    fun = "event", #"pct"
    xlab = "Days",
    ylab = "Cumulative events" #"Overall survival probability"
  )


# PUBLICATION PREPRINT

# Trials with PD before CD (negative time)
n_preprint_pre <- nrow(filter(km_data, time_publication_preprint < 0))

# Create KM fit
km_fit_preprint <-
  survfit(Surv(time_publication_preprint, publication_preprint) ~ 1,

          # Remove trials with PD before CD (negative time) (n = 8)
          data = filter(km_data, time_publication_preprint >= 0)
  )

# Plot cumulative incidence
km_plot_preprint <-
  ggsurvplot(
    fit = km_fit_preprint,
    conf.int = TRUE,
    risk.table = TRUE,
    cumevents = TRUE,
    cumcensor = TRUE,
    fun = "event", #"pct"
    xlab = "Days",
    ylab = "Cumulative events" #"Overall survival probability"
  )


# PUBLICATION SUMMARY RESULTS

# Trials with PD before CD (negative time)
n_summary_pre <- nrow(filter(km_data, time_publication_summary < 0))

# Create KM fit
km_fit_summary <-
  survfit(Surv(time_publication_summary, publication_summary) ~ 1,

          # Remove trials with PD before CD (negative time) (n = 8)
          data = filter(km_data, time_publication_summary >= 0)
  )

# Plot cumulative incidence
km_plot_summary <-
  ggsurvplot(
    fit = km_fit_summary,
    conf.int = TRUE,
    risk.table = TRUE,
    cumevents = TRUE,
    cumcensor = TRUE,
    fun = "event", #"pct"
    xlab = "Days",
    ylab = "Cumulative events" #"Overall survival probability"
  )


# Do additional time-to-pub analyses --------------------------------------

# Time to summary results
n_days_summary <-
  km_data %>%
  filter(publication_summary) %>%
  pull(time_publication_summary) %>%
  sort() %>%
  glue_collapse(sep = ", ", last = " and ")

summary_results_registry <-
  km_data %>%
  filter(publication_summary) %>%
  semi_join(registrations, ., by = "id") %>%
  distinct(registry) %>%
  pull(registry) %>%
  glue_collapse(sep = ", ", last = " and ")

# Trials with both preprint and article
preprint_article <-
  km_data %>%
  filter(publication_article & publication_preprint) %>%
  mutate(
    preprint_to_article = lubridate::as.duration(date_publication_preprint %--% date_publication_article)/ ddays(1)
  )

n_preprint_article <- nrow(preprint_article)
median_preprint_article <- median(preprint_article$preprint_to_article)
iqr_preprint_article <- IQR(preprint_article$preprint_to_article)
q1_preprint_article <- median_preprint_article - iqr_preprint_article/2
q3_preprint_article <- median_preprint_article + iqr_preprint_article/2
