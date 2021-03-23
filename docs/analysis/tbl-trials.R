# possible edits:
# add n_reg?
# show all registrations (including cross) and cross-reg as binary?

cross_registrations <-
  registrations %>%
  semi_join(trials, by = "id") %>%
  group_by(id) %>%

  mutate(
    n_reg = n(),
    cross_registry = if_else(n_reg == 1, registry, "Cross-registered")
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(id, cross_registry) %>%
  ungroup() %>%

  mutate(

    # Lump infrequent (<3 trials) registries
    cross_registry =
      fct_lump_min(cross_registry, 3, other_level = "Registries with <3 trials"),

    # Order by frequency, with cross-registrations first, and lumped last
    cross_registry =
      fct_infreq(cross_registry),
    cross_registry =
      fct_relevel(cross_registry, "Cross-registered", after = 0),
    cross_registry =
      fct_relevel(cross_registry, "Registries with <3 trials", after = Inf)
  ) #%>%
  # right_join(trials, by = "id")

tbl_trials <-
  trials %>%

  mutate(
    has_result = if_else(id %in% unique(results$id), "With Results", "Without Results"),

    # Collapse multinational (i.e., with comma)
    countries =
      if_else(str_detect(countries, ","), "Multinational", countries),

    # Lump infrequent (<5 trials) countries
    countries =
      fct_lump_min(countries, 5, other_level = "Countries with <5 trials"),

    # Order by frequency, with multinational, lumped, and no country at end
    countries = fct_infreq(countries),
    countries = fct_relevel(countries, "Multinational", after = Inf),
    countries = fct_relevel(countries, "Countries with <5 trials", after = Inf),
    countries = fct_relevel(countries, "No Country Given", after = Inf),
  ) %>%

  # Group countries with fewer than 4 trials (overall)
  # add_count() %>%
  # group_by(countries) %>%
  # mutate(
  #   n_countries = n(),
  #   countries = if_else(n_countries <= 4, "Countries w/ <5 trials", countries)
  # ) %>%

  # Join in registrations
  left_join(cross_registrations, by = "id") %>%

  select(has_result,
         target_enrollment,
         # study_type,
         phase,
         cross_registry,
         countries
  ) %>%

  gtsummary::tbl_summary(
    by = has_result,
    # type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{N_nonmiss}",
    #                                  "{median} ({p25}, {p75})",
    #                                  "{min}, {max}"),
    missing = "no",
    label = list(
      target_enrollment ~ "Target enrollment",
      # study_type ~ "Study type",
      phase ~ "Phase",
      cross_registry ~ "Registry",
      countries ~ "Countries"
    ),
    # sort = list(
      # cross_registry ~ "frequency",
      # countries ~ "frequency"
    # ),
  ) %>%

  add_overall() %>%

  # add_difference(include = target_enrollment) %>%

  # modify_header(label = "**Publication Type**") %>%

  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Results Disseminated**") %>%
  modify_caption("**Trial Characteristics** (N = {N})") #%>%
#remove_row_type(study_type, type = "header")

# show_header_names(tbl)
#
gt::gtsave(as_gt(tbl_trials), here("docs", "figures", "tbl-trials.png"))
