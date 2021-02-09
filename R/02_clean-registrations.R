# Prior to manual search, we deduplicated known cross-registrations (in previous Python steps based on ND TrialsTracker). Since we found additional cross-registrations through the search, we deduplicate again.

source(here::here("R", "environment.R"), echo = FALSE)

# Tidy registrations ------------------------------------------------------
# trials <- read_csv(here("data", "trials.csv"))
trials <- read_rds(here("data", "processed", "cleaned-trials.rds"))

# Get registrations from manual search, plus "primary"
registrations <-
  trials %>%
  select(id, trialid, source_register, starts_with("cross_reg_")) %>%

  # Rename "primary" trial id and registry
  rename(cross_reg_0 = trialid, cross_reg_registry_0 = source_register) %>%
  mutate(cross_reg_source_0 = "ictrp", .after = id) %>%

  # Currently one trial per row, so pivot to get one registriation per row
  pivot_longer(starts_with("cross_reg"),
               names_to = "name",
               values_to = "value",
               values_drop_na = TRUE
  ) %>%
  mutate(name = str_remove(name, "cross_reg_")) %>%
  separate(name, into = c("type", "n_trn"), sep = "_", fill = "left", convert = TRUE) %>%
  mutate(type = replace_na(type, "trn")) %>%

  # Number of trialid should start at 1
  mutate(n_trn = n_trn + 1) %>%

  # Pivot wider so that we have 1 row per trial registration
  pivot_wider(id_cols = c(id, n_trn), names_from = "type", values_from = "value") %>%

  # EUCTR ids in `cross_registrations` from input dataset do NOT have country code, so remove for joining
  mutate(trn = if_else(
    str_detect(trn, "EUCTR20\\d{2}\\W*0\\d{5}\\W*\\d{2}"),
    str_remove(trn, "-[A-Z]{2}"),
    trn
  ))

# Get additional cross-registrations from input dataset
ictrp_cross_reg <-
  trials %>%
  select(id, cross_registrations) %>%
  filter(cross_registrations != "None") %>%

  # Split cross-registration list column
  # TODO: this is not the way to split this column...
  mutate(cross_registrations = str_split(cross_registrations, "; ")) %>%
  unnest_wider(cross_registrations) %>%
  rename_with(.cols = starts_with("..."),
              ~ str_replace(.x, "...", "ictrp_")
  ) %>%

  # Pivot longer so that we have 1 row per trial registration
  pivot_longer(starts_with("ictrp"),
               names_to = c("source", "n_trn"), names_sep = "_",
               values_to = "trn", values_drop_na = TRUE#,
               # values_transform = list(n_trn = as.numeric)
  ) %>%
  mutate(n_trn = as.numeric(n_trn)) #%>%
  # mutate(registry = ctregistries::which_registries(trn))

# Combine various registrations, removing duplicates (i.e., if a registration is in both the manual and input dataset)
# registrations <-
registrations <-
  ictrp_cross_reg %>%

  # Combine registrations, putting original `cross_registrations` at bottom
  bind_rows(registrations, .) %>%

  # Remove duplicate registrations for the same trial, same registry
  distinct(id, trn, .keep_all = TRUE) %>%

  # Renumber TRNs (since current numbers don't account for various sources)
  # TODO: If ND has order preference, arrange() here
  group_by(id) %>%
  mutate(n_trn = row_number()) %>%
  ungroup() %>%

  # Update all registries based on TRN
  # There are discrepancies between ICTRP and ctregistries (e.g., ICTRP uses JPRN only, where as ctregistries distinguishes registries)
  mutate(registry = ctregistries::which_registries(trn))

# NOTE: could add registration unique id

write_rds(registrations, here("data", "processed", "cleaned-registrations.rds"))
# write_csv(registrations, here("data", "registrations.csv"))
