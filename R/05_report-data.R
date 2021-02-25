source(here::here("R", "environment.R"), echo = FALSE)

# Prep data for reporting
# TODO: Create/generate codebook

# Get deduped data --------------------------------------------------------

trials <- read_rds(here("data", "processed", "deduped-trials.rds"))
registrations <- read_rds(here("data", "processed", "deduped-registrations.rds"))
results <- read_rds(here("data", "processed", "deduped-results.rds"))

# Save data as csvs -------------------------------------------------------

write_csv(trials, here("data", "reporting", "trials.csv"))
write_csv(registrations, here("data", "reporting", "registrations.csv"))
write_csv(results, here("data", "reporting", "results.csv"))
