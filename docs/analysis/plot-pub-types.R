# Bar plot ----------------------------------------------------------------
# Could add registration status, country, enrollment, etc.

plot_pubs <-

  trials %>%

  # Select trial info to display in plots
  select(id, retrospective_registration, countries) %>%

  # Filter join for trials with results
  right_join(select(results, id, pub_type), by = "id") %>%


  # Tidy countries
  # NOTE: Numbers are so low so does not make sense for cutoff 1
  mutate(

    # Collapse multinational (i.e., with comma)
    countries = if_else(str_detect(countries, ","), "Multinational", countries),

    # Lump infrequent (<5 trials) countries
    # countries =
    #   fct_lump_min(countries, 5, other_level = "Countries with <5 trials"),

    # Order by frequency, with multinational, lumped, and no country at end
    countries = fct_infreq(countries),
    countries = fct_relevel(countries, "Multinational", after = Inf),
    # countries = fct_relevel(countries, "Countries with <5 trials", after = Inf),
    countries = fct_relevel(countries, "No Country Given", after = Inf),
  ) %>%

  # Tidy levels for plots
  mutate(
    pub_type = case_when(
      pub_type == "full_results_preprint" ~ "Full Preprint",
      pub_type == "full_results_journal_article" ~ "Full Journal Article",
      pub_type == "summary_results" ~ "Registry Results",
    ),

    registration =
      if_else(retrospective_registration, "Retrospective", "Prospective")
  ) %>%
  select(-retrospective_registration) %>%


  ggplot(aes(x = pub_type, fill = pub_type)) +
  # ggplot(aes(x = pub_type, fill = countries)) +
  # ggplot(aes(x = pub_type, fill = registration)) +
  # ggplot(aes(fill = pub_type, x = registration)) +

  geom_bar(position = position_dodge(preserve = "single")) +
  # facet_wrap(~countries) +
  # geom_label(aes(label = nrow(pub_type)), y = 20)+
  # xlab("All Publication Types") +
  # ggstatsplot::ggbarstats()
  scale_fill_viridis_d() +
  labs(
    title = "COVID-19 Clinical Trial Results Publication Types",
    # subtitle = "By country and retrospective registration status",
    x = NULL,
    y = NULL
  ) +
  guides(fill = FALSE) +

  ggthemes::theme_fivethirtyeight()

ggsave(plot = plot_pubs, filename = here("docs", "figures", "plot-pubs.png"))


# Upset plot --------------------------------------------------------------

plot_pubs_upset <-
  results %>%
  select(id, pub_type) %>%
  distinct() %>% # Remove multiple of single publication type
  group_by(id) %>%
  mutate(pub_type = case_when(
    pub_type == "full_results_preprint" ~ "Preprint",
    pub_type == "full_results_journal_article" ~ "Journal Article",
    pub_type == "summary_results" ~ "Registry Results"
  )) %>%
  summarise(pub_types = list(pub_type)) %>%
  ggplot(aes(x = pub_types)) +
  geom_bar() +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = -.5) +
  ggupset::scale_x_upset(
    sets = c("Preprint", "Journal Article", "Registry Results")
  ) +
  scale_y_continuous(breaks = NULL, #lim = c(0, 3),
                     name = "") +
  # xlab("Publication Types") +
  labs(
    title = "COVID-19 Clinical Trial Results",
    subtitle = "Intersection of Publication Types"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme_combmatrix(combmatrix.label.make_space = TRUE,
                   combmatrix.label.text = element_text(size=15)
  )

ggsave(plot = plot_pubs_upset, filename = here("docs", "figures", "plot-pubs-upset.png"))


# Venn diagram ------------------------------------------------------------

# Prepare sets for venn diagram
trials_pubtype <-
  results %>%
  select(id, pub_type) %>%
  distinct() %>%
  mutate(pub_type = case_when(
    pub_type == "full_results_preprint" ~ "Preprint",
    pub_type == "full_results_journal_article" ~ "Journal Article",
    pub_type == "summary_results" ~ "Registry Results"
  ))

pub_sets <-
  split(trials_pubtype$id, trials_pubtype$pub_type)

# Save out intersection sets
write_csv(trials_pubtype, here("data", "reporting", "pub-type-sets.csv"))

# Version 1: {ggVennDiagram}
plot_pubs_venn_ggVennDiagram <-
ggVennDiagram::ggVennDiagram(pub_sets,
                             # label_geom = geom_text
                             # label_alpha = 0,
                             # cat.cex = 100
) +
  scale_fill_gradient2()+
  # scale_fill_viridis_c() +
   # scale_fill_gradient(low = "white", high  = "lightblue")+
  # scale_fill_gradient(low = "lightblue", high  = "blue")+
  # scale_fill_brewer() +
  guides(fill = "none" ) +
  labs(
    title = "COVID-19 clinical trial results by publication type"#,
    # subtitle = "Intersection of Publication Types"
  ) +
  # theme_void(base_size = 15) +
  theme(
    text = element_text(size = 12)
  )

ggsave(plot = plot_pubs_venn_ggVennDiagram, filename = here("docs", "figures", "plot-pubs-venn-ggVennDiagram.png"))

# Version 2: {eulerr}
plot_pubs_venn_eulerr <- plot(eulerr::venn(pub_sets))

ggsave(plot = plot_pubs_venn_eulerr, filename = here("docs", "figures", "plot-pubs-venn-eulerr.png"))
