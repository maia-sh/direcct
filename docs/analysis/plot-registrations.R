# Create a dataframe of all registrations for included trials, plus whether they had a result (any pub_type)

# trials_w_any_result <-
#   results %>%
#   distinct(id) %>%
#   pull()

registrations_results <-
  registrations %>%

  # Limit registrations to included trials
  semi_join(trials, by = "id") %>%

  select(id, trn, registry) %>%

  # Add in results info
  mutate(has_result = if_else(id %in% unique(results$id), TRUE, FALSE)) %>%

  # Get number of trials by registry
  add_count(registry, name = "n_reg") %>%

  # Get number of trials with/without results by registry
  add_count(registry, has_result, name = "n_by_result") %>%

  # Get proportion of trials with results by registry
  # mutate(prop_by_result = n_by_result/n_reg) %>%
  # mutate(name = has_result) %>%
  # pivot_wider(names_from = name, names_prefix = "prop_results_", values_from = prop_by_result) %>%
  # mutate(prop_results = if_else(is.na(prop_results_TRUE), 1 - prop_results_FALSE, prop_results_TRUE), .keep = "unused")

  # Get number and proportion of trials with results by registry
  mutate(prop_by_result = n_by_result/n_reg) %>%
  mutate(name = has_result) %>%
  pivot_wider(names_from = name,
              # names_prefix = c("n_results_", "prop_results_"),
              values_from = c(n_by_result, prop_by_result)) %>%
  mutate(
    n_results = if_else(is.na(n_by_result_TRUE), n_reg - n_by_result_FALSE, n_by_result_TRUE),
    prop_results = if_else(is.na(prop_by_result_TRUE), 1 - prop_by_result_FALSE, prop_by_result_TRUE)#,
    # .keep = "unused"
  )

reg_results_summary <-
  registrations_results %>%
  distinct(registry, .keep_all = TRUE) %>%
  select(registry, n_reg, n_results, prop_results)

# Colors
darkred <- "#851123"
lightgray <- "#B7B7B7"

# Plot reporting rate by registry
plot_registrations_results <-
  registrations_results %>%
  ggplot(aes(x = reorder(registry, n_reg))) +
  geom_bar(
    aes(fill = has_result),
    position = "stack",
    color = lightgray) +

  # Use geom_text with summary dataframe to remove overprinting
  # geom_text(
  #   aes(y = n_reg,
  #       # label = scales::percent(prop_results)
  #       label = glue("{scales::percent(prop_results)}({n_results}/{n_reg})")
  #       ),
  #   nudge_y = 5
  # ) +

  geom_text(
    data = reg_results_summary,
    aes(x = reorder(registry, n_reg),
        y = n_reg,
        label = glue("{scales::percent(prop_results, accuracy = 0.1)} ({n_results}/{n_reg})")),
    nudge_y = 8.5,
    size = rel(4.1)
  ) +

  coord_flip() +
  # scale_fill_grey(start = 0.7, end = 0) +
  scale_fill_manual(values = c("TRUE" = darkred, "FALSE" =  lightgray)) +
  guides(fill = "none") +
  labs(
    title = "Result reporting rate per clinical trial registry",
    cpation = "",
    # caption = "Each registration is counted. If a trial is cross-registered in more than one registry, it is counted for each registration.",
    # y = "Number of registrations with results reported of total registrations", # Number of registrations (results reported/total)
    # y = glue("Number of",
    #          " <span style='color:{darkred}'>registrations with results reported</span> ",
    #          "of",
    #          " <span style='color:{lightgray}'>total registrations</span> "
    # ),
    y = NULL,
    x = NULL
  ) +
  # theme(
  #   axis.title.y = element_markdown(),
  # ) +
geom_richtext(x = "CRiS",
              y = 75,
              vjust = 1.4,
              # size = 16,
              size = rel(4.7),
              label.color = NA,
              label = glue("Number of",
                           " <span style='color:{darkred}'>registrations with results reported</span> ",
                           "of",
                           " <span style='color:{lightgray}'>total registrations</span> "
              )
) +
theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = rel(.9))
    # panel.spacing.y = unit(100, "lines")
    # axis.ticks.y = element_blank()
    # axis.text.y = element_text(margin = margin(r = -10))
  )


ggsave(plot = plot_registrations_results, filename = here("docs", "figures", "plot-registrations-results.png"), scale = 1.18)

# Note: issue with ggtext on axis so using textbox
# https://wilkelab.org/ggtext/
# https://www.infoworld.com/article/3527449/add-color-to-your-ggplot2-text-in-r.html
# https://plotnine.readthedocs.io/en/stable/tutorials/miscellaneous-show-counts-and-percentages-for-bar-plots.html
