# Install and load packages

cran_pkgs <- c("dplyr", "tidyr", "stringr", "glue", "purrr", "forcats", "readr", "here",
               # "tidylog",
               "rlang",
               "ggplot2", "ggupset",
               "janitor", "writexl",
               "fs",
               "gt",
               "knitr",
               #"irr",
               # "viridis",
               "see",
               "assertr"
)
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

# non_cran_pkgs <- "mashr"
#
# if (!"mashr" %in% installed.packages()) {
#   # devtools::install_github("cstubben/tidypubmed")
#   devtools::install_github("maia-sh/mashr")
#   devtools::install_github("maia-sh/ctregistries")
# }

suppressMessages(invisible(lapply(c(cran_pkgs#, non_cran_pkgs
                                    ), library, character.only = TRUE)))

# credit: Wil Doane
latest <- function (name, dir = here::here("data"), hash = FALSE) {
  pattern <- sprintf("^20[0-9\\-]+_%s", name)
  filenames <- list.files(dir, pattern, full.names = TRUE)
  result <- sort(filenames, decreasing = TRUE)[1]
  if (is.na(result))
    NULL
  else {
    if (hash)
      message(digest::sha1(readLines(result)))
    result
  }
}
