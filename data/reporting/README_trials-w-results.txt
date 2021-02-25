trials-w-results.csv

This dataset includes all trials in the [ICTRP COVID-19 trials dataset](https://www.who.int/clinical-trials-registry-platform) for which we found *any* result.

Each row represents a unique trial which may have multiple registrations and/or results.

The dataset does excludes trials *not* in the ICTRP COVID-19 trials dataset, even if results were found. For example, results for which we found no trial registration number (TRN) or only a TRN not in the ICTRP dataset (e.g., NCT04323527).

The dataset is *not* limited to trials or results included in the DIRECCT analysis population.
- I.e., trials are not limited to trials which passed the DIRECCT automated and manual screening criteria as valid COVID-19 intervention or prevention clinical trials, that are not withdrawn, and completed per the registry by June 30, 2020.
- I.e., results are not limited to full-results as registry summary results, preprints, or journal articles.
- Additional filters can be applied using the inclusion/exclusion criteria columns (prefixed with `is_` or `has_`)

Since we did not systematically search for protocols, protocols may be captured as a `pub_type` "other" or under the `comments`. Additional trials which did not have results (and therefore are not included in this dataset) may have protocols noted under `comments`.

**Codebook**
We are in the process of developing a more detailed codebook, so the following is a summary.
`id`: unique trial id within our database. Please keep this id column!
columns between `id` and `trn_1`: trial information, including from ICTRP, individual registry indicated in `source_register`, automated and manual screening, and inclusion/exclusion criteria (prefixed with `is_` or `has_`)
`trn_#` and `registry_#`: trial registration number and registry, suffixed by number
`doi_#`, `pmid_#`, `cord_id_#`, `data_publication_#`, `date_completion_#`, `comments_results_#`: results publication information, suffixed by number