
# Main analysis -----------------------------------------------------------

analysis_type <- "main"

rmarkdown::render("Weissman-replication-raw-processed.Rmd", params = list(dataset = "main"))
rmarkdown::render("Weissman-replication-analysis.Rmd", params = list(dataset = "main",download_from_osf = "no"))

# withouht first trial

analysis_type <- "no_fst"

rmarkdown::render("Weissman-replication-raw-processed.Rmd", params = list(dataset = "main"))
rmarkdown::render("Weissman-replication-analysis.Rmd", params = list(dataset = "main",download_from_osf = "no"))
