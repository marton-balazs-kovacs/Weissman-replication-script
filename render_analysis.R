
library(rmarkdown)

# Main analysis -----------------------------------------------------------

analysis_type <- "main"

render("Weissman-replication-raw-processed.Rmd", params = list(dataset = analysis_type))
render("Weissman-replication-analysis.Rmd", params = list(dataset = analysis_type,download_from_osf = "no"))

# without first trial  -----------------------------------------------------------

analysis_type <- "no_fst"

render("Weissman-replication-raw-processed.Rmd", params = list(dataset = analysis_type))
render("Weissman-replication-analysis.Rmd", params = list(dataset = analysis_type, download_from_osf = "no"))
