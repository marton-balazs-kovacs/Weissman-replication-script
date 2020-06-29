
library(rmarkdown)

download_from_osf = "no"
datasets <- c("main", "no_fst", "no_scnd", "both")
combined <- list()
save_passes <- "multiverse/pass"

for (d in datasets) {
  if (is.character(save_passes))
    tmp <- paste0(save_passes, "_", d, ".html")
  else
    tmp <- tempfile()
  render("Weissman-replication-raw-processed.Rmd", 
         params = list(dataset = d, download_from_osf = download_from_osf), 
         output_file = tmp)
  # Never download Processed data from the OSF because we just processed a fresh copy!
  render("Weissman-replication-analysis.Rmd", 
         params = list(dataset = d, download_from_osf = "no"), 
         output_file = tmp)
  # Save each of the analysis dataframes into a dataframe with the dataset as a column
  for (x in ls()) {
    if (typeof(get(x)) == "list" && 
        !(x %in% c("combined", "params", "datasets"))) {
      df <- get(x) 
      df$dataset <- d
      combined[[x]] <- bind_rows(combined[[x]], df)
    }
  }
}

# Clean the environment
rm(list = ls()[!ls() %in% "combined"])

for (x in names(combined)) {
  print(x)
  print(combined[[x]])
}
