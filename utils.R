#' Download all files from osf directory 
#' This function is intended to run inside dplyr do() verb. Downloads data locally
#'
#' @param df row with directory on osf repository
#' @param local_data_pth Path where the data should be saved
#' @param should_overwrite whether the data should be overwritten. Default is True
#'
#' @return
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth)
#' data_files <- 
#'   weissman_project %>% 
#'   osf_ls_files() %>% 
#'   filter(name == "Source") %>%
#'   osf_ls_files() 
#'   
#'   data_files %>% 
#'   group_by(name) %>% # for each experiment type
#'     do(download_files(.,local_data_pth))
download_files <- function(df, local_data_pth, should_overwrite = T) {
  # we need to set correct class as the current version of osfr does not works with dplyr properly
  class(df) <- c("osf_tbl_file","osf_tbl", class(df)) 
  stopifnot(nrow(df) == 1)
  df %>% 
    osf_ls_files() %>% 
    rowwise() %>% 
    do(osf_retrieve_file(.$id) %>% 
         osf_download(path = file.path(local_data_pth, df$name,.$name), 
                      overwrite = should_overwrite))
}

#' Create local file structure
#' As we have four experiments, we create individual directory for each of them
#' @param pth path, in which the file structure will be created
#'
#' @return nothing, just the side effects
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth) 
#'
create_local_structure <- function(pth) {
  # simple function for test-and-create behavior
  verbose_create <- function(f) {
    if(!dir.exists(f)) {
      message(sprintf("Directory %s does not exist, creating..", f))
      dir.create(f, recursive = T)  
    }
  }
  
  flanker_dir    <- file.path(pth,"flanker")
  primeprobe_dir <- file.path(pth,"primeprobe")
  stroop_dir     <- file.path(pth,"stroop")
  simon_dir      <- file.path(pth,"simon")
  
  verbose_create(flanker_dir)
  verbose_create(primeprobe_dir)
  verbose_create(stroop_dir)
  verbose_create(simon_dir)
}

#' Removes local data 
#' Just a simple wrapper around unlink function
#'
#' @param local_data_pth path to downloaded data
#'
#' @return
#' @export
#'
#' @examples
#' local_data_pth <- file.path("data","Source")
#' create_local_structure(local_data_pth)
#' remove_local_data(local_data_pth)
remove_local_data <- function(local_data_pth) {
  unlink(local_data_pth, recursive = T)
}

#' Reads in and merge local csv files
#' The function reads in and merge individual locale csv files
#' into one tibble from different subdirectories. The function also
#' saves directory, subdirectory and file names as a variable.
#' 
#' @param pattern The pattern to look for when listing files in the locale directory.
#' @param path The path to the locale directory that contains the subdirectories.
#' @param subfolder_name A character vector of the subfolder names to look into.
#' @param exclude A character string. Filenames containing this string will not be read in.
#' @param sep Used as delim in read_delim.
#' 
#' @return All files that meets the criteria merged into a tibble from the specified subdirectory.
read_plus <- function(pattern, path, subfolder_name, exclude = NULL, sep) {
  files <- tibble(list = list.files(path = paste0(path, subfolder_name), pattern = pattern, full.names = T, recursive = T))
  
  if(!is.null(exclude)){
  files <- filter(files, !str_detect(list, exclude))
  }
  
  files %>%
  pull(list) %>% 
  map_dfr(.,
          ~ read_delim(.x, delim = sep) %>% 
            mutate(data_type = str_extract(.x, "(?<=/).*?(?=/)"),
                   filename = str_remove_all(.x, ".*/|.csv"),
                   task = str_extract(.x, subfolder_name)))
}

#' Function to join nested dataframes with an unnested dataframe
#' Retrieved from https://stackoverflow.com/questions/50125026/joining-dataframe-to-nested-dataframes-within-purrrmap?noredirect=1&lq=1
#' 
#' A wrapper function around left_join
#' 
#' @param df_nest The name of the vector that contains the nested dataframes.
#' @param df_unnest The name of the unnested dataframe that will be joined to the nested ones.
#' @param var_by The id variables to join by.
#' 
#' @return A vector of nested dataframes.
join_df <- function(df_nest, df_unnest, var_by = c("id", "task", "consentTime")) {
  left_join(df_nest, df_unnest, by = var_by)
}

#' Writes experimental data into tdf format
#' 
#' This function is meant to be run for each row containing experimental data
#'
#' @param x  row of "source" data frame
#'
#' @return
#' this function is useful for its side effect
#' @export
#'
#' @examples
#' 
#' source df was created using steps in the markdown document
#' 
#' source %>% 
#'   by_row(write_tdf2)
write_tdf2 <- function(x) {
  write_tsv(unnest(x$join[[1]]), path = paste0("data/Raw/", x$task[[1]], "/", x$task[[1]], "_data.tsv"))
}

#' Function to not include a vector in another vector
#' Retrieved from https://stackoverflow.com/questions/5831794/opposite-of-in
#' 
`%ni%` <- Negate(`%in%`)