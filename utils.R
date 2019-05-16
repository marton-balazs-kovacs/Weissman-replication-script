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