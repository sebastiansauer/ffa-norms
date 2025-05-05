

prepare_FMI_data <- function(file_name = NULL,
                             match_items_according_to = "PAID_2009",
                             exclude_categorical = TRUE,  # exclude items with categorical answers such as "fast nie"?
                             verbose = TRUE) {
  
  # read row data:
  library(tidyverse)
  library(here)
  library(easystats)
  library(janitor)
  
  # warum absoluter Pfad?!
  

  
  file_name <- "data/Achtsamkeit_Daten_FFAEichung_rekodiert_2.sav"
  
  stopifnot(file.exists(file_name))
  
  # if (is.null(file_name)) file_name <- "/Users/sebastiansaueruser/datasets/mindfulness/ffa-norms-raw-data/data/Achtsamkeit_Daten_FFAEichung_rekodiert_2.sav"
  
  d_raw <- data_read(file_name)
  
  d_no_empty_cols <- d_raw %>% remove_empty_columns()
  
  # mind the recoded item:
  d_no_empty_cols_rec <-
    d_no_empty_cols %>% 
    rename(FFA_13r = FFA_13_rek)
  
  
  # get item names:
  ffa_items <-
    d_no_empty_cols_rec %>% 
    select(starts_with("FFA_"))
  
  ffa_items_names <- 
    ffa_items %>% 
    names()
  
  ffa13_items_names <-
    ffa_items_names %>%
    setdiff("FFA_13r")
  
  
  # recode items.
  ffa_items_num <-
    ffa_items %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ case_when(. == "fast nie" ~ 0,
                                     . == "eher selten" ~ 1,
                                     . == "relativ oft" ~ 2,
                                     . == "fast immer" ~ 3,
                                     TRUE ~ NA_real_))) |> 
    clean_names()
  
  
  # Mapping of items to subfactors:
  ffa13_item_nrs <- c(1:12, 14)
  ffa14_item_nrs <- c(1:14)
  ffa13_items_names <- names(ffa_items_num)
  ffa14_items_names <- names(ffa_items_num)
  
  presence_item_nrs <- c(1, 2, 3, 5,7, 10)
  
  presence_item_nrs_2013_diagnostica <- c(1, 2, 3, 5,7)
  
  if (match_items_according_to == "Diagnostica_2013") presence_item_nrs_2013 <- presence_item_nrs_2013_diagnostica
  
  presence_item_names <- names(ffa_items_num)[presence_item_nrs]
  
  acceptance13_item_nrs <- setdiff(ffa13_item_nrs, presence_item_nrs)
  acceptance13_item_names <- names(ffa_items_num)[acceptance13_item_nrs]
  
  
  if (verbose) cat("The following items were matched to the *presence* factor: ", presence_item_names, " \n")
  if (verbose) cat("The following items were matched to the *acceptance* factor: ", acceptance13_item_names, " \n")
  
  # Compute mean scores:
  ffa_mean_scores <-
    ffa_items_num %>% 
    #select(all_of(presence_item_names)) %>% 
    mutate(presence_mean = rowMeans(select(., all_of(presence_item_names))),
           acceptance13_mean = rowMeans(select(., all_of(acceptance13_item_names))),
           fmi13_mean = rowMeans(select(., all_of(ffa13_items_names))),
           fmi14_mean = rowMeans(select(., all_of(ffa14_items_names)))
    )
  
  
  d_means_items <-
    d_no_empty_cols_rec %>%
    bind_cols(ffa_mean_scores %>% select(fmi13_mean, presence_mean, acceptance13_mean, fmi14_mean)) |> 
    bind_cols(ffa_items_num) |> 
    rename(phq_sum = PHQ_Sum)
  
  if (verbose) {
    cat("mutating factors to character variables\n")
    d_means_items <-
      d_means_items %>% 
      mutate(across(where(is.factor), as.character))
  }
  
  
  d_means_items_rec <- 
    d_means_items %>% 
    mutate(Geschlecht = case_when(
      Geschlecht == "männlich" ~ "male",
      Geschlecht == "weiblich" ~ "female",
      Geschlecht == "divers" ~ "other"
    ),
    Achts_regel = case_when(
      Achts_regel == "nicht" ~ "no",
      Achts_regel == "ja" ~ "yes"
    ),
    Retreats = case_when(
      Retreats == "Alle paar Jahre" ~ "Once every couple of years",
      Retreats == "Einmal pro Jahr" ~ "Once a year",
      Retreats == "Mehrmals pro Jahr" ~ "Multiple retreats per year",
      Retreats == "Nie" ~ "Never",
      Retreats == "Seltener" ~ "Rarely"
    ),
    mindfulness_experience = case_when(
      tägl_Übung == "gar nie" ~ 0,
      TRUE ~ 1
    )
    )
  
  
  if (exclude_categorical) {
    d_means_items_rec <-
      d_means_items_rec |> 
      dplyr::select(-starts_with("FFA_", ignore.case = FALSE)) |> 
      dplyr::select(-starts_with("PHQ_", ignore.case = FALSE))
  }
  
  
  
  return(d_means_items_rec)
  
}
