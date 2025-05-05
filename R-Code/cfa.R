# CFA


library(lavaan)
library(semPlot)


# rename the items for the sake of brevity:

fmi_items <- 
 d_w_items %>%
  select(starts_with("ffa"), mindfulness_experience) |> 
  rename_with(~ gsub("^ffa_(\\d+[a-zA-Z]?)$", "i\\1", .), starts_with("ffa_")) 


model_one_dim <- "General_Factor =~  i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12 + i13r + i14"


# Setup

cfa_results <- list()

get_results_list <- function(cfa_model) {
  out <- list()
  
  out <- list(
    model_name = deparse(substitute(cfa_model)),
   # overview = list(summary(cfa_model)),
    cfi = fitMeasures(cfa_model)["cfi"],
    tli = fitMeasures(cfa_model)["tli"],
    rmsea = fitMeasures(cfa_model)["rmsea"],
    srmr = fitMeasures(cfa_model)["srmr"]
  )
}


# One general mindfulness factor, including item 13 -----------------------



# items as categorical:

model_one_dim_fit_ordered <- cfa(model_one_dim, 
                                 data = fmi_items,
                                 estimator = "DWLS",
                                 ordered = TRUE)

cfa_results[["model_one_dim_fit_ordered"]] <- 
  get_results_list(model_one_dim_fit_ordered)
  


# items as numerical:

model_one_dim_fit_numeric <- cfa(model_one_dim, data = fmi_items)

  cfa_results[["model_one_dim_fit_numeric"]] <- 
  get_results_list(model_one_dim_fit_numeric)




# Two factors (presence, acceptance) without item 13, correlated factors --------



model_pres_acc <- "
# Presence:
presence =~ i1 + i2 + i3 + i5 + i7 + i10 

acceptance =~ i4 + i6 + i8 + i9 + i11 + i12 + i14

presence ~~ acceptance
"

# items as numeric:

model_two_dim_wo_13 <- cfa(model_pres_acc,
                           data = fmi_items)

cfa_results[["model_two_dim_wo_13"]] <-
  get_results_list(model_two_dim_wo_13)



# items as categorical:

model_two_dim_wo_13_ordered <- cfa(model_pres_acc,
                                   data = fmi_items,
                                   estimator = "DWLS",
                                   ordered = TRUE)


cfa_results[["model_two_dim_wo_13_ordered"]] <-
  get_results_list(model_two_dim_wo_13_ordered)



semPaths(model_two_dim_wo_13_ordered,
         what = "stand",
         whatLabels = "stand",
         layout = "circle",
        # sizeMan = 3,
         #style = "lisrel",
         residuals = FALSE,
         #fixed = FALSE,
         intercepts = FALSE,
         normalize = FALSE,
         thresholds = F,
         width = 12,
         height = 6,
        # rotation = 2,
         intAtSide = TRUE,
       #  nCharNodes = 0
)
title("model_two_dim_wo_13_ordered")




# Two correlated factors, individuals with meditation practice only -------

# items as numeric:
model_two_dim_wo_13_meditators_only <- 
  cfa(model_pres_acc,
      data = fmi_items |> filter(mindfulness_experience == 1))


cfa_results[["model_two_dim_wo_13_meditators_only"]] <-
  get_results_list(model_two_dim_wo_13_meditators_only)




# items as categorical:

model_two_dim_wo_13_meditators_only_categorical <- 
  cfa(model_pres_acc,
      estimator = "DWLS",
      ordered = TRUE,
      data = fmi_items |> filter(mindfulness_experience == 1))

cfa_results[["model_two_dim_wo_13_meditators_only_categorical"]] <-
  get_results_list(model_two_dim_wo_13_meditators_only_categorical)



d <- as.data.frame(cfa_results)

cfa_results_df <-
  do.call(rbind, lapply(cfa_results, as.data.frame))


out <- cfa_results[lengths(cfa_results) > 0]
