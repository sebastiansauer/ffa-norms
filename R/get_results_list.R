get_results_list <- function(cfa_model, model_name) {
  out <- list()
  
  out <- list(
    obj_name = deparse(substitute(cfa_model)),
    model_name = model_name,
    # overview = list(summary(cfa_model)),
    cfi = fitMeasures(cfa_model)["cfi"],
    tli = fitMeasures(cfa_model)["tli"],
    rmsea = fitMeasures(cfa_model)["rmsea"],
    srmr = fitMeasures(cfa_model)["srmr"],
    chisq = fitMeasures(cfa_model)["chisq"],
    df = fitMeasures(cfa_model)["df"],
    pvalue = fitMeasures(cfa_model)["pvalue"]
  )
  
  return(out)
}
