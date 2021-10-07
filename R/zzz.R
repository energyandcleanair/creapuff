.onLoad <- function(libname, pkgname){
  # message("Creating required python environment")
  
  if(!"creapuff" %in% reticulate::conda_list()$name){
    reticulate::conda_create("creapuff")  
  }
  reticulate::use_condaenv("creapuff")
  reticulate::conda_install("creapuff",c("pandas","tqdm"))
  
  # This isn't ideal not to use a virtual env
  # but running into SSL issue on Lauri's Windows computer
  # Using system python
  # reticulate::py_install(c("pandas","tqdm"))
}