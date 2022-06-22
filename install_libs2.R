
package_list <- c(
  "DescTools",
  "tidyverse",
  "DescTools",
  "lubridate",
  "caret",
  "data.table",
  "ggExtra",
  "ggvis",
  "ada",
  "animation",
  "corrplot",
  "DiagrammeR",
  "gam",
  "gbm",
  "gdata",
  "glmnet",
  "gplots",
  "gtable",
  "igraph",
  "visNetwork",
  "knitr",
  "MASS",
  "mboost",
  "microbenchmark",
  "neuralnet",
  "nnet",
  "randomForest",
  "rmarkdown",
  "ROCR",
  "rvest",
  "shiny",
  "swirl",
  "manipulate",
  "pROC",
  "zoo",
  "e1071",
  "broom",
  "questionr",
  "beepr",
  "fastDummies",
  "feather",
  "mltools",
  "odbc",
  "openxlsx",
  "pacman",
  "pbapply",
  "rJava",
  "rjson",
  "xgboost",
  # "h2o",
  "RJDBC",
  "RODBC", # install after xcode / command line tools
  "RPostgreSQL",
  "RPostgres",
  "addinslist"
)


for(p in package_list) {
  if( !p %in% as.data.frame(installed.packages())$Package ) {
    install.packages(p)
  }
}



# GITHUB PACKAGES ---------------------------------------------------------
devtools::install_github("Ather-Energy/ggTimeSeries")
devtools::install_github("dkilfoyle/rpivotGadget")
devtools::install_github("ujjwalkarn/xda") # xda: exploratory data analysis
devtools::install_github("MangoTheCat/tidyshiny") # tidyshiny - use tidyr interactively to gather columns into rows - tidyData(iris)
devtools::install_github("bnosac/cronR") # cronR: schedule R scripts/processes with the cron scheduler

# addins manager
devtools::install_github('rstudio/DT')
devtools::install_github("csgillespie/addinmanager")

