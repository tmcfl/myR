
package_list <- c(
  "DescTools",
  "tidyverse",
  "DescTools",
  "lubridate",
  "box",  # https://github.com/klmr/box
  "caret",
  "data.table",
  "janitor",  # https://github.com/sfirke/janitor
  "remotes",
  "clock",  # https://clock.r-lib.org/
  "naniar",  # https://github.com/njtierney/naniar
  "Hmisc",
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
  "fasttime",
  "arrow",
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
remotes::install_github("ropensci/skimr") # https://github.com/ropensci/skimr
remotes::install_github("Ather-Energy/ggTimeSeries")
remotes::install_github("dkilfoyle/rpivotGadget")
remotes::install_github("ujjwalkarn/xda") # xda: exploratory data analysis
remotes::install_github("MangoTheCat/tidyshiny") # tidyshiny - use tidyr interactively to gather columns into rows - tidyData(iris)
remotes::install_github("bnosac/cronR") # cronR: schedule R scripts/processes with the cron scheduler

# addins manager
remotes::install_github('rstudio/DT')
remotes::install_github("csgillespie/addinmanager")

