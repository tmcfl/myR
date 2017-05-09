###########
# OPTIONS #
###########
# UPDATED 2017-05-09

options(width = 80)
options(max.print = 500)
options(stringsAsFactors = FALSE)
options(datatable.fread.datatable = FALSE)
options(row.names = FALSE)
#options(strip.white = TRUE)
Sys.setenv(TZ = "UTC")
#Sys.setenv("plotly_api_key" = "rxwd8zm45i")
#Sys.setenv("plotly_username" = "tmcfl")

# effectively forces R to never use scientific notation to express very small or large numbers
options(scipen = 10)

# changes the continuation prompt to the wider "... " (instead of "+ ")
options(prompt = "> ")
options(continue = "... ")

# allows you to tab-complete package names for use in library() and require() calls
utils::rc.settings(ipck = TRUE)

# this instructs R to, before anything else, echo a timestamp to the console
# and to my R history file. Saves every command run in the console to a history file.
.First <- function() {
  if(interactive()) {
    library(utils)
    timestamp(, prefix = paste("##------ [", getwd(), "] ", sep = ""))
  }
}

# this instructs R, right before exiting, to write all commands I used in that
# session to my R command history file.
.Last <- function() {
  if(interactive()) {
    hist_file <- Sys.getenv("R_HISTFILE")
    if(hist_file == "") hist_file <- "~/.RHistory"
    savehistory(hist_file)
  }
}

# defines the packages I want automatically loaded
# auto.loads <- c("dplyr", "ggplot2")

# loads the packages in "auto.loads" if the R session is interactive
# if(interactive()) {
#   invisible(sapply(auto.loads, sshhh))
# }

# if(interactive()) {
#   invisible(suppressPackageStartupMessages(library("ggplot2")))
#   invisible(suppressPackageStartupMessages(library("dplyr")))
# }

####################
# CUSTOM FUNCTIONS #
####################

# better defaults for write.csv
# write.csv <- function(adataframe, filename, ...){
#   outfile <- file(filename, "w", encoding="UTF-8")
#   utils::write.csv(adataframe, outfile, row.names=FALSE, ...)
#   close(outfile)
# }

# Creates a new hidden namespace where we can store some functions in.
# This is in order for these functions to survive a call to "rm(list=ls())"
# which removes everything in the current namespace.
.env <- new.env()

# defines a function that loads a library into the namespace without warning or startup messages
.env$shhh <- function(a.package) {
  suppressWarnings(suppressPackageStartupMessages(
  library(a.package, character.only=TRUE)))
}

.env$library_shh <- function(x, ...){
  eval(substitute(suppressPackageStartupMessages(library(x, ...))))
}

# Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x) {
    rownames(x) <- NULL
    x
}

# This defines a function to sanely undo a "factor()" call.
.env$unfactor <- function(df) {
  id <- sapply(df, is.factor)
  df[id] <- sapply(df[id], as.character)
  df
}

# Returns names(df) in single column, numbered matrix format.
.env$dfnames <- function(df) matrix(names(df))

# Makes names safer
.env$safenames <- function(names) {
  names <- gsub('[^a-z0-9]+', '_', tolower(names))
  names <- make.names(names, unique = TRUE, allow_ = TRUE)
  names <- gsub('.', '_', names, fixed = TRUE)
  names
}

# Quick defaults for data.table::fread
.env$import_fread <- function(filepath, colclasses = NULL, sel = NULL){
  df <- data.table::fread(input = filepath, na.strings = c("NA", "", " "), colClasses = colclasses, select = sel)
  df
}

# Get the count of NA's per column of a data.frame
.env$count_na <- function(df, include_pct = FALSE, return_only_na_cols = FALSE){
  df_dim <- dim(df)
  cnt_na <- apply(df, 2, function(x) sum(is.na(x)))
  pct_na <- round(cnt_na / df_dim[1], 8)
  output <- data.frame(idx = 1:df_dim[2], col_name = names(df), cnt_na = cnt_na, row.names = NULL)
  
  if (include_pct == TRUE) {
    output$pct_na <- pct_na
  }
  
  if (return_only_na_cols == TRUE) {
    output <- output[output$cnt_na > 0, ]
  }
  
  output
}

# Get the count of unique values per column of a data.frame
.env$count_unique <- function(df, include_pct = FALSE, include_na = TRUE) {
  col_class <- apply(df, 2, class)
  cnt_unique <- apply(df, 2, function(x) length(unique(na.omit(x))))
  cnt_na <- apply(df, 2, function(x) sum(is.na(x)))
  
  pct_unique <- round(cnt_unique / nrow(df), 8)
  pct_na <- round(cnt_na / nrow(df), 8)
  
  output <- 
    data.frame(
      idx = 1:length(df),       # 1
      col_name = names(df),     # 2
      col_class = col_class,    # 3
      cnt_unique = cnt_unique,  # 4 - unique count
      pct_unique = pct_unique,  # 5 - unique pct
      cnt_na = cnt_na,          # 6 - na count
      pct_na = pct_na,          # 7 - na pct
      row.names = NULL
    )
  
  if (include_na == TRUE & include_pct == FALSE) {
    return(output[, c(1:4, 6)])
  } else if (include_na == FALSE & include_pct == TRUE) {
    return(output[, c(1:4, 5)])
  } else if (include_na == TRUE & include_pct == TRUE) {
    return(output)
  } else {
    return(output[, 1:4])
  }
}

# Simple count unique
.env$cnt_uniq <- function(df) {
  #len_unique <- function(x) length(unique(na.omit(x)))
  #cnt_unique <- apply(df, 2, len_unique)
  cnt_unique <- apply(df, 2, data.table::uniqueN)
  
  output_df <- data.frame(
    idx = 1:length(df),
    col_name = names(df),
    cnt_unique = cnt_unique,
    row.names = NULL
  )
  output_df
}

# Find columns in a data frame with only 1 unique value (not counting missing)
.env$find_constants <- function(df, with_NAs = FALSE) {
  if (with_NAs == TRUE) {
    subset(count_unique(df), cnt_unique == 1 & cnt_na > 0)
  } else {
    subset(count_unique(df), cnt_unique == 1)
  }
}

# Find identical columns in a data frame
.env$find_identicals <- function(df) {
  col_names <- names(df)
  feature_pairs <- combn(col_names, 2, simplify = FALSE)
  
  col1 <- c()
  col2 <- c()
  
  for (pair in feature_pairs) {
    f1 <- pair[1]
    f2 <- pair[2]
    if (!(f1 %in% col2) & !(f2 %in% col2)) {
      if (identical(df[[f1]], df[[f2]])) {
        col1 <- c(col1, f1)
        col2 <- c(col2, f2)
      }
    }
  }
  col3 <- which(col_names %in% col2)
  
  output <- data.frame(x_column = col1, identical_column = col2, idx = col3)
  output
}

# Use Desc and Manipulate to interactively explore a new dataset
.env$interactive_exploration <- function(df, two_var=FALSE) {
  library(manipulate)
  library(DescTools)
  
  cnames <- colnames(df)
  
  if(two_var) {
    manipulate(
      Desc(df[[c_picker1]] ~ df[[c_picker2]], main = paste(c_picker1, c_picker2, sep = " ~ ")),
      c_picker1 = picker(as.list(cnames)),
      c_picker2 = picker(as.list(cnames))
    )
  } else {
    manipulate(
      Desc(df[[c_picker1]], main = c_picker1),
      c_picker1 = picker(as.list(cnames))
    )
  }
}


# Convert a character string representing days of the week into an ordered factor (sun to sat)
.env$dow2factor <- function(x){
  if(min(nchar(x)) > 3){
    d <- factor(x, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  }
  else{
    d <- factor(x, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  }
  d
}

# Calculate the harmonic mean
.env$h_mean <- function(x, ignore.zeros=FALSE, ...) {
  if(ignore.zeros == TRUE) {
    i <- which(!is.na(x) & x != 0)
    x[i] <- 1 / x[i]
    (1/mean(x, ...))
  } else {
    (1/mean(1/x, ...))
  }
}

# Calculate the geometric mean
.env$g_mean <- function(x, ...) {
  if(min(x, na.rm=TRUE) == 0) {
    warning('presence of zeros makes the output zero')
  }
  prod(x, ...)^(1/length(x))
}


# Create a new project directory
.env$newProj <- function(proj_name){
  path <- paste("~/projects", proj_name, sep = "/")
  if(dir.exists(path) == FALSE){
    dir.create(path)
    setwd(path)
    dir.create("data")
    dir.create("functions")
    dir.create("output")
    dir.create("plots")
    dir.create("references")
    dir.create("scripts")
  } else {
    warning("Project folder already exists")
  }
}


attach(.env)


# Remind myself about changed defaults
message("*** Changed Defaults: ***")
message("options(stringsAsFactors=FALSE)")
message("options(datatable.fread.datatable=FALSE)")
message("options(row.names=FALSE)")
message("Sys.setenv(TZ='UTC'), not 'MDT'")
message("\n*** Successfully loaded ~/.Rprofile ***\n")
# helpful for warning myself if I meant to run vanilla R
