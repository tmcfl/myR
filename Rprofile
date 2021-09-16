#----------------------------------------------------------------------------------------------------
# OPTIONS #------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
updated <- "2021.09.15"
# CHANGES:
#  - changed max.print option from 300 to 3000
#  - added tibble.print_max option
#  - added new helper function zero_if_na()
#  - fixed issue with dfcounts() where all-null df columns caused an error with the viz_func() function

options(width = 80)
options(max.print = 3000)
options(tibble.print_max = 300)
options(stringsAsFactors = FALSE)
options(datatable.fread.datatable = FALSE)
options(row.names = FALSE)
#options(strip.white = TRUE)
Sys.setenv(TZ = "UTC")

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
# 	invisible(sapply(auto.loads, sshhh))
# }

# if(interactive()) {
# 	invisible(suppressPackageStartupMessages(library("ggplot2")))
# 	invisible(suppressPackageStartupMessages(library("dplyr")))
# }

#----------------------------------------------------------------------------------------------------
# CUSTOM FUNCTIONS #---------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

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

# Print all rows of a dataframe by default, or specify number of rows as a parameter
.env$print_all <- function(df, n = Inf) {
  print(tibble::tibble(df), n = n)
}

# Returns names(df) in single column, numbered matrix format.
.env$dfnames <- function(df) matrix(names(df))

# Makes names safer
.env$safenames <- function(x) {
  safe_names <- names(x)
  if(is.null(safe_names)) {
    if(!is.character(x)) { stop("You supplied something other than a dataframe or character vector") }
    safe_names <- x
  }
  safe_names <- trimws(safe_names)
  safe_names <- gsub('[^a-zA-Z0-9_]+', '_', safe_names) # replace anything that's not a lowercase letter or a number with underscore
  safe_names <- make.names(safe_names, unique = TRUE)
  safe_names <- gsub('.', '_', safe_names, fixed = TRUE)
  safe_names
}

# Quick defaults for data.table::fread
.env$import_fread <- function(filepath, colclasses = NULL, select = NULL){
	df <- data.table::fread(input = filepath, colClasses = colclasses, select = select)
	df
}

# Helper functions
.env$zero_if_na <- function(x) {
  x[is.na(x)] <- 0
  x
}
.env$col_class_ <- function(x) { (class(x)[1]) }
.env$cnt_na_ <- function(x) { sum(is.na(x)) }
.env$cnt_unique_ <- function(x, na.rm = TRUE) {
  if(isTRUE(na.rm)) {
    x <- na.omit(x)
    y <- length(x[!duplicated(x)])
  }
  if(isFALSE(na.rm)) {
    y <- length(x[!duplicated(x)])
  }
  y
}
.env$pct_na_ <- function(x) { round(cnt_na_(x) / length(x), 5)}
.env$pct_unique_ <- function(x) { round(cnt_unique_(x) / length(x), 5) }

# Get counts and percents of uniques and NA's per column of a data.frame
# ... replacement for count_na and count_unique
.env$dfcounts <- function(df, include_vis = TRUE) {
  shhh("data.table")
  DT <- data.table(df)
  nrow_df <- nrow(df)
  
  output <- 
    data.frame(
      idx = 1:length(DT)        # 1
      , col_name = names(DT)    # 2
      , col_class = DT[, sapply(.SD, function(x) class(x)[1])]
      , cnt_uniq = DT[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE))]
      , cnt_na = DT[, sapply(.SD, function(x) sum(is.na(x)))]
      , row.names = NULL
    )
  detach("package:data.table")
  
  # output$pct_uniq <- round(output$cnt_uniq / (nrow_df - output$cnt_na), 4)
  output$pct_uniq <- as.numeric(substr(output$cnt_uniq / (nrow_df - output$cnt_na), 1, 6))
  # output$pct_na <- round(output$cnt_na / nrow_df, 4)
  output$pct_na <- as.numeric(substr(output$cnt_na / nrow_df, 1, 6))
  output[, 4:7] <- sapply(output[, 4:7], zero_if_na)
  
  vis_func <- function(x) {
    pct_bar <- stringr::str_pad(paste0(c(rep_len("-", floor(10*x))), collapse = ""), 10, "right", " ")
    paste0("|", pct_bar)
  }
  
  if (include_vis) {
    output$vis_uniq <- sapply(output$pct_uniq, vis_func)
    output$vis_na <- sapply(output$pct_na, vis_func)
    return(output[, c("idx", "col_name", "col_class", "cnt_uniq", "pct_uniq", "vis_uniq", 
                      "cnt_na", "pct_na", "vis_na")])
  } else {
    return(output[, c("idx", "col_name", "col_class", "cnt_uniq", "pct_uniq", "cnt_na", "pct_na")])
  }
}

# Legacy dfcounts()
.env$dfcounts_ <- function(df, include_pct = TRUE, include_na = TRUE, include_vis = TRUE) {
  shhh("data.table")
  DT <- data.table(df)
  
  output <- 
    data.frame(
      idx = 1:length(DT)        # 1
      , col_name = names(DT)    # 2
      , col_class = DT[, sapply(.SD, class)]
      , cnt_unique = DT[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE))]
      , row.names = NULL
    )
  
  if (include_pct) {
    output$pct_unique <- round(output$cnt_unique / nrow(df), 4)
    if(include_vis) {
      output$vis_unique <- sapply(
        output$pct_unique, 
        function(x) {
          paste0(
            "|", 
            stringr::str_pad(paste0(c(rep_len("-", floor(10*x))), collapse = ""), 10, "right", " "),
            "|"
          )
        }
      )
    }
    if (include_na) {
      output$cnt_na <- DT[, sapply(.SD, function(x) sum(is.na(x)))]
      output$pct_na <- round(output$cnt_na / nrow(df), 4)
      if(include_vis) {
        output$vis_na <- sapply(
          output$pct_na, 
          function(x) {
            paste0(
              "|", 
              stringr::str_pad(paste0(c(rep_len("-", floor(10*x))), collapse = ""), 10, "right", " "),
              "|"
            )
          }
        )
      }
      return(output)
    } else {
      return(output)
    }
  }
  if (!include_pct) {
    if (include_na) {
      output$cnt_na <- DT[, sapply(.SD, function(x) sum(is.na(x)))]
      return(output)
    }
  }
}



# Get the count of NA's per column of a data.frame
.env$count_na <- function(df, include_unique = FALSE, return_only_na_cols = FALSE){
  if(!is.data.frame(df)) {stop("You supplied something other than a dataframe")}
  
  n_rows <- nrow(df)
  col_class <- sapply(df, col_class_)
  cnt_na <- sapply(df, cnt_na_)
  pct_na <- sapply(df, pct_na_)
  
  output <- 
    dplyr::tibble(
      idx = 1:ncol(df)
      , col_name = names(df)
      , col_class = col_class
      , cnt_na = cnt_na
      , pct_na = pct_na
    )
  
  if(isTRUE(return_only_na_cols)) {
    output <- output[output$cnt_na > 0, ]
  }
  if(isTRUE(include_unique)) {
    cnt_unique <- sapply(df[, output$col_name], cnt_unique_)
    pct_unique <- sapply(df[, output$col_name], pct_unique_)
    output[["cnt_unique"]] <- cnt_unique
    output[["pct_unique"]] <- pct_unique
  }
  
  sample_len <- ifelse(n_rows < 50, n_rows, 50)
  row_idx <- sort(sample(1:n_rows, size = sample_len))
  col_sample <- sapply(df[row_idx, output$col_name], function(x) stringr::str_trunc(paste(x, collapse = ", "), width = 80))
  output[["sample"]] <- col_sample
  
  output
}

# Get the count of unique values per column of a data.frame
.env$count_unique <- function(df, include_na = FALSE) {
  if(!is.data.frame(df)) {stop("You supplied something other than a dataframe")}
  
  n_rows <- nrow(df)
  col_class <- sapply(df, col_class_)
  cnt_unique <- sapply(df, cnt_unique_)
  pct_unique <- sapply(df, pct_unique_)
  
  output <- 
    dplyr::tibble(
      idx = 1:ncol(df)          # 1
      , col_name = names(df)    # 2
      , col_class = col_class   # 3
      , cnt_unique = cnt_unique # 4 - unique count
      , pct_unique = pct_unique
    )
  
  if(isTRUE(include_na)) {
    cnt_na <- sapply(df, cnt_na_)
    pct_na <- sapply(df, pct_na_)
    output[["cnt_na"]] <- cnt_na
    output[["pct_na"]] <- pct_na
  }

  sample_len <- ifelse(n_rows < 50, n_rows, 50)
  row_idx <- sort(sample(1:n_rows, size = sample_len))
  col_sample <- sapply(df[row_idx, output$col_name], function(x) stringr::str_trunc(paste(x, collapse = ", "), width = 80))
  output[["sample"]] <- col_sample

  output
}


# Find columns in a data frame with only 1 unique value (not counting missing)
.env$find_constants <- function(df, with_NAs = FALSE) {
  if (isTRUE(with_NAs)) {
    subset(dfcounts(df, include_pct = FALSE), cnt_unique == 1 & cnt_na > 0)
  } else {
    subset(dfcounts(df, include_pct = FALSE), cnt_unique == 1)
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
# .env$interactive_exploration <- function(df, two_var=FALSE) {
#   library(manipulate)
#   library(DescTools)
#   
#   cnames <- colnames(df)
#   
#   if(two_var) {
#     manipulate(
#       Desc(df[[c_picker1]] ~ df[[c_picker2]], main = paste(c_picker1, c_picker2, sep = " ~ ")),
#       c_picker1 = picker(as.list(cnames)),
#       c_picker2 = picker(as.list(cnames))
#     )
#   } else {
#     manipulate(
#       Desc(df[[c_picker1]], main = c_picker1),
#       c_picker1 = picker(as.list(cnames))
#     )
#   }
# }

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

# Convert an Excel DateTime serial number to R DateTime
.env$excel_date_to_datetime <- function(dt, as_POSIXct = FALSE) {
  # (as.POSIXct(dt * (60*60*24), origin = "1899-12-30", tz = "UTC")) # original method
  output <- as.Date(dt, origin = "1899-12-30", tz = "UTC")
  if(as_POSIXct) {
    output <- as.POSIXct(output)
  }
  output
}

# Convert an Excel file (.xls or .xlsx) to a CSV
.env$excel_to_csv <- function(fpath, sheet = NULL, na = c("NA", "N/A", "")) {
  df <- readxl::read_excel(fpath, sheet = sheet, na = na, col_types = "text")
  names(df) <- safenames(names(df))
  fpath <- gsub("\\.xls.*", ".csv", fpath)
  readr::write_csv(df, fpath, na = "")
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

profile_message <- paste0("*** Successfully loaded ~/.Rprofile, version: ", updated, " ***")

# Remind myself about changed defaults
message("*** Changed Defaults: ***")
message("options(stringsAsFactors=FALSE)")
message("options(datatable.fread.datatable=FALSE)")
message("options(row.names=FALSE)")
message("Sys.setenv(TZ='UTC'), not 'MDT'")
message("\n", profile_message, "\n")
# helpful for warning myself if I meant to run vanilla R
rm(updated, profile_message)
