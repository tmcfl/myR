###########
# OPTIONS #
###########
# UPDATED 08/11/15

options(width = 80)
options(max.print = 100)
options(stringsAsFactors = FALSE)
options(datatable.fread.datatable = FALSE)
Sys.setenv(TZ='UTC')

# effectively forces R to never use scientific notation to express very small or large numbers
options(scipen=10)

# changes the continuation prompt to the wider "... " (instead of "+ ")
options(prompt="> ")
options(continue="... ")

# allows you to tab-complete package names for use in library() and require() calls
utils::rc.settings(ipck=TRUE)

# this instructs R to, before anything else, echo a timestamp to the console
# and to my R history file. Saves every command run in the console to a history file.
.First <- function() {
	if(interactive()) {
		library(utils)
		timestamp(,prefix = paste("##------ [",getwd(),"] ",sep=""))
	}
}

# this instructs R, right before exiting, to write all commands I used in that
# session to my R command history file.
.Last <- function() {
	if(interactive()) {
		hist_file <- Sys.getenv("R_HISTFILE")
		if(hist_file=="") hist_file <- "~/.RHistory"
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
	df[id] <- lapply(df[id], as.character)
	df
}

# Returns names(df) in single column, numbered matrix format.
.env$dfnames <- function(df) matrix(names(df))

attach(.env)


# Remind myself about changed defaults
message("*** Changed Defaults: ***")
message("options(stringsAsFactors=FALSE)")
message("options(datatable.fread.datatable=FALSE)")
message("Sys.setenv(TZ='UTC'), not 'MDT'")
message("\n*** Successfully loaded ~/.Rprofile ***\n")
# helpful for warning myself if I meant to run vanilla R
