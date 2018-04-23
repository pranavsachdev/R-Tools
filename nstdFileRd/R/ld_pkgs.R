#' A function to support fl_read to install required packages
#'
#' This function allows you to read all the data files that reside within a nested folder structure.
#' @keywords csv, xlsx, xls
#' @import readxl
#' @import utils

ld_pkgs <- function(){
     # Loading library:
     # List packages to install or load
     list.of.packages <- c("tidyverse","readxl","reshape2", "stringr")
     new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
     if(length(new.packages)) install.packages(new.packages)
     # Load packages
     lapply(list.of.packages, library, character.only = TRUE)
}
