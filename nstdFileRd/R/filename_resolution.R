#' A function to support fl_read() function
#'
#' This function allows you to read all the data files that reside within a nested folder structure.
#' @param dfcol dataframe column to be operated upon
#' @keywords csv, xlsx, xls
#' @import readxl
#' @import utils



# A function to split the string into its components. To be used later in 'fl_read' function.
filename_resolution <- function(dfcol){
     chartr <- as.character(dfcol)
     splt <- unlist(strsplit(as.character(chartr), "/"))
     lastTrm <- "[["(splt, length(splt))
     return(lastTrm)
}
