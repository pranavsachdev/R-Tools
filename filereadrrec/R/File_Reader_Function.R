#' A Data read-in Function
#'
#' This function allows you to read all the data files that reside within a nested folder structure.
#' @param pth The path of the parent folder for the nested data files to be read from
#' @keywords csv, xlsx, xls
#' @export
#'     fl_read filename_resolution
#' @examples
#' fl_read()
#' filename_resolution()
#'
#' Description:
#' Provide the starting folder path and the system will read all relevant files within this nested folder.
#' 1.	Essentially, this package was designed to read files of the format (csv/xlsx/xls), so any other files that reside within the folder structure, eg. PDF, txt, etc; will be ignored.
#' 2.	The naming convention of the read files will be:
#'   a.	For CSV:	 ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename
#'   b.	For XLSX/XLS:	ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename_SheetName1
#' ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename_SheetName2
#' 3.	A report of files which could not be read (maybe due to corrupt data) will be generated for reference in the end.

    # Loading library:
      # List packages to install or load
      list.of.packages <- c("tidyverse","readxl","reshape2", "stringr")
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
      if(length(new.packages)) install.packages(new.packages)
      # Load packages
      lapply(list.of.packages, library, character.only = TRUE)



     # Loading the data files (Recursive Algorithm): (Not using!)

     # loc_explor_recursive <- function(pth, i = 1){
     #      tmp_pth = pth
     #      locnames <- list.files(path = pth)
     #      print(locnames)
     #
     #      for(loc_val in locnames){
     #           if(!grepl("\\.",loc_val)){
     #                pth = paste(tmp_pth, "/", loc_val, sep="")
     #                print(paste("path = ", pth, "i = ", i))
     #                loc_explor(pth, i = i+1)
     #           }
     #           else{
     #                print(loc_val)
     #
     #                pth_wo_period <- gsub("\\.", "", pth)
     #                pth_w_unscr <- gsub("/", "_", pth_wo_period)
     #                print(pth_w_unscr)
     #                assign(paste("projData", pth_w_unscr, "_", strsplit(loc_val, "\\.")[[1]][1], sep = ""), "hello", envir = .GlobalEnv)
     #
     #                fl_nam = strsplit(loc_val, "\\.")[[1]][1]
     #                fl_type = strsplit(loc_val, "\\.")[[1]][2]
     #
     #                if(fl_type == "csv") {print(paste("Reading csv", fl_nam));assign(paste("projData", pth_w_unscr, "_", fl_nam, sep = ""), read.csv(paste(pth, "/", loc_val, sep = "")), envir = .GlobalEnv)}
     #                if(fl_type == "xlsx") {print(paste("Reading xlsx", fl_nam))
     #                     sheet_nm <- excel_sheets(paste(pth, "/", loc_val, sep = ""))
     #                     for(tmp_sht in sheet_nm){
     #                          assign(paste("projData", pth_w_unscr, "_", fl_nam, "_", tmp_sht, sep = ""), read_excel(paste(pth, "/", loc_val, sep = ""), sheet = tmp_sht), envir = .GlobalEnv)
     #                     }
     #
     #                }
     #                else{
     #                     print(paste("Unable to read file", fl_nam))
     #                }
     #
     #           }
     #      }
     #
     # }


     # A function to split the string into its components. To be used later in 'fl_read' function.
     filename_resolution <- function(dfcol){
          chartr <- as.character(dfcol)
          splt <- unlist(strsplit(as.character(chartr), "/"))
          lastTrm <- "[["(splt, length(splt))
          return(lastTrm)
     }

     # Function to read the files present in nested folders
     fl_read <- function(pth, i = 1){
          if(pth != "."){
               setwd(pth)
               pth = "."
          }
          files_list <- list.files(pth, recursive = TRUE)
          split_contents <- (strsplit(files_list, "\\."))
          df1 <- data.frame(matrix(unlist(split_contents), nrow=length(split_contents), byrow=T))
          df1$X3 <- unlist(lapply(df1$X1, filename_resolution))
          #sapply(strsplit(as.character(df1$X1), "/"), "[[", 3)
          #df1$X3 <- lapply(as.character(df1$X1), strsplit, "/")
          colnames(df1) <- c('Path', 'Extension', 'Filename')
          df1 <- df1[c(1,3,2)]

          err_cntr <<- 1
          err_list <<- list()

          # Filtering out records of dataframe that are either csv or xls/xlsx
          # csv:
          filenames_csv <- df1[tolower(df1$Extension)=='csv',]
          # Reading csv files:
          for(i in 1:dim(filenames_csv)[1]){
               tryCatch({
                    #print(i)
                    #print(filenames_csv[i,])
                    assign(paste("projData_", gsub("/", "_", filenames_csv$Path[i]), sep = ""), read.csv(as.character(paste("./", filenames_csv$Path[i],".",filenames_csv$Extension[i], sep = ""))) , envir = .GlobalEnv)
               }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                                    err_list[[err_cntr]] <<- conditionMessage(e)
                                    err_cntr <<- err_cntr + 1
                                   })
          }

          # Reading xlsx, xls files:
          filenames_xlsx <- df1[tolower(df1$Extension)=='xlsx' | tolower(df1$Extension)=='xls',]
          # Reading files:
          for(i in 1:dim(filenames_xlsx)[1]){
               tryCatch({
                    sheet_nm <- excel_sheets(paste("./", filenames_xlsx$Path[i], ".", filenames_xlsx$Extension[i], sep = ""))
                    for(tmp_sht in sheet_nm){
                         assign(paste("projData_", gsub("/", "_", filenames_xlsx$Path[i]), "_", tmp_sht, sep = ""), read_excel(paste("./", filenames_xlsx$Path[i],".",filenames_xlsx$Extension[i], sep = ""), sheet = tmp_sht), envir = .GlobalEnv)
                    }
               }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                                    err_list[[err_cntr]] <<- conditionMessage(e)
                                    err_cntr <<- err_cntr + 1
                                   })
          }

          # Print the message for displaying information of unread files:
          err_list <<- as.data.frame(unlist(err_list))
          cat(paste("\n\n\n\n\nINFORMATION OF UNREAD FILES IN THE DATAFRAME NAMED: err_list\n\n\n"))
     }
