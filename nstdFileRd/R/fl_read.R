#' A Data read-in Function
#'
#' Allows you to read all the data files that reside within a nested folder structure.\cr
#' Provide the starting folder path and the system will read all relevant files within this nested folder.\cr \cr
#' 1.	Essentially, this package was designed to read files of the format (csv/xlsx/xls), so any other files that reside within the folder structure, eg. PDF, txt, etc; will be ignored.\cr
#' 2.	The naming convention of the read files will be:\cr
#'   a.	For CSV:	 ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename\cr
#'   b.	For XLSX/XLS:	ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename_SheetName1\cr
#'                       ProjData_<FolderName1>_<FolderName2>_<FolderName3>._filename_SheetName2\cr
#' 3.	A report of files which could not be read (maybe due to corrupt data) will be generated for reference in the end.\cr
#' @param pth The path of the parent folder for the nested data files to be read from
#' @param i A number (optional)
#' @keywords csv, xlsx, xls
#' @export
#'     fl_read
#' @import readxl
#' @import utils
#' @examples \dontrun{
#' fl_read("Path of parent folder")
#'
#' # Will read all the data files (in csv/excel format) present in this nested folder structure.
#'
#' # To view the error log of unread files, use command:
#' err_list
#' }
#'



# Function to read the files present in nested folders
fl_read <- function(pth, i = 1){

     # If the path is not provided in the standard format ("."), this step will make sure that the path is set as "."
     if(pth != "."){
          setwd(pth)
          pth = "."
     }

     # Load the required packages
     ld_pkgs()

     # Create a dataframe for the folder
     files_list <- list.files(pth, recursive = TRUE)
     split_contents <- (strsplit(files_list, "\\."))
     df1 <- data.frame(matrix(unlist(split_contents), nrow=length(split_contents), byrow=T))
     df1$X3 <- unlist(lapply(df1$X1, filename_resolution))
     #sapply(strsplit(as.character(df1$X1), "/"), "[[", 3)
     #df1$X3 <- lapply(as.character(df1$X1), strsplit, "/")
     colnames(df1) <- c('Path', 'Extension', 'Filename')
     df1 <- df1[c(1,3,2)]

     # Creating a temporary environment
     temp_env <- new.env()
     temp_env$err_cntr <- 1
     temp_env$err_list <- list()

     # Filtering out records of dataframe that are either csv or xls/xlsx
     # csv:
     filenames_csv <- df1[tolower(df1$Extension)=='csv',]
     # Reading csv files:
     for(i in 1:dim(filenames_csv)[1]){
          tryCatch({
               #print(i)
               #print(filenames_csv[i,])
               assign(paste("projData_", gsub("/", "_", filenames_csv$Path[i]), sep = ""), read.csv(as.character(paste("./", filenames_csv$Path[i],".",filenames_csv$Extension[i], sep = ""))) , envir = parent.frame())
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
               temp_env$err_list[[temp_env$err_cntr]] <- conditionMessage(e)
               temp_env$err_cntr <- temp_env$err_cntr + 1
          })
     }

     # Reading xlsx, xls files:
     filenames_xlsx <- df1[tolower(df1$Extension)=='xlsx' | tolower(df1$Extension)=='xls',]
     # Reading files:
     for(i in 1:dim(filenames_xlsx)[1]){
          tryCatch({
               sheet_nm <- excel_sheets(paste("./", filenames_xlsx$Path[i], ".", filenames_xlsx$Extension[i], sep = ""))
               for(tmp_sht in sheet_nm){
                    assign(paste("projData_", gsub("/", "_", filenames_xlsx$Path[i]), "_", tmp_sht, sep = ""), read_excel(paste("./", filenames_xlsx$Path[i],".",filenames_xlsx$Extension[i], sep = ""), sheet = tmp_sht), envir = parent.frame())
               }
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
               temp_env$err_list[[temp_env$err_cntr]] <- conditionMessage(e)
               temp_env$err_cntr <- temp_env$err_cntr + 1
          })
     }

     # Print the message for displaying information of unread files:
     assign("err_list", as.data.frame(matrix(unlist(temp_env$err_list), ncol=1, dimnames = list(NULL, "UnreadFiles"))), envir = parent.frame())
     cat(paste("\n\n\n\n\nINFORMATION OF UNREAD FILES IN THE DATAFRAME NAMED: err_list\n\n\n"))

     # Removing the temporary environment
     rm(temp_env)
}
