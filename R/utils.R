

#' Returns file name of calling Rscript
#'
#' \code{get_Rscript_filename} returns the file name of calling Rscript 
#' @return A string with the filename of the calling script.
#'      If not found (i.e. you are in a interactive session) returns NA.
#'
#' @export
get_Rscript_filename <- function() {
    prog <- sub("--file=", "", grep("--file=", commandArgs(), value=TRUE)[1])
    if( .Platform$OS.type == "windows") { 
        prog <- gsub("\\\\", "\\\\\\\\", prog)
    }
    prog
}

#' Recursively sorts a list
#'
#' \code{sort_list} returns a sorted list
#' @param unsorted_list A list.
#' @return A sorted list.
#' @export
sort_list <- function(unsorted_list) {
    for(ii in seq(along=unsorted_list)) {
        if(is.list(unsorted_list[[ii]])) {
            unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
        }
    }
    unsorted_list[sort(names(unsorted_list))] 
}
