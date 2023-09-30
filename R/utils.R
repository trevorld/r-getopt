# Copyright (c) 2011-2013 Trevor L. Davis <trevor.l.davis@gmail.com>
#
#  This file is free software: you may copy, redistribute and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation, either version 2 of the License, or (at your
#  option) any later version.
#
#  This file is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Returns file name being interpreted by Rscript
#'
#' `get_Rscript_filename()` returns the file name that `Rscript` is interpreting.
#' @return A string with the filename of the calling script.
#'      If not found (i.e. you are in a interactive session) returns `NA_character_`.
#' @export
get_Rscript_filename <- function() { # nolint
    prog <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
    if (.Platform$OS.type == "windows") {
        prog <- gsub("\\\\", "\\\\\\\\", prog)
    }
    prog
}

#' Recursively sorts a list
#'
#' `sort_list()` returns a recursively sorted list
#' @param unsorted_list A list.
#' @return A sorted list.
#' @examples
#' l <- list(b = 2, a = 1)
#' sort_list(l)
#' @export
sort_list <- function(unsorted_list) {
    for (ii in seq(along = unsorted_list)) {
        if (is.list(unsorted_list[[ii]])) {
            unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
        }
    }
    unsorted_list[sort(names(unsorted_list))]
}
