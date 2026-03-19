# Copyright (c) 2011-2026 Trevor L. Davis <trevor.l.davis@gmail.com>
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
#' `getfile()` returns the file name that `Rscript` is interpreting.
#' `get_Rscript_filename()` is an alias.
#' @return A string with the filename of the calling script.
#'      If not found (i.e. you are in a interactive session) returns `NA_character_`.
#' @export
getfile <- function() {
	args <- command_args()
	args_idx <- match("--args", args)
	if (!is.na(args_idx)) {
		args <- args[seq_len(args_idx - 1L)]
	}
	prog <- sub("--file=", "", grep("^--file=", args, value = TRUE)[1L])
	if (.Platform$OS.type == "windows") {
		prog <- gsub("\\\\", "\\\\\\\\", prog)
	}
	prog
}

#' @rdname getfile
#' @export
get_Rscript_filename <- getfile

command_args <- function() commandArgs()

is_negative_number <- function(x) {
	regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x) > 0L
}

is_long_flag <- function(x) {
	startsWith(x, "--") && nchar(x) > 2L
}

is_short_flag <- function(x) {
	startsWith(x, "-") && !startsWith(x, "--") && nchar(x) > 1L && !is_negative_number(x)
}

normalize_opt_helper <- function(o) {
	if (is_short_flag(o) && nchar(o) > 2L) {
		paste0("-", strsplit(substring(o, 2L), "")[[1L]])
	} else {
		o
	}
}

normalize_opt <- function(opt) {
	unlist(lapply(opt, normalize_opt_helper))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

na_omit <- function(x) {
	Filter(Negate(is.na), x)
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
