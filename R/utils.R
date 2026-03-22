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
	startsWith(x, "-") && !startsWith(x, "--") && nchar(x) == 2L
}

is_bundle <- function(o) {
	startsWith(o, "-") && !startsWith(o, "--") && nchar(o) > 2L
}

expand_bundle <- function(o) {
	paste0("-", strsplit(substring(o, 2L), "")[[1L]])
}

prev_takes_argument <- function(prev, spec) {
	if (is_long_flag(prev) && !grepl("=", prev, fixed = TRUE)) {
		rowmatch <- tryCatch(get_rowmatch(spec, long = substring(prev, 3L)), error = function(e) {
			NA_integer_
		})
	} else if (is_short_flag(prev)) {
		rowmatch <- tryCatch(get_rowmatch(spec, short = substring(prev, 2L)), error = function(e) {
			NA_integer_
		})
	} else {
		return(FALSE)
	}
	actions <- c("append", "store", "store_optional")
	!is.na(rowmatch) && spec[rowmatch, COL_ACTION] %in% actions
}

normalize_opt <- function(opt, spec) {
	result <- character(0L)
	for (o in opt) {
		if (!is_bundle(o)) {
			result <- c(result, o)
		} else if (!is_negative_number(o)) {
			result <- c(result, expand_bundle(o))
		} else if (length(result) > 0L && prev_takes_argument(result[length(result)], spec)) {
			result <- c(result, o)
		} else {
			result <- c(result, expand_bundle(o))
		}
	}
	result
}

`%||%` <- function(x, y) if (is.null(x)) y else x

get_rowmatch <- function(spec, long = NULL, short = NULL) {
	if (!is.null(long)) {
		rowmatch <- grep(long, spec[, COL_LONG_NAME], fixed = TRUE)
		if (length(rowmatch) == 0L) {
			stop(paste0('long flag "', long, '" is invalid'))
		} else if (length(rowmatch) > 1L) {
			rowmatch <- which(long == spec[, COL_LONG_NAME])
			if (length(rowmatch) == 0L) {
				stop(paste0('long flag "', long, '" is ambiguous'))
			}
		}
	} else {
		rowmatch <- grep(short, spec[, COL_SHORT_NAME], fixed = TRUE)
		if (length(rowmatch) == 0L) {
			stop(paste0('short flag "', short, '" is invalid'))
		}
	}
	rowmatch
}

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
	for (ii in seq_along(unsorted_list)) {
		if (is.list(unsorted_list[[ii]])) {
			unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
		}
	}
	unsorted_list[sort(names(unsorted_list))]
}
