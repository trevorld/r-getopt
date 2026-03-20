# Copyright (c) 2008-2010 Allen Day
# Copyright (c) 2011-2018 Trevor L. Davis <trevor.l.davis@gmail.com>
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

#' C-like getopt behavior
#'
#' `getopt` is primarily intended to be used with `Rscript`.  It
#' facilitates writing `#!` shebang scripts that accept short and long
#' flags/options.  It can also be used from `R` directly, but is probably less
#' useful in this context.
#'
#' Notes on naming convention:
#'
#' 1. An \emph{option} is one of the shell-split input strings.
#'
#' 2. A \emph{flag} is a type of \emph{option}.  a \emph{flag} can be defined as
#' having no \emph{argument} (defined below), a required \emph{argument}, or an
#' optional \emph{argument}.
#'
#' 3. An \emph{argument} is a type of \emph{option}, and is the value associated
#' with a flag.
#'
#' 4. A \emph{long flag} is a type of \emph{flag}, and begins with the string
#' `--`.  If the \emph{long flag} has an associated \emph{argument}, it may be
#' delimited from the \emph{long flag} by either a trailing `=`, or may be
#' the subsequent \emph{option}.
#'
#' 5. A \emph{short flag} is a type of \emph{flag}, and begins with the string
#' `-`.  If a \emph{short flag} has an associated \emph{argument}, it is the
#' subsequent \emph{option}.  \emph{short flags} may be bundled together,
#' sharing a single leading `-`, but only the final \emph{short flag} is able
#' to have a corresponding \emph{argument}.
#'
#' Many users wonder whether they should use the `getopt` package, `optparse` package,
#' or `argparse` package.
#' Here is some of the major differences:
#'
#' Features available in `getopt` unavailable in `optparse`
#'
#' 1. `getopt` allows one to specify options with an optional argument.
#'
#' 2. Long flags may be abbreviated as long as the abbreviation is unique,
#' e.g. `--verb` matches `--verbose` if no other long flag starts with `verb`.
#'
#' Some features implemented in the `optparse` package unavailable in `getopt`
#'
#' 1. Support for capturing positional arguments after the optional arguments
#' when `positional_arguments` set to `TRUE` in `optparse::parse_args()`
#'
#' 2. Automatic generation of an help option and printing of help text when encounters an `-h`
#'
#' 3. Option to specify default arguments for options as well the
#'    variable name to store option values
#'
#' There is also package `argparse` supports
#' all the features of both getopt and optparse (plus more)
#  but which has a dependency on Python 2.7 or 3.2+.
#'
#' Some Features unlikely to be implemented in `getopt`:
#'
#' 1. Support for lists, e.g. `--define os=linux --define os=redhat` would
#' set `result$os$linux=TRUE` and `result$os$redhat=TRUE`.
#'
#' @aliases getopt getopt-package
#' @param spec The getopt specification, or spec of what options are considered
#' valid.  The specification must be either a 4-5 column matrix, a 4-5 column data frame, or a
#' character vector coercible into a 4 column matrix using
#' `matrix(x, ncol = 4L, byrow = TRUE)` command.  The matrix/vector
#' contains:
#'
#' Column 1: the \emph{long flag} name.  A multi-character string.
#'
#' Column 2: \emph{short flag} alias of Column 1.  A single-character string.
#' May be `NA_character_` if there is no short flag.
#'
#' Column 3: \emph{Action} of the \emph{flag}.  A string.
#' Possible values:
#'
#' * `"append"` (flag takes a required argument; appends it to a vector each time the flag is used)
#' * `"count"` (flag takes no argument; stores count of how many times the flag was present)
#' * `"store_true"` (flag takes no argument; stores `TRUE`)
#' * `"store_false"` (flag takes no argument; stores `FALSE`)
#' * `"store"` (flag takes a required argument)
#' * `"store_optional"` (flag takes an optional argument but if none present stores `TRUE`)
#'
#' For backwards compatibility `0`, `1`, `2` are accepted as aliases for
#' `"store_true"`, `"store"`, `"store_optional"` respectively.
#'
#' Column 4: Data type to which the \emph{flag}'s argument shall be cast using
#' [storage.mode()].  A multi-character string.
#' Only used when an action took an argument.
#' Possible values: "logical", "integer", "double", "complex", "character".
#' "numeric" is treated as an alias for "double".
#'
#' Column 5 (optional): A brief description of the purpose of the option.
#'
#' The terms \emph{option}, \emph{flag}, \emph{long flag}, \emph{short flag},
#' and \emph{argument} have very specific meanings in the context of this
#' document.  Read the \dQuote{Details} section of [getopt()] for definitions.
#' @param opt This defaults to the return value of `commandArgs(TRUE)` unless
#'    `argv` is in the global environment in which case it uses that instead
#'    (this is for compatibility with `littler`).
#'
#' If R was invoked directly via the `R` command, this corresponds to all
#' arguments passed to R after the `--args` flag.
#'
#' If R was invoked via the `Rscript` command, this corresponds to all
#' arguments after the name of the R script file.
#'
#' Read about [commandArgs()] and \link{Rscript} to learn more.
#' @param command The string to use in the usage message as the name of the
#' script.  See argument \emph{usage}.
#' @param usage If `TRUE`, argument `opt` will be ignored and a usage
#' statement (character string) will be generated and returned from `spec`.
#' @param debug This is used internally to debug the `getopt()` function itself.
#' @param operand Controls how positional arguments (operands) are handled.
#'
#'   * `"after--only"` (the default) only collects operands that appear after a `"--"` separator.
#'   * `"strict"` (in addition to operands that appear after a `"--"` separator) collects tokens that do not look
#'     like flags and were not consumed as a flag argument, while still erroring on
#' unrecognized flags.
#'
#'   Operands are stored in the `"operand"` attribute of the
#' result and can be retrieved with [getoperand()].
#' @return [getopt()] returns a list data structure containing names of the
#'   flags that were present in the character vector passed in under
#'   the `opt` argument.  Each value of the list is coerced to the
#'   data type specified according to the value of the `spec` argument.  See
#'   the \dQuote{Details} section for more information.
#'   Any positional arguments (operands) are stored in its `"operand"` attribute
#'   and can also be accessed by [getoperand()].
#' @author Allen Day and Trevor L. Davis
#' @keywords data
#' @export
#' @examples
#' #!/path/to/Rscript
#' library('getopt')
#' # get options, using the spec as defined by the matrix
#' spec <- matrix(c(
#'   'verbose', 'v', 2, "integer",
#'   'help'   , 'h', 0, "logical",
#'   'count'  , 'c', 1, "integer",
#'   'mean'   , 'm', 1, "double",
#'   'sd'     , 's', 1, "double"
#' ), byrow = TRUE, ncol = 4L)
#' opt <- getopt(spec)
#'
#' # if help was asked for print a friendly message and exit
#' if (isTRUE(opt$help)) {
#'   cat(getusage(spec))
#'   q(status = 0)
#' }
#'
#' # set reasonable defaults for options that were not specified
#' if (is.null(opt$mean)) opt$mean <- 0
#' if (is.null(opt$sd)) opt$sd <- 1
#' if (is.null(opt$count)) opt$count <- 10L
#' if (is.null(opt$verbose)) opt$verbose <- FALSE
#'
#' # print some progress messages to stderr, if requested
#' if (opt$verbose) write("writing...", stderr())
#'
#' # do some operation based on user input
#' cat(rnorm(opt$count, mean = opt$mean, sd = opt$sd), sep = "\n")
getopt <- function(
	spec = NULL,
	opt = NULL,
	command = getfile(),
	usage = FALSE,
	debug = FALSE,
	operand = "after--only"
) {
	operand <- match.arg(operand, c("after--only", "strict"))
	if (usage) {
		return(getusage(spec, command))
	}

	# littler compatibility - map argv vector to opt
	if (is.null(opt)) {
		if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
			opt <- get("argv", envir = .GlobalEnv) # nocov
		} else {
			opt <- commandArgs(TRUE)
		}
	}

	dashdash <- match("--", opt, nomatch = NA_integer_)
	if (!is.na(dashdash)) {
		dashdash_operands <- opt[seq_len(length(opt) - dashdash) + dashdash]
		opt <- opt[seq_len(dashdash - 1L)]
		if (debug) {
			cat(
				"extracted positional args after `--`:",
				paste(dashdash_operands, collapse = " "),
				"\n"
			)
		}
	} else {
		dashdash_operands <- NULL
	}
	inline_operands <- NULL

	spec <- as_spec(spec)

	opt_normalized <- normalize_opt(opt, spec)
	if (debug && !identical(opt, opt_normalized)) {
		cat("normalized opt to:", paste(opt_normalized, collapse = " "), "\n")
	}
	opt <- opt_normalized

	result <- list()

	i <- 1L

	while (i <= length(opt)) {
		if (debug) {
			cat("processing", opt[i], "\n")
		}

		optstring <- opt[i]

		if (is_long_flag(optstring)) {
			if (debug) {
				cat("\tlong option:", optstring, "\n")
			}

			optstring <- substring(optstring, 3)
			if (grepl("=", optstring)) {
				kv <- strsplit(optstring, "=")[[1L]]
				this_flag <- kv[1L]
				this_argument <- paste(kv[-1L], collapse = "=")
			} else {
				this_flag <- optstring
				this_argument <- NA
			}

			rowmatch <- get_rowmatch(spec, long = this_flag)

			# if we have an argument embedded via '='
			if (!is.na(this_argument)) {
				long_name <- spec[rowmatch, COL_LONG_NAME]
				action <- spec[rowmatch, COL_ACTION]
				# if we can't accept the argument, bail out
				if (action %in% c("store_true", "store_false", "count")) {
					stop(paste0('long flag "', this_flag, '" accepts no arguments'))
				} else if (action == "append") {
					result[[long_name]] <- c(result[[long_name]], this_argument)
					i <- i + 1L
					next
				} else {
					result[[long_name]] <- this_argument
					i <- i + 1L
					next
				}
			}
		} else if (is_short_flag(optstring)) {
			if (debug) {
				cat("\tshort option:", optstring, "\n")
			}

			this_flag <- substring(optstring, 2L)
			rowmatch <- get_rowmatch(spec, short = this_flag)
		} else if (operand == "strict") {
			if (debug) {
				cat("collecting operand:", sQuote(optstring), "\n")
			}
			inline_operands <- c(inline_operands, optstring)
			i <- i + 1L
			next
		} else {
			stop(paste0('"', optstring, '" is not a valid option, or does not support an argument'))
		}

		long_name <- spec[rowmatch, COL_LONG_NAME]
		action <- spec[rowmatch, COL_ACTION]

		if (action == "count") {
			result[long_name] <- (result[[long_name]] %||% 0L) + 1L
			i <- i + 1L
			next
		} else if (action %in% c("store_true", "store_false")) {
			result[long_name] <- action != "store_false"
			i <- i + 1L
			next
		} else {
			# store or store_optional: peek ahead for argument
			if (debug) {
				if (action == "store") {
					cat("\t\trequires a argument\n")
				} else {
					cat("\t\toptionally takes an argument\n")
				}
			}
			if (length(opt) > i) {
				peek_optstring <- opt[i + 1L]
				if (debug) {
					cat("\t\t\tpeeking ahead at:", sQuote(peek_optstring), "\n")
				}

				if (
					!is_long_flag(peek_optstring) &&
						(is_negative_number(peek_optstring) || !is_short_flag(peek_optstring))
				) {
					if (debug) {
						cat("\t\t\t\tconsuming argument", sQuote(peek_optstring), "\n")
					}
					if (action == "append") {
						result[[long_name]] <- c(result[[long_name]], peek_optstring)
					} else {
						result[[long_name]] <- peek_optstring
					}
					i <- i + 2L
					next
				}
			} else if (debug) {
				cat("\t\t\tnothing ahead\n")
			}
			if (debug) {
				cat("\t\t\t\tno argument!\n")
			}
			if (action %in% c("store", "append")) {
				stop(paste0('flag `', this_flag, '` requires an argument'))
			} else {
				# action == "store_optional"
				result[long_name] <- TRUE
			}
		}
		i <- i + 1L
	}

	for (j in seq_len(nrow(spec))) {
		long_name <- spec[j, COL_LONG_NAME]
		if (is.character(result[[long_name]])) {
			mode <- spec[j, COL_MODE]
			val <- result[[long_name]]
			tryCatch(
				storage.mode(result[[long_name]]) <- mode,
				warning = function(w) {
					warning(paste(mode, "expected, got", dQuote(val)))
				}
			)
			if (any(is.na(result[[long_name]]) & !is.na(val))) {
				warning(paste(
					mode,
					"expected, got",
					paste(dQuote(val[is.na(result[[long_name]]) & !is.na(val)]), collapse = ", ")
				))
			}
		}
	}
	structure(result, class = "getopt", operand = c(inline_operands, dashdash_operands))
}

#' Extract positional arguments from a getopt result
#'
#' Extracts the positional arguments stored in the `"operands"` attribute of the
#' object returned by [getopt()].
#'
#' @param x An object of class `"getopt"` as returned by [getopt()].
#' @return A character vector of positional arguments.
#' @export
#' @examples
#' spec <- matrix(c("verbose", "v", 0, "logical"), ncol = 4, byrow = TRUE)
#' opt <- getopt(spec, c("--verbose", "--", "file1.txt", "file2.txt"))
#' getoperand(opt)
getoperand <- function(x) {
	stopifnot(inherits(x, "getopt"))
	attr(x, "operand")
}

#' Generate a usage string
#'
#' Generate a usage string from a getopt `spec` matrix.
#'
#' @inheritParams getopt
#' @param usage A template string for the usage line.  `"%command"` is replaced
#'   by the value of `command` and `"%options"` is replaced by the computed
#'   options string.
#' @return A character string with the usage message.
#' @export
#' @examples
#' spec <- matrix(c(
#'   'verbose', 'v', 2, "integer",
#'   'help'   , 'h', 0, "logical",
#'   'count'  , 'c', 1, "integer",
#'   'mean'   , 'm', 1, "double",
#'   'sd'     , 's', 1, "double"
#' ), byrow = TRUE, ncol = 4)
#' cat(getusage(spec, command = "myscript"))
#' cat(getusage(spec, command = "myscript",
#'              usage = "Usage: %command %options FILE"))
getusage <- function(spec, command = getfile(), usage = "Usage: %command %options") {
	spec <- as_spec(spec)

	options <- ""
	for (j in seq_len(nrow(spec))) {
		options <- paste0(
			options,
			" [-[-",
			spec[j, COL_LONG_NAME],
			"|",
			spec[j, COL_SHORT_NAME],
			"]"
		)
		if (spec[j, COL_ACTION] %in% c("store_true", "store_false", "count")) {
			options <- paste0(options, "]")
		} else if (spec[j, COL_ACTION] %in% c("store", "append")) {
			options <- paste0(options, " <", spec[j, COL_MODE], ">]")
		} else if (spec[j, COL_ACTION] == "store_optional") {
			options <- paste0(options, " [<", spec[j, COL_MODE], ">]]")
		}
	}
	options <- trimws(options)
	usage <- gsub("%command", format(command), usage, fixed = TRUE)
	usage <- gsub("%options", options, usage, fixed = TRUE)
	ret <- paste0(usage, "\n")
	# include usage strings
	if (ncol(spec) >= 5L) {
		max.long <- max(nchar(spec[, COL_LONG_NAME]))
		for (j in seq_len(nrow(spec))) {
			ret <- paste0(
				ret,
				sprintf(
					paste0("    -%s|--%-", max.long, "s    %s\n"),
					spec[j, COL_SHORT_NAME],
					spec[j, COL_LONG_NAME],
					spec[j, COL_DESCRIPTION]
				)
			)
		}
	}
	ret
}

as_spec <- function(spec) {
	if (is.data.frame(spec)) {
		spec <- as.matrix(spec)
	} else if (is.vector(spec) && is.character(spec) && length(spec) %% 4L == 0L) {
		warning(
			'argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion'
		)
		spec <- matrix(spec, ncol = 4L, byrow = TRUE)
	} else if (!is.matrix(spec)) {
		stop(
			'argument "spec" must be a matrix, data frame, or character vector with length divisible by 4.'
		)
	}

	if (ncol(spec) < 4L) {
		stop('"spec" should have at least 4 columns.')
	} else if (ncol(spec) > 6L) {
		stop('"spec" should have no more than 6 columns.')
	}

	# sanity check.  make sure long names are unique, and short names are unique.
	if (anyDuplicated(spec[, COL_LONG_NAME])) {
		stop(paste0("redundant long names for flags (column ", COL_LONG_NAME, " of spec matrix)."))
	}
	if (anyDuplicated(na_omit(spec[, COL_SHORT_NAME]))) {
		stop(paste0(
			"redundant short names for flags (column ",
			COL_SHORT_NAME,
			" of spec matrix)."
		))
	}

	# normalize legacy numeric action values to action strings
	spec[, COL_ACTION] <- gsub("0", "store_true", spec[, COL_ACTION], fixed = TRUE)
	spec[, COL_ACTION] <- gsub("1", "store", spec[, COL_ACTION], fixed = TRUE)
	spec[, COL_ACTION] <- gsub("2", "store_optional", spec[, COL_ACTION], fixed = TRUE)

	valid_actions <- c("append", "count", "store", "store_false", "store_optional", "store_true")
	bad_actions <- setdiff(spec[, COL_ACTION], valid_actions)
	if (length(bad_actions) > 0L) {
		stop(paste0(
			'column ',
			COL_ACTION,
			' of "spec" contains invalid action(s): ',
			paste(dQuote(bad_actions), collapse = ", "),
			'.  Valid actions: ',
			paste(dQuote(valid_actions), collapse = ", "),
			"."
		))
	}

	# convert numeric type to double type
	spec[, COL_MODE] <- gsub("numeric", "double", spec[, COL_MODE], fixed = TRUE)

	valid_modes <- c("character", "complex", "double", "integer", "logical")
	bad_modes <- setdiff(spec[, COL_MODE], valid_modes)
	if (length(bad_modes) > 0L) {
		stop(paste0(
			'column ',
			COL_MODE,
			' of "spec" contains invalid mode(s): ',
			paste(dQuote(bad_modes), collapse = ", "),
			'.  Valid modes: ',
			paste(dQuote(valid_modes), collapse = ", "),
			"."
		))
	}

	spec
}
