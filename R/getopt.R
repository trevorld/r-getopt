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
#' [getopt()] returns a list data structure containing names of the
#' flags that were present in the character vector passed in under
#' the `opt` argument.  Each value of the list is coerced to the
#' data type specified according to the value of the `spec` argument.  See
#' below for details.
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
#' 1. As well as allowing one to specify options that take either
#'    no argument or a required argument like `optparse`,
#'    `getopt` also allows one to specify option with an optional argument.
#'
#' Some features implemented in `optparse` package unavailable in `getopt`
#'
#' 1. Limited support for capturing positional arguments after the optional arguments
#' when `positional_arguments` set to `TRUE` in `optparse::parse_args()`
#'
#' 2. Automatic generation of an help option and printing of help text when encounters an `-h`
#'
#' 3. Option to specify default arguments for options as well the
#'    variable name to store option values
#'
#' There is also new package `argparse` introduced in 2012 which contains
#' all the features of both getopt and optparse but which has a dependency on
#' Python 2.7 or 3.2+.
#'
#' Some Features unlikely to be implemented in `getopt`:
#'
#' 1. Support for multiple, identical flags, e.g. for `-m 3 -v 5 -v`, the
#' trailing `-v` overrides the preceding `-v 5`, result is `v=TRUE` (or equivalent
#' typecast).
#'
#' 2. Support for multi-valued flags, e.g. `--libpath=/usr/local/lib
#' --libpath=/tmp/foo`.
#'
#' 3. Support for lists, e.g. `--define os=linux --define os=redhat` would
#' set `result$os$linux=TRUE` and `result$os$redhat=TRUE`.
#'
#' 4. Support for incremental, argument-less flags, e.g. `/path/to/script
#' -vvv` should set `v=3`.
#'
#' 5. Support partial-but-unique string match on options, e.g. `--verb` and
#' `--verbose` both match long flag `--verbose`.
#'
#' 6. No support for mixing in positional arguments or extra arguments that
#' don't match any options.  For example, you can't do `my.R --arg1 1 foo bar
#' baz` and recover `foo`, `bar`, `baz` as a list.  Likewise for `my.R foo
#' --arg1 1 bar baz`.
#'
#' @aliases getopt getopt-package
#' @param spec The getopt specification, or spec of what options are considered
#' valid.  The specification must be either a 4-5 column matrix, or a
#' character vector coercible into a 4 column matrix using
#' `matrix(x,ncol=4,byrow=TRUE)` command.  The matrix/vector
#' contains:
#'
#' Column 1: the \emph{long flag} name.  A multi-character string.
#'
#' Column 2: \emph{short flag} alias of Column 1.  A single-character
#' string.
#'
#' Column 3: \emph{Argument} mask of the \emph{flag}.  An integer.
#' Possible values: 0=no argument, 1=required argument, 2=optional argument.
#'
#' Column 4: Data type to which the \emph{flag}'s argument shall be cast using
#' [storage.mode()].  A multi-character string.  This only considered
#' for same-row Column 3 values of 1,2.  Possible values: logical,
#' integer, double, complex, character.
#' If numeric is encountered then it will be converted to double.
#'
#' Column 5 (optional): A brief description of the purpose of the option.
#'
#' The terms \emph{option}, \emph{flag}, \emph{long flag}, \emph{short flag},
#' and \emph{argument} have very specific meanings in the context of this
#' document.  Read the \dQuote{Description} section for definitions.
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
#' @author Allen Day
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
#' # if help was asked for print a friendly message
#' # and exit with a non-zero error code
#' if (isTRUE(opt$help)) {
#'   cat(getusage(spec))
#'   q(status = 1L)
#' }
#'
#' # set reasonable defaults for options that were not specified
#' if (is.null(opt$mean)) opt$mean <- 0
#' if (is.null(opt$sd)) opt$sd <- 1
#' if (is.null(opt$count)) opt$count <- 10L
#' if (is.null(opt$verbose)) opt$verbose <- 0L
#'
#' # print some progress messages to stderr, if requested
#' if (opt$verbose) write("writing...", stderr())
#'
#' # do some operation based on user input
#' cat(rnorm(opt$count, mean = opt$mean, sd = opt$sd), sep = "\n")
#'
#' # signal success and exit
#' # q(status = 0L)
#' @import stats
getopt <- function(
	spec = NULL,
	opt = NULL,
	command = get_Rscript_filename(),
	usage = FALSE,
	debug = FALSE
) {
	# nolint

	# littler compatibility - map argv vector to opt
	if (is.null(opt)) {
		if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
			opt <- get("argv", envir = .GlobalEnv) # nocov
		} else {
			opt <- commandArgs(TRUE)
		}
	}

	result <- list()
	result$ARGS <- vector(mode = "character") # nolint

	spec <- as_spec(spec)

	if (usage) {
		return(getusage(spec, command))
	}
	i <- 1L

	while (i <= length(opt)) {
		if (debug) {
			print(paste("processing", opt[i]))
		}

		current_flag <- 0L # XXX use NA
		optstring <- opt[i]

		# long flag
		if (substr(optstring, 1, 2) == "--") {
			if (debug) {
				print(paste("  long option:", opt[i]))
			}

			optstring <- substring(optstring, 3)

			this_flag <- NA
			this_argument <- NA
			kv <- strsplit(optstring, "=")[[1L]]
			# if (!is.na(kv[2])) {
			if (grepl("=", optstring)) {
				this_flag <- kv[1]
				this_argument <- paste(kv[-1L], collapse = "=")
			} else {
				this_flag <- optstring
			}

			rowmatch <- grep(this_flag, spec[, COL_LONG_NAME], fixed = TRUE)

			# long flag is invalid, matches no options
			if (length(rowmatch) == 0) {
				stop(paste('long flag "', this_flag, '" is invalid', sep = ""))

				# long flag is ambiguous, matches too many options
			} else if (length(rowmatch) > 1) {
				# check if there is an exact match and use that
				rowmatch <- which(this_flag == spec[, COL_LONG_NAME])
				if (length(rowmatch) == 0) {
					stop(paste('long flag "', this_flag, '" is ambiguous', sep = ""))
				}
			}

			# if we have an argument
			if (!is.na(this_argument)) {
				# if we can't accept the argument, bail out
				if (spec[rowmatch, COL_HAS_ARGUMENT] == FLAG_NO_ARGUMENT) {
					stop(paste('long flag "', this_flag, '" accepts no arguments', sep = ""))

					# otherwise assign the argument to the flag
				} else {
					mode <- spec[rowmatch, COL_MODE]
					warning_msg <- tryCatch(
						storage.mode(this_argument) <- mode,
						warning = function(w) {
							warning(paste(mode, "expected, got", dQuote(this_argument)))
						}
					)
					if (is.na(this_argument) && !grepl("expected, got", warning_msg)) {
						warning(paste("long flag", this_flag, "given a bad argument"))
					}
					result[spec[rowmatch, COL_LONG_NAME]] <- this_argument
					i <- i + 1L
					next
				}

				# otherwise, we don't have an argument
			} else {
				# nolint start
				# if we require an argument, bail out
				### if (spec[rowmatch, COL_HAS_ARGUMENT] == FLAG_REQUIRED_ARGUMENT) {
				###  stop(paste('long flag "', this_flag, '" requires an argument', sep = ""))

				# long flag has no attached argument. set flag as present.
				# set current_flag so we can peek ahead later and consume the argument if it's there
				# nolint end
				###} else {
				result[spec[rowmatch, COL_LONG_NAME]] <- TRUE
				current_flag <- rowmatch
				###}
			}

			# short flag(s)
		} else if (substr(optstring, 1, 1) == "-") {
			if (debug) {
				print(paste("  short option:", opt[i]))
			}

			these_flags <- strsplit(optstring, "")[[1L]]

			done <- FALSE
			for (j in 2L:length(these_flags)) {
				this_flag <- these_flags[j]
				rowmatch <- grep(this_flag, spec[, COL_SHORT_NAME], fixed = TRUE)

				# short flag is invalid, matches no options
				if (length(rowmatch) == 0) {
					stop(paste('short flag "', this_flag, '" is invalid', sep = ""))

					# short flag has an argument, but is not the last in a compound flag string
				} else if (
					j < length(these_flags) &
						spec[rowmatch, COL_HAS_ARGUMENT] == FLAG_REQUIRED_ARGUMENT
				) {
					stop(paste(
						'short flag "',
						this_flag,
						'" requires an argument, but has none',
						sep = ""
					))

					# short flag has no argument, flag it as present
				} else if (spec[rowmatch, COL_HAS_ARGUMENT] == FLAG_NO_ARGUMENT) {
					result[spec[rowmatch, COL_LONG_NAME]] <- TRUE
					done <- TRUE

					# can't definitively process this_flag flag yet, need to see if next option is an argument or not
				} else {
					result[spec[rowmatch, COL_LONG_NAME]] <- TRUE
					current_flag <- rowmatch
					done <- FALSE
				}
			}
			if (done) {
				i <- i + 1L
				next
			}
		}

		# invalid opt
		if (current_flag == 0L) {
			stop(paste(
				'"',
				optstring,
				'" is not a valid option, or does not support an argument',
				sep = ""
			))

			# nolint start
			# TBD support for positional args
			# if (debug) print(paste('"', optstring, '" not a valid option.  It is appended to getopt(...)$ARGS', sep = ""))
			# result$ARGS = append(result$ARGS, optstring)
			# nolint end

			# some dangling flag, handle it
		} else if (current_flag > 0L) {
			if (debug) {
				print("    dangling flag")
			}
			if (length(opt) > i) {
				peek_optstring <- opt[i + 1L]
				if (debug) {
					print(paste('      peeking ahead at: "', peek_optstring, '"', sep = ""))
				}

				# got an argument.  attach it, increment the index, and move on to the next option.
				# we don't allow arguments beginning with "-" UNLESS specfile indicates the value is an "integer" or "double",
				# in which case we allow a leading dash (and verify trailing digits/decimals).
				if (
					substr(peek_optstring, 1, 1) != "-" |
						# match negative double
						(substr(peek_optstring, 1, 1) == "-" &
							regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek_optstring) > 0 &
							spec[current_flag, COL_MODE] == "double") |
						# match negative integer
						(substr(peek_optstring, 1, 1) == "-" &
							regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek_optstring) > 0 &
							spec[current_flag, COL_MODE] == "integer")
				) {
					if (debug) {
						print(paste("        consuming argument *", peek_optstring, "*", sep = ""))
					}

					mode <- spec[current_flag, COL_MODE]
					tryCatch(storage.mode(peek_optstring) <- mode, warning = function(w) {
						warning(paste(mode, "expected, got", dQuote(peek_optstring)))
					})
					result[spec[current_flag, COL_LONG_NAME]] <- peek_optstring
					i <- i + 1L

					# a lone dash
				} else if (
					substr(peek_optstring, 1, 1) == "-" &
						length(strsplit(peek_optstring, "")[[1L]]) == 1L
				) {
					if (debug) {
						print('        consuming "lone dash" argument')
					}
					mode <- spec[current_flag, COL_MODE]
					tryCatch(storage.mode(peek_optstring) <- mode, warning = function(w) {
						warning(paste(mode, "expected, got", dQuote(peek_optstring)))
					}) # nocov
					result[spec[current_flag, COL_LONG_NAME]] <- peek_optstring
					i <- i + 1L

					# no argument
				} else {
					if (debug) {
						print("        no argument!")
					}

					# if we require an argument, bail out
					if (spec[current_flag, COL_HAS_ARGUMENT] == FLAG_REQUIRED_ARGUMENT) {
						stop(paste('flag "', this_flag, '" requires an argument', sep = ""))

						# otherwise set flag as present.
					} else if (
						spec[current_flag, COL_HAS_ARGUMENT] == FLAG_OPTIONAL_ARGUMENT |
							spec[current_flag, COL_HAS_ARGUMENT] == FLAG_NO_ARGUMENT
					) {
						x <- TRUE
						storage.mode(x) <- spec[current_flag, COL_MODE]
						result[spec[current_flag, COL_LONG_NAME]] <- x
					} else {
						stop(paste(
							"This should never happen.", # nocov
							"Is your spec argument correct?  Maybe you forgot to set", # nocov
							"ncol=4, byrow=TRUE in your matrix call?"
						)) # nocov
					}
				}
				# trailing flag without required argument
			} else if (spec[current_flag, COL_HAS_ARGUMENT] == FLAG_REQUIRED_ARGUMENT) {
				stop(paste('flag "', this_flag, '" requires an argument', sep = ""))

				# trailing flag without optional argument
			} else if (spec[current_flag, COL_HAS_ARGUMENT] == FLAG_OPTIONAL_ARGUMENT) {
				x <- TRUE
				storage.mode(x) <- spec[current_flag, COL_MODE]
				result[spec[current_flag, COL_LONG_NAME]] <- x

				# trailing flag without argument
			} else if (spec[current_flag, COL_HAS_ARGUMENT] == FLAG_NO_ARGUMENT) {
				x <- TRUE
				storage.mode(x) <- spec[current_flag, COL_MODE]
				result[spec[current_flag, COL_LONG_NAME]] <- x
			} else {
				stop("this should never happen (2).  please inform the author.") # nocov
			}
		} # no dangling flag, nothing to do.

		i <- i + 1L
	}
	return(result)
}

#' Generate a usage string
#'
#' Generate a usage string from a getopt `spec` matrix.
#'
#' @inheritParams getopt
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
#' cat(getusage(spec))
getusage <- function(spec, command = get_Rscript_filename()) {
	spec <- as_spec(spec)

	ncol <- dim(spec)[2L]

	ret <- ""
	ret <- paste(ret, "Usage: ", command, sep = "")
	for (j in 1L:(dim(spec))[1L]) {
		ret <- paste(
			ret,
			" [-[-",
			spec[j, COL_LONG_NAME],
			"|",
			spec[j, COL_SHORT_NAME],
			"]",
			sep = ""
		)
		if (spec[j, COL_HAS_ARGUMENT] == FLAG_NO_ARGUMENT) {
			ret <- paste(ret, "]", sep = "")
		} else if (spec[j, COL_HAS_ARGUMENT] == FLAG_REQUIRED_ARGUMENT) {
			ret <- paste(ret, " <", spec[j, COL_MODE], ">]", sep = "")
		} else if (spec[j, COL_HAS_ARGUMENT] == FLAG_OPTIONAL_ARGUMENT) {
			ret <- paste(ret, " [<", spec[j, COL_MODE], ">]]", sep = "")
		}
	}
	# include usage strings
	if (ncol >= 5L) {
		max.long <- max(apply(cbind(spec[, COL_LONG_NAME]), 1L, function(x) {
			length(strsplit(x, "")[[1L]])
		}))
		ret <- paste(ret, "\n", sep = "")
		for (j in 1L:(dim(spec))[1L]) {
			ret <- paste(
				ret,
				sprintf(
					paste("    -%s|--%-", max.long, "s    %s\n", sep = ""),
					spec[j, COL_SHORT_NAME],
					spec[j, COL_LONG_NAME],
					spec[j, COL_DESCRIPTION]
				),
				sep = ""
			)
		}
	} else {
		ret <- paste(ret, "\n", sep = "")
	}
	ret
}

as_spec <- function(spec) {
	ncol <- 4L
	maxcol <- 6L

	if (is.null(spec)) {
		stop('argument "spec" must be non-null.')
	} else if (!is.matrix(spec)) {
		if (length(spec) %% 4L == 0L) {
			warning(
				'argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion'
			)
			spec <- matrix(spec, ncol = ncol, byrow = TRUE)
		} else {
			stop(
				'argument "spec" must be a matrix, or a character vector with length divisible by 4, rtfm.'
			)
		}
	} else if (dim(spec)[2L] < ncol) {
		stop(paste('"spec" should have at least ', ncol, " columns.", sep = ""))
	} else if (dim(spec)[2L] > maxcol) {
		stop(paste('"spec" should have no more than ', maxcol, " columns.", sep = ""))
	}

	# sanity check.  make sure long names are unique, and short names are unique.
	if (length(unique(spec[, COL_LONG_NAME])) != length(spec[, COL_LONG_NAME])) {
		stop(paste(
			"redundant long names for flags (column ",
			COL_LONG_NAME,
			" of spec matrix).",
			sep = ""
		))
	}
	if (
		length(stats::na.omit(unique(spec[, COL_SHORT_NAME]))) !=
			length(stats::na.omit(spec[, COL_SHORT_NAME]))
	) {
		stop(paste(
			"redundant short names for flags (column ",
			COL_SHORT_NAME,
			" of spec matrix).",
			sep = ""
		))
	}

	# convert numeric type to double type
	spec[, COL_MODE] <- gsub("numeric", "double", spec[, COL_MODE])

	# XXX check spec validity here.  e.g. column three should be convertible to integer

	spec
}
