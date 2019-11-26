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
#' getopt is primarily intended to be used with ``\link{Rscript}''.  It
#' facilitates writing ``\#!'' shebang scripts that accept short and long
#' flags/options.  It can also be used from ``R'' directly, but is probably less
#' useful in this context.
#'
#' getopt() returns a \link{list} data structure containing \link{names} of the
#' flags that were present in the \link{character} \link{vector} passed in under
#' the \emph{opt} argument.  Each value of the \link{list} is coerced to the
#' data type specified according to the value of the \emph{spec} argument.  See
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
#' ``--''.  If the \emph{long flag} has an associated \emph{argument}, it may be
#' delimited from the \emph{long flag} by either a trailing \emph{=}, or may be
#' the subsequent \emph{option}.
#'
#' 5. A \emph{short flag} is a type of \emph{flag}, and begins with the string
#' ``-''.  If a \emph{short flag} has an associated \emph{argument}, it is the
#' subsequent \emph{option}.  \emph{short flags} may be bundled together,
#' sharing a single leading ``-'', but only the final \emph{short flag} is able
#' to have a corresponding \emph{argument}.
#'
#' Many users wonder whether they should use the getopt package, optparse package,
#' or argparse package.
#' Here is some of the major differences:
#'
#' Features available in \code{getopt} unavailable in \code{optparse}
#'
#' 1. As well as allowing one to specify options that take either
#'      no argument or a required argument like \code{optparse},
#'    \code{getopt} also allows one to specify option with an optional argument.
#'
#' Some features implemented in \code{optparse} package unavailable in \code{getopt}
#'
#' 1. Limited support for capturing positional arguments after the optional arguments
#' when \code{positional_arguments} set to TRUE in \code{parse_args}
#'
#' 2. Automatic generation of an help option and printing of help text when encounters an "-h"
#'
#' 3. Option to specify default arguments for options as well the
#'    variable name to store option values
#'
#' There is also new package \code{argparse} introduced in 2012 which contains
#' all the features of both getopt and optparse but which has a dependency on
#' Python 2.7 or 3.2+ and has not been used in production since 2008 or 2009
#' like the getopt and optparse packages.
#'
#' Some Features unlikely to be implemented in \code{getopt}:
#'
#' 1. Support for multiple, identical flags, e.g. for "-m 3 -v 5 -v", the
#' trailing "-v" overrides the preceding "-v 5", result is v=TRUE (or equivalent
#' typecast).
#'
#' 2. Support for multi-valued flags, e.g. "--libpath=/usr/local/lib
#' --libpath=/tmp/foo".
#'
#' 3. Support for lists, e.g. "--define os=linux --define os=redhat" would
#' set result$os$linux=TRUE and result$os$redhat=TRUE.
#'
#' 4. Support for incremental, argument-less flags, e.g. "/path/to/script
#' -vvv" should set v=3.
#'
#' 5. Support partial-but-unique string match on options, e.g. "--verb" and
#' "--verbose" both match long flag "--verbose".
#'
#' 6. No support for mixing in positional arguments or extra arguments that
#' don't match any options.  For example, you can't do "my.R --arg1 1 foo bar
#' baz" and recover "foo", "bar", "baz" as a list.  Likewise for "my.R foo
#' --arg1 1 bar baz".
#'
#' @aliases getopt getopt-package
#' @param spec The getopt specification, or spec of what options are considered
#' valid.  The specification must be either a 4-5 column \link{matrix}, or a
#' \link{character} \link{vector} coercible into a 4 column \link{matrix} using
#' \link{matrix}(x,ncol=4,byrow=TRUE) command.  The \link{matrix}/\link{vector}
#' contains:
#'
#' Column 1: the \emph{long flag} name.  A multi-\link{character} string.
#'
#' Column 2: \emph{short flag} alias of Column 1.  A single-\link{character}
#' string.
#'
#' Column 3: \emph{Argument} mask of the \emph{flag}.  An \link{integer}.
#' Possible values: 0=no argument, 1=required argument, 2=optional argument.
#'
#' Column 4: Data type to which the \emph{flag}'s argument shall be cast using
#' \link{storage.mode}.  A multi-\link{character} string.  This only considered
#' for same-row Column 3 values of 1,2.  Possible values: \link{logical},
#' \link{integer}, \link{double}, \link{complex}, \link{character}.
#' If \link{numeric} is encountered then it will be converted to double.
#'
#' Column 5 (optional): A brief description of the purpose of the option.
#'
#' The terms \emph{option}, \emph{flag}, \emph{long flag}, \emph{short flag},
#' and \emph{argument} have very specific meanings in the context of this
#' document.  Read the ``Description'' section for definitions.
#' @param opt This defaults to the return value of \link{commandArgs}(TRUE) unless
#'    \code{argv} is in the global environment in which case it uses that instead
#'    (this is for compatibility with littler).
#'
#' If R was invoked directly via the ``R'' command, this corresponds to all
#' arguments passed to R after the ``--args'' flag.
#'
#' If R was invoked via the ``\link{Rscript}'' command, this corresponds to all
#' arguments after the name of the R script file.
#'
#' Read about \link{commandArgs} and \link{Rscript} to learn more.
#' @param command The string to use in the usage message as the name of the
#' script.  See argument \emph{usage}.
#' @param usage If TRUE, argument \emph{opt} will be ignored and a usage
#' statement (character string) will be generated and returned from \emph{spec}.
#' @param debug This is used internally to debug the getopt() function itself.
#' @author Allen Day
#' @seealso \code{\link{getopt}}
#' @keywords data
#' @export
#' @examples
#'
#' #!/path/to/Rscript
#' library('getopt')
#' # get options, using the spec as defined by the enclosed list.
#' # we read the options from the default: commandArgs(TRUE).
#' spec = matrix(c(
#'   'verbose', 'v', 2, "integer",
#'   'help'   , 'h', 0, "logical",
#'   'count'  , 'c', 1, "integer",
#'   'mean'   , 'm', 1, "double",
#'   'sd'     , 's', 1, "double"
#' ), byrow=TRUE, ncol=4)
#' opt = getopt(spec)
#'
#' # if help was asked for print a friendly message
#' # and exit with a non-zero error code
#' if (!is.null(opt$help)) {
#'   cat(getopt(spec, usage = TRUE))
#'   q(status = 1)
#' }
#'
#' # set some reasonable defaults for the options that are needed,
#' # but were not specified.
#' if (is.null(opt$mean)) opt$mean <- 0
#' if (is.null(opt$sd)) opt$sd <- 1
#' if (is.null(opt$count)) opt$count <- 10
#' if (is.null(opt$verbose)) opt$verbose <- FALSE
#'
#' # print some progress messages to stderr, if requested.
#' if (opt$verbose) write("writing...", stderr())
#'
#' # do some operation based on user input.
#' cat(paste(rnorm(opt$count, mean = opt$mean, sd = opt$sd), collapse = "\n"))
#' cat("\n")
#'
#' # signal success and exit.
#' # q(status=0)
#'
#' @import stats
getopt <- function(spec = NULL, opt = NULL, command = get_Rscript_filename(), usage = FALSE, debug = FALSE) { # nolint

  # littler compatibility - map argv vector to opt
  if (is.null(opt)) {
      if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
        opt <- get("argv", envir = .GlobalEnv) # nocov
      } else {
        opt <- commandArgs(TRUE)
      }
  }

  ncol <- 4
  maxcol <- 6
  col_long_name <- 1
  col_short_name <- 2
  col_has_argument <- 3
  col_mode <- 4
  col_description <- 5

  flag_no_argument <- 0
  flag_required_argument <- 1
  flag_optional_argument <- 2

  result <- list()
  result$ARGS <- vector(mode = "character") # nolint

  # no spec.  fail.
  if (is.null(spec)) {
    stop('argument "spec" must be non-null.')

  # spec is not a matrix.  attempt to coerce, if possible.  issue a warning.
  } else if (!is.matrix(spec)) {
    if (length(spec) / 4 == as.integer(length(spec) / 4)) {
      warning('argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion')
      spec <- matrix(spec, ncol = ncol, byrow = TRUE)
    } else {
      stop('argument "spec" must be a matrix, or a character vector with length divisible by 4, rtfm.')
    }

  # spec is a matrix, but it has too few columns.
  } else if (dim(spec)[2] < ncol) {
    stop(paste('"spec" should have at least ', ncol, " columns.", sep = ""))

  # spec is a matrix, but it has too many columns.
  } else if (dim(spec)[2] > maxcol) {
    stop(paste('"spec" should have no more than ', maxcol, " columns.", sep = ""))

  # spec is a matrix, and it has some optional columns.
  } else if (dim(spec)[2] != ncol) {
    ncol <- dim(spec)[2]
  }

  # sanity check.  make sure long names are unique, and short names are unique.
  if (length(unique(spec[, col_long_name])) != length(spec[, col_long_name])) {
    stop(paste("redundant long names for flags (column ", col_long_name, " of spec matrix).", sep = ""))
  }
  if (length(stats::na.omit(unique(spec[, col_short_name]))) != length(stats::na.omit(spec[, col_short_name]))) {
    stop(paste("redundant short names for flags (column ", col_short_name, " of spec matrix).", sep = ""))
  }
  # convert numeric type to double type
  spec[, 4] <- gsub("numeric", "double", spec[, 4])

  # if usage=TRUE, don't process opt, but generate a usage string from the data in spec
  if (usage) {
    ret <- ""
    ret <- paste(ret, "Usage: ", command, sep = "")
    for (j in 1:(dim(spec))[1]) {
      ret <- paste(ret, " [-[-", spec[j, col_long_name], "|", spec[j, col_short_name], "]", sep = "")
      if (spec[j, col_has_argument] == flag_no_argument) {
        ret <- paste(ret, "]", sep = "")
      } else if (spec[j, col_has_argument] == flag_required_argument) {
        ret <- paste(ret, " <", spec[j, col_mode], ">]", sep = "")
      } else if (spec[j, col_has_argument] == flag_optional_argument) {
        ret <- paste(ret, " [<", spec[j, col_mode], ">]]", sep = "")
      }
    }
    # include usage strings
    if (ncol >= 5) {
      max.long <- max(apply(cbind(spec[, col_long_name]), 1, function(x)length(strsplit(x, "")[[1]])))
      ret <- paste(ret, "\n", sep = "")
      for (j in 1:(dim(spec))[1]) {
        ret <- paste(ret, sprintf(paste("    -%s|--%-", max.long, "s    %s\n",  sep = ""),
          spec[j, col_short_name], spec[j, col_long_name], spec[j, col_description]
        ), sep = "")
      }
    }
    else {
      ret <- paste(ret, "\n", sep = "")
    }
    return(ret)
  }

  # XXX check spec validity here.  e.g. column three should be convertible to integer

  i <- 1

  while (i <= length(opt)) {
    if (debug) print(paste("processing", opt[i]))

    current_flag <- 0 # XXX use NA
    optstring <- opt[i]


    # long flag
    if (substr(optstring, 1, 2) == "--") {
      if (debug) print(paste("  long option:", opt[i]))

      optstring <- substring(optstring, 3)

      this_flag <- NA
      this_argument <- NA
      kv <- strsplit(optstring, "=")[[1]]
      # if (!is.na(kv[2])) {
      if (grepl("=", optstring)) {
        this_flag <- kv[1]
        this_argument <- paste(kv[-1], collapse = "=")
      } else {
        this_flag <- optstring
      }

      rowmatch <- grep(this_flag, spec[, col_long_name], fixed = TRUE)

      # long flag is invalid, matches no options
      if (length(rowmatch) == 0) {
        stop(paste('long flag "', this_flag, '" is invalid', sep = ""))

      # long flag is ambiguous, matches too many options
      } else if (length(rowmatch) > 1) {
        # check if there is an exact match and use that
        rowmatch <- which(this_flag == spec[, col_long_name])
        if (length(rowmatch) == 0) {
          stop(paste('long flag "', this_flag, '" is ambiguous', sep = ""))
        }
      }

      # if we have an argument
      if (!is.na(this_argument)) {
        # if we can't accept the argument, bail out
        if (spec[rowmatch, col_has_argument] == flag_no_argument) {
          stop(paste('long flag "', this_flag, '" accepts no arguments', sep = ""))

        # otherwise assign the argument to the flag
        } else {
          mode <- spec[rowmatch, col_mode]
          warning_msg <- tryCatch(storage.mode(this_argument) <- mode,
                   warning = function(w) warning(paste(mode, "expected, got", dQuote(this_argument))))
          if (is.na(this_argument) && !grepl("expected, got",  warning_msg)) {
              warning(paste("long flag", this_flag, "given a bad argument"))
          }
          result[spec[rowmatch, col_long_name]] <- this_argument
          i <- i + 1
          next
        }

      # otherwise, we don't have an argument
      } else {
        # nolint start
        # if we require an argument, bail out
        ### if (spec[rowmatch, col_has_argument] == flag_required_argument) {
        ###  stop(paste('long flag "', this_flag, '" requires an argument', sep = ""))

        # long flag has no attached argument. set flag as present.  
        # set current_flag so we can peek ahead later and consume the argument if it's there
        # nolint end
        ###} else {
          result[spec[rowmatch, col_long_name]] <- TRUE
          current_flag <- rowmatch
        ###}
      }

    # short flag(s)
    } else if (substr(optstring, 1, 1) == "-") {
      if (debug) print(paste("  short option:", opt[i]))

      these_flags <- strsplit(optstring, "")[[1]]

      done <- FALSE
      for (j in 2:length(these_flags)) {
        this_flag <- these_flags[j]
        rowmatch <- grep(this_flag, spec[, col_short_name], fixed = TRUE)

        # short flag is invalid, matches no options
        if (length(rowmatch) == 0) {
          stop(paste('short flag "', this_flag, '" is invalid', sep = ""))

        # short flag has an argument, but is not the last in a compound flag string
        } else if (j < length(these_flags) & spec[rowmatch, col_has_argument] == flag_required_argument) {
          stop(paste('short flag "', this_flag, '" requires an argument, but has none', sep = ""))

        # short flag has no argument, flag it as present
        } else if (spec[rowmatch, col_has_argument] == flag_no_argument) {
          result[spec[rowmatch, col_long_name]] <- TRUE
          done <- TRUE

        # can't definitively process this_flag flag yet, need to see if next option is an argument or not
        } else {
          result[spec[rowmatch, col_long_name]] <- TRUE
          current_flag <- rowmatch
          done <- FALSE
        }
      }
      if (done) {
        i <- i + 1
        next
      }
    }

    # invalid opt
    if (current_flag == 0) {
        stop(paste('"', optstring, '" is not a valid option, or does not support an argument', sep = ""))

        # nolint start
        # TBD support for positional args
        # if (debug) print(paste('"', optstring, '" not a valid option.  It is appended to getopt(...)$ARGS', sep = ""))
        # result$ARGS = append(result$ARGS, optstring)
        # nolint end

    # some dangling flag, handle it
    } else if (current_flag > 0) {
      if (debug) print("    dangling flag")
      if (length(opt) > i) {
        peek_optstring <- opt[i + 1]
        if (debug) print(paste('      peeking ahead at: "', peek_optstring, '"', sep = ""))

        # got an argument.  attach it, increment the index, and move on to the next option.
        # we don't allow arguments beginning with "-" UNLESS specfile indicates the value is an "integer" or "double",
        # in which case we allow a leading dash (and verify trailing digits/decimals).
        if (substr(peek_optstring, 1, 1) != "-" |
          # match negative double
          (substr(peek_optstring, 1, 1) == "-"
          & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek_optstring) > 0
          & spec[current_flag, col_mode] ==  "double"
          ) |
          # match negative integer
          (substr(peek_optstring, 1, 1) == "-"
          & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek_optstring) > 0
          & spec[current_flag, col_mode] ==  "integer"
          )
        ) {
          if (debug) print(paste("        consuming argument *", peek_optstring, "*", sep = ""))

          mode <- spec[current_flag, col_mode]
          tryCatch(storage.mode(peek_optstring) <- mode,
                   warning = function(w) warning(paste(mode, "expected, got", dQuote(peek_optstring))))
          result[spec[current_flag, col_long_name]] <- peek_optstring
          i <- i + 1

        # a lone dash
        } else if (substr(peek_optstring, 1, 1) == "-" & length(strsplit(peek_optstring, "")[[1]]) == 1) {
          if (debug) print('        consuming "lone dash" argument')
          mode <- spec[current_flag, col_mode]
          tryCatch(storage.mode(peek_optstring) <- mode,
                   warning = function(w) warning(paste(mode, "expected, got", dQuote(peek_optstring)))) # nocov
          result[spec[current_flag, col_long_name]] <- peek_optstring
          i <- i + 1

        # no argument
        } else {
          if (debug) print("        no argument!")

          # if we require an argument, bail out
          if (spec[current_flag, col_has_argument] == flag_required_argument) {
            stop(paste('flag "', this_flag, '" requires an argument', sep = ""))

          # otherwise set flag as present.
          } else if (
            spec[current_flag, col_has_argument] == flag_optional_argument |
            spec[current_flag, col_has_argument] == flag_no_argument
          ) {
            x <- TRUE
            storage.mode(x) <- spec[current_flag, col_mode]
            result[spec[current_flag, col_long_name]] <- x
          } else {
            stop(paste("This should never happen.", # nocov
              "Is your spec argument correct?  Maybe you forgot to set", # nocov
              "ncol=4, byrow=TRUE in your matrix call?")) # nocov
          }
        }
      # trailing flag without required argument
      } else if (spec[current_flag, col_has_argument] == flag_required_argument) {
        stop(paste('flag "', this_flag, '" requires an argument', sep = ""))

      # trailing flag without optional argument
      } else if (spec[current_flag, col_has_argument] == flag_optional_argument) {
        x <- TRUE
        storage.mode(x) <- spec[current_flag, col_mode]
        result[spec[current_flag, col_long_name]] <- x

      # trailing flag without argument
      } else if (spec[current_flag, col_has_argument] == flag_no_argument) {
        x <- TRUE
        storage.mode(x) <- spec[current_flag, col_mode]
        result[spec[current_flag, col_long_name]] <- x
      } else {
        stop("this should never happen (2).  please inform the author.") # nocov
      }
    } # no dangling flag, nothing to do.

    i <- i + 1
  }
  return(result)
}
