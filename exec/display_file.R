#!/usr/bin/env Rscript
# Copyright 2010-2026 Trevor L Davis <trevor.l.davis@gmail.com>
# Copyright 2013 Kirill Müller
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
library("getopt")
options(error = function(e) quit("no", status = 1, runLast = FALSE))

# fmt: skip
spec <- matrix(c(
	"add_numbers", "n", "store_true", "logical", "Print line number at the beginning of each line",
	"help", "h", "store_true", "logical", "Print usage message"
), ncol = 5L, byrow = TRUE)

opt <- getopt(spec, operand = "strict")

if (!is.null(opt$help)) {
	cat(getusage(spec, usage = "Usage: %command %options FILE"))
	quit(status = 1)
}

file <- getoperand(opt)
if (is.null(file) || length(file) != 1L) {
	stop("You must pass in a single positional argument of a file")
}

if (!file.exists(file)) {
	stop(sprintf("Specified file `%s` does not exist", file))
}

file_text <- readLines(file)

if (isTRUE(opt$add_numbers)) {
	cat(paste(seq_along(file_text), file_text), sep = "\n")
} else {
	cat(file_text, sep = "\n")
}
