test_that("getopt works as expected", {
	# fmt: skip
	spec <- matrix(c(
		"verbose", "v", 2, "integer",
		"help"   , "h", 0, "logical",
		"dummy1" , "d", 0, "logical",
		"dummy2" , "e", 2, "logical",
		"count"  , "c", 1, "integer",
		"mean"   , "m", 1, "double",
		"sd"     , "s", 1, "double",
		"output" , "O", 1, "character"
	), ncol = 4, byrow = TRUE)
	expect_equal(
		sort_list(getopt(spec, c("-c", "-1", "-m", "-1.2"))),
		sort_list(list(ARGS = character(0), count = -1, mean = -1.2))
	)
	expect_equal(
		sort_list(getopt(spec, c("-v", "-m", "3"))),
		sort_list(list(ARGS = character(0), verbose = 1, mean = 3))
	)
	expect_equal(
		sort_list(getopt(spec, c("-m", "3", "-v"))),
		sort_list(list(ARGS = character(0), mean = 3, verbose = 1))
	)
	expect_equal(
		sort_list(getopt(spec, c("-m", "3", "-v", "2", "-v"))),
		sort_list(list(ARGS = character(0), mean = 3, verbose = 1))
	)
	expect_equal(
		sort_list(getopt(spec, c("-O", "-", "-m", "3"))),
		sort_list(list(ARGS = character(0), output = "-", mean = 3))
	)
	expect_equal(
		sort_list(getopt(spec, c("-O", "-", "-m", "3"))),
		sort_list(getopt(spec, c("-m", "3", "-O", "-")))
	)
	expect_equal(sort_list(getopt(spec, c("-de"))), sort_list(getopt(spec, c("-ed"))))
	expect_equal(
		sort_list(getopt(spec, c("-de"))),
		sort_list(list(ARGS = character(0), dummy1 = TRUE, dummy2 = TRUE))
	)
	expect_equal(
		sort_list(getopt(spec, c("-de", "1"))),
		sort_list(list(ARGS = character(0), dummy1 = TRUE, dummy2 = NA))
	)
	expect_equal(
		sort_list(getopt(spec, c("--verbose"))),
		sort_list(list(ARGS = character(0), verbose = 1))
	)
	expect_equal(
		sort_list(getopt(spec, c("--verbose", "--help"))),
		sort_list(list(ARGS = character(0), verbose = 1, help = TRUE))
	)
	expect_equal(
		sort_list(getopt(spec, c("--verbose", "--mean", "5"))),
		sort_list(list(ARGS = character(0), verbose = 1, mean = 5))
	)
	expect_equal(
		sort_list(getopt(spec, c("--mean=5"))),
		sort_list(list(ARGS = character(0), mean = 5))
	)
	expect_equal(
		sort_list(getopt(spec, c("--verbose", "--mean=5", "--sd", "5"))),
		sort_list(list(ARGS = character(0), verbose = 1, mean = 5, sd = 5))
	)
	expect_equal(
		sort_list(getopt(spec, c("--verbose", "--mean=5", "--sd", "5"))),
		sort_list(getopt(spec, c("--mean=5", "--sd", "5", "--verbose")))
	)

	# fmt: skip
	spec <- c(
		"date"     , "d", 1, "character",
		"help"     , "h", 0, "logical",
		"getdata"  , "g", 0, "logical",
		"market"   , "m", 1, "character",
		"threshold", "t", 1, "double"
	)
	spec2 <- matrix(spec, ncol = 4, byrow = TRUE)
	# should give warning is spec is not matrix
	expect_warning(getopt(spec, c("--date", "20080421", "--market", "YM", "--getdata")))
	expect_equal(
		sort_list(getopt(spec2, c("--date", "20080421", "--market", "YM", "--getdata"))),
		sort_list(list(ARGS = character(0), date = "20080421", market = "YM", getdata = TRUE))
	)
	expect_equal(
		sort_list(getopt(spec2, c("--date", "20080421", "--market", "YM", "--getdata"))),
		sort_list(getopt(spec2, c("--date", "20080421", "--getdata", "--market", "YM")))
	)
	expect_snapshot(cat(getopt(spec2, usage = TRUE)))
})
test_that("numeric is cast to double", {
	# Feature reported upstream (optparse) by Miroslav Posta
	spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
	opt <- getopt(spec, c("-c", "-55"))
	expect_equal(typeof(opt$count), "integer")

	spec <- matrix(c("count", "c", 1, "numeric"), ncol = 4, byrow = TRUE)
	opt <- getopt(spec, c("-c", "-55.0"))
	expect_equal(typeof(opt$count), "double")
})
test_that("action strings work as alternatives to 0/1/2", {
	# fmt: skip
	spec <- matrix(c(
		"foo", "f", "store_true"    , "logical",
		"bar", "b", "store"         , "character",
		"biz", "z", "store_optional", "logical"
	), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, "-f")$foo, TRUE)
	expect_equal(getopt(spec, c("-b", "hello"))$bar, "hello")
	expect_equal(getopt(spec, "-z")$biz, TRUE)
})
test_that("data.frame spec is coerced to matrix", {
	spec <- as.data.frame(matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE))
	expect_equal(getopt(spec, c("-c", "5"))$count, 5L)
})
test_that("empty strings are handled correctly for mandatory character arguments", {
	# fmt: skip
	spec <- matrix(c(
		"string", "s", 1, "character",
		"number", "n", 1, "numeric"
	), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("--string=foo"))$string, "foo")
	expect_equal(getopt(spec, c("--string="))$string, "")
	expect_warning(getopt(spec, c("--number=")))
})

test_that("negative numbers are handled correctly", {
	# Issue if negative number preceded by space instead of '=' reported by Roman Zenka
	spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("-c", "5"))$count, 5)
	spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("-c", "-5"))$count, -5)
	spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("--count=-1E5"))$count, -1E5)
	spec <- matrix(c("count", "c", 1, "double"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("-c", "-5"))$count, -5)
	spec <- matrix(c("count", "c", 1, "double"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("-c", "-1e5"))$count, -1e5)
	spec <- matrix(c("count", "c", 1, "double"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("--count=-1.2e5"))$count, -1.2e5)
	spec <- matrix(c("count", "c", 1, "double"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("--count", "-1e5"))$count, -1e5)
})

test_that("more helpful warnings upon incorrect input", {
	# Give more pointed warning upon wildly incorrect input
	expect_snapshot({
		spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
		getopt(spec, c("-c", "hello"))$count
	})

	expect_snapshot(
		{
			spec <- NULL
			getopt(spec, "")

			spec <- c("foo", "f", 0)
			getopt(spec, "")

			spec <- matrix(c("foo", "f", 0, "integer"), ncol = 2)
			getopt(spec, "")

			spec <- matrix(c("foo", "f", 0, "integer", "bar", "b", 0, "integer"), ncol = 8)
			getopt(spec, "")

			spec <- matrix(c("foo", "f", "store_sideways", "logical"), ncol = 4, byrow = TRUE)
			getopt(spec, "")

			spec <- matrix(c("foo", "f", 0, "bignum"), ncol = 4, byrow = TRUE)
			getopt(spec, "")
		},
		error = TRUE
	)
})

test_that("don't throw error if multiple matches match one argument fully", {
	# test if partial name matches fully,
	# still throw error if multiple matches and doesn't match both fully
	# feature request from Jonas Zimmermann
	# fmt: skip
	spec <- matrix(c(
		"foo"   , "f", 0, "logical",
		"foobar", "b", 0, "logical",
		"biz"   , "z", 0, "logical"
	), ncol = 4, byrow = TRUE)
	expect_error(getopt(spec, c("--fo")))
	expect_equal(getopt(spec, c("--foo")), sort_list(list(ARGS = character(0), foo = TRUE)))
})

test_that("sort_list works as expected", {
	expect_equal(sort_list(list(a = 3, b = 2)), sort_list(list(b = 2, a = 3)))
	expect_false(identical(sort_list(list(b = 3, a = 2)), list(b = 3, a = 2)))
	expect_false(identical(
		sort_list(list(b = list(b = 3, c = 2), a = 2)),
		list(b = list(c = 2, b = 3), a = 2)
	))
})

test_that("Use h flag for non help", {
	spec <- matrix(c("foo", "h", 0, "logical"), ncol = 4, byrow = TRUE)
	expect_equal(getopt(spec, c("-h")), sort_list(list(ARGS = character(0), foo = TRUE)))

	spec <- matrix(c("foo", "h", 0, "logical", "help", "h", 0, "logical"), ncol = 4, byrow = TRUE)
	expect_error(getopt(spec, c("-h")), "redundant short names for flags")

	spec <- matrix(c("foo", "f", 0, "logical", "foo", "h", 0, "logical"), ncol = 4, byrow = TRUE)
	expect_error(getopt(spec, c("-h")), "redundant long names for flags")
})

test_that("Optional usage strings work as expected", {
	# fmt: skip
	spec <- matrix(c(
		"foo"   , "f", 0, "logical"  , "foo usage",
		"foobar", "b", 1, "character", "foobar usage",
		"biz"   , "z", 2, "logical"  , "biz usage",
		"number", "n", 1, "numeric"  , "number usage",
		"help"  , "h", 0, "logical"  , "help"
	), ncol = 5, byrow = TRUE)
	expect_snapshot(cat(getopt(spec, usage = TRUE)))
})

test_that("tests to get coverage up", {
	# fmt: skip
	spec <- matrix(c(
		"foo"   , "f", 0, "logical"  , "foo usage",
		"foobar", "b", 1, "character", "foobar usage",
		"biz"   , "z", 2, "logical"  , "biz usage",
		"number", "n", 1, "numeric"  , "number usage",
		"help"  , "h", 0, "logical"  , "help"
	), ncol = 5, byrow = TRUE)
	expect_error(getopt(spec, "--whodunit"), 'long flag "whodunit" is invalid')

	expect_error(getopt(spec, "--foo=4"), 'long flag "foo" accepts no arguments')

	# expect_error(getopt(spec, c("--foo", "4")), 'long flag "foo" accepts no arguments') # nolint

	expect_equal(getopt(spec, "--biz", "4"), sort_list(list(ARGS = character(0), biz = TRUE)))

	expect_warning(getopt(spec, c("-n", "bar")), paste("double expected, got", dQuote("bar")))

	expect_warning(getopt(spec, c("--number=bar")), paste("double expected, got", dQuote("bar")))

	expect_error(getopt(spec, "-n"), 'flag "n" requires an argument')

	expect_error(getopt(spec, "-p"), 'short flag "p" is invalid')

	expect_error(getopt(spec, "-nh"), 'short flag "n" requires an argument, but has none')
	expect_error(getopt(spec, "-fn"), 'flag "n" requires an argument')

	expect_error(
		getopt(spec, c("--number", 3, 4)),
		'"4" is not a valid option, or does not support an argument'
	)
	expect_error(getopt(spec, c("-b")), '"b" requires an argument')
	expect_error(getopt(spec, c("--foobar")), 'flag "foobar" requires an argument')
	expect_error(getopt(spec, c("--foobar", "--help")), 'flag "foobar" requires an argument')

	expect_warning(getopt(spec, c("-n", "-")), paste("double expected, got", dQuote("-")))
})
test_that("debug output", {
	spec <- matrix(
		c("foo", "f", 0L, "logical", "bar", "b", 1L, "character", "num", "n", 2L, "double"),
		ncol = 4L,
		byrow = TRUE
	)
	expect_snapshot(. <- getopt(spec, c("--foo", "--bar", "baz"), debug = TRUE))
	expect_snapshot(. <- getopt(spec, c("-fn", "2"), debug = TRUE))
	expect_snapshot(. <- getopt(spec, c("-b", "-"), debug = TRUE))
})
