test_that("is_long_flag() identifies long flags", {
	expect_true(is_long_flag("--verbose"))
	expect_false(is_long_flag("--"))
	expect_false(is_long_flag("-v"))
	expect_false(is_long_flag("foo"))
})

test_that("is_short_flag() identifies short flags", {
	expect_true(is_short_flag("-v"))
	expect_true(is_short_flag("-3"))
	expect_false(is_short_flag("--verbose"))
	expect_false(is_short_flag("-3.14"))
	expect_false(is_short_flag("foo"))
})

test_that("normalize_opt() splits short flag clusters", {
	# fmt: skip
	spec <- as_spec(matrix(c(
		"s"      , "s", 0, "logical",
		"b"      , "b", 0, "logical",
		"g"      , "g", 0, "logical",
		"verbose", "v", 0, "logical"
	), ncol = 4, byrow = TRUE))
	expect_equal(normalize_opt(c("-sbg", "--verbose"), spec), c("-s", "-b", "-g", "--verbose"))
})

test_that("normalize_opt() treats negative-number-like bundles as arguments when preceding flag takes an argument", {
	# fmt: skip
	spec <- as_spec(matrix(c(
		"mean"   , "m", "store", "double",
		"verbose", "v", "store_true", "logical"
	), ncol = 4, byrow = TRUE))
	expect_equal(normalize_opt(c("-m", "-3.14"), spec), c("-m", "-3.14"))
	expect_equal(normalize_opt(c("--mean", "-1e5"), spec), c("--mean", "-1e5"))
	expect_equal(normalize_opt(c("-v", "-1e5"), spec), c("-v", "-1", "-e", "-5"))
	expect_equal(normalize_opt(c("-1e5"), spec), c("-1", "-e", "-5"))
})

test_that("get_Rscript_filename() ignores --file= after --args", {
	local_mocked_bindings(
		command_args = function() c("Rscript", "--file=script.R", "--args", "--file=fake.R")
	)
	expect_equal(get_Rscript_filename(), "script.R")
})

test_that("get_Rscript_filename() works with no --args", {
	local_mocked_bindings(
		command_args = function() c("Rscript", "--file=script.R")
	)
	expect_equal(get_Rscript_filename(), "script.R")
})

test_that("get_Rscript_filename() returns NA when no --file=", {
	local_mocked_bindings(
		command_args = function() character(0)
	)
	expect_equal(get_Rscript_filename(), NA_character_)
})

test_that("get_Rscript_filename() returns NA when no --file= before --args", {
	local_mocked_bindings(
		command_args = function() c("R", "--args", "--file=boo.r")
	)
	expect_equal(get_Rscript_filename(), NA_character_)
})
