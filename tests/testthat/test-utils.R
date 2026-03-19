test_that("normalize_opt() splits short flag clusters", {
	expect_equal(normalize_opt(c("-sbg", "--verbose")), c("-s", "-b", "-g", "--verbose"))
})

test_that("normalize_opt() does not split negative numbers", {
	expect_equal(normalize_opt(c("-3", "-3.14", "-1e5")), c("-3", "-3.14", "-1e5"))
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
