# getopt works as expected

    Code
      cat(getopt(spec2, usage = TRUE))
    Output
      Usage: NA [-[-date|d] <character>] [-[-help|h]] [-[-getdata|g]] [-[-market|m] <character>] [-[-threshold|t] <double>]

# more helpful warnings upon incorrect input

    Code
      spec <- matrix(c("count", "c", 1, "integer"), ncol = 4, byrow = TRUE)
      getopt(spec, c("-c", "hello"))$count
    Condition
      Warning in `value[[3L]]()`:
      integer expected, got "hello"
    Output
      [1] "hello"

---

    Code
      spec <- NULL
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! argument "spec" must be a matrix, data frame, or character vector with length divisible by 4.
    Code
      spec <- c("foo", "f", 0)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! argument "spec" must be a matrix, data frame, or character vector with length divisible by 4.
    Code
      spec <- matrix(c("foo", "f", 0, "integer"), ncol = 2)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! "spec" should have at least 4 columns.
    Code
      spec <- matrix(c("foo", "f", 0, "integer", "bar", "b", 0, "integer"), ncol = 8)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! "spec" should have no more than 6 columns.
    Code
      spec <- matrix(c("foo", "f", "store_sideways", "logical"), ncol = 4, byrow = TRUE)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! column 3 of "spec" contains invalid action(s): "store_sideways".  Valid actions: "store_true", "store_false", "store", "store_optional".
    Code
      spec <- matrix(c("foo", "f", 0, "bignum"), ncol = 4, byrow = TRUE)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! column 4 of "spec" contains invalid mode(s): "bignum".  Valid modes: "logical", "integer", "double", "complex", "character".

# Optional usage strings work as expected

    Code
      cat(getopt(spec, usage = TRUE))
    Output
      Usage: NA [-[-foo|f]] [-[-foobar|b] <character>] [-[-biz|z] [<logical>]] [-[-number|n] <double>] [-[-help|h]]
          -f|--foo       foo usage
          -b|--foobar    foobar usage
          -z|--biz       biz usage
          -n|--number    number usage
          -h|--help      help

# debug output

    Code
      . <- getopt(spec, c("--foo", "--bar", "baz"), debug = TRUE)
    Output
      processing --foo 
      	long option: --foo 
      		dangling flag
      			peeking ahead at: " --bar "
      				no argument!
      processing --bar 
      	long option: --bar 
      		dangling flag
      			peeking ahead at: " baz "
      				consuming argument * baz *

---

    Code
      . <- getopt(spec, c("-fn", "2"), debug = TRUE)
    Output
      processing -fn 
      	short option: -fn 
      		dangling flag
      			peeking ahead at: " 2 "
      				consuming argument * 2 *

---

    Code
      . <- getopt(spec, c("-b", "-"), debug = TRUE)
    Output
      processing -b 
      	short option: -b 
      		dangling flag
      			peeking ahead at: " - "
      				consuming "lone dash" argument

