# getopt works as expected

    Code
      cat(getopt(spec2, usage = TRUE))
    Output
      Usage: NA [-[-date|d] <character>] [-[-help|h]] [-[-getdata|g]] [-[-market|m] <character>] [-[-threshold|t] <double>]

# append collects repeated flag values into a vector

    Code
      getopt(spec, "-p")
    Condition
      Error in `getopt()`:
      ! flag `p` requires an argument

# operand = 'strict' collects non-flag tokens as operands

    Code
      getopt(spec, c("--unknown"), operand = "strict")
    Condition
      Error in `get_rowmatch()`:
      ! long flag "unknown" is invalid

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
      ! column 3 of "spec" contains invalid action(s): "store_sideways".  Valid actions: "append", "count", "store", "store_false", "store_optional", "store_true".
    Code
      spec <- matrix(c("foo", "f", 0, "bignum"), ncol = 4, byrow = TRUE)
      getopt(spec, "")
    Condition
      Error in `as_spec()`:
      ! column 4 of "spec" contains invalid mode(s): "bignum".  Valid modes: "character", "complex", "double", "integer", "logical".

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
      processing --bar 
      	long option: --bar 
      		requires a argument
      			peeking ahead at: 'baz' 
      				consuming argument 'baz' 

---

    Code
      . <- getopt(spec, c("-fn", "2"), debug = TRUE)
    Output
      normalized opt to: -f -n 2 
      processing -f 
      	short option: -f 
      processing -n 
      	short option: -n 
      		optionally takes an argument
      			peeking ahead at: '2' 
      				consuming argument '2' 

---

    Code
      . <- getopt(spec, c("-b", "-"), debug = TRUE)
    Output
      processing -b 
      	short option: -b 
      		requires a argument
      			peeking ahead at: '-' 
      				consuming argument '-' 

---

    Code
      . <- getopt(spec, c("--foo", "--", "file1.txt", "file2.txt"), debug = TRUE)
    Output
      extracted positional args after `--`: file1.txt file2.txt 
      processing --foo 
      	long option: --foo 

