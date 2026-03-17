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

