getopt
======

.. image:: https://www.r-pkg.org/badges/version/getopt
    :target: https://cran.r-project.org/package=getopt
    :alt: CRAN Status Badge

.. image:: https://github.com/trevorld/r-getopt/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master
    :target: https://github.com/trevorld/r-getopt/actions
    :alt: R-CMD-check

.. image:: https://codecov.io/github/trevorld/r-getopt/branch/master/graph/badge.svg
    :target: https://app.codecov.io/github/trevorld/r-getopt?branch=master
    :alt: Coverage Status

.. image:: https://cranlogs.r-pkg.org/badges/getopt
    :target: https://cran.r-project.org/package=getopt
    :alt: RStudio CRAN mirror downloads

.. image:: https://tinyverse.netlify.app/badge/getopt
    :target: https://cran.r-project.org/package=getopt
    :alt: Number of dependencies

.. raw:: html

   <img src="man/figures/logo.png" align="right" width="200px" alt="optparse hex sticker">

``getopt`` is an R package designed to be used with ``Rscript`` to write
"#!"-shebang scripts that accept short and long flags/options.  Many users will
prefer using instead the package `optparse <https://github.com/trevorld/r-optparse>`_
which adds extra features (automatically generated help option and usage,
support for default values, basic positional argument support).

To install the last version released on CRAN use the following command:

.. code:: r

    install.packages("getopt")

To install the development version use the following command:

.. code:: r

    install.packages("remotes")
    remotes:install_github("trevorld/r-getopt")

example
-------

An example Rscript using ``getopt`` with R 4.4+ (i.e. support for ``%||%`` and ``|>``):

.. code:: r

    #!/path/to/Rscript
    library('getopt')
    # get options, using the spec as defined by the matrix
    # fmt: skip
    spec <- matrix(c(
      'verbose', 'v', 2, "integer",
      'help'   , 'h', 0, "logical",
      'count'  , 'c', 1, "integer",
      'mean'   , 'm', 1, "double",
      'sd'     , 's', 1, "double"
    ), byrow = TRUE, ncol = 4L)
    opt <- getopt(spec)

    # if help was asked for print a friendly message and exit
    if (isTRUE(opt$help)) {
      getusage(spec) |> cat()
      quit(status = 0)
    }

    # set reasonable defaults for options that were not specified
    opt$mean <- opt$mean %||% 0
    opt$sd <- opt$sd %||% 1
    opt$count <- opt$count %||% 10L
    opt$verbose <- opt$verbose %||% FALSE

    # print some progress messages to stderr, if requested
    if (opt$verbose) write("writing...", stderr())

    # do some operation based on user input
    rnorm(opt$count, mean = opt$mean, sd = opt$sd) |> cat(sep="\n")

An example Rscript using ``getopt`` for old versions of R:

.. code:: r

    #!/path/to/Rscript
    library('getopt')
    # get options, using the spec as defined by the matrix
    spec <- matrix(c(
      'verbose', 'v', 2, "integer",
      'help'   , 'h', 0, "logical",
      'count'  , 'c', 1, "integer",
      'mean'   , 'm', 1, "double",
      'sd'     , 's', 1, "double"
    ), byrow = TRUE, ncol = 4L)
    opt <- getopt(spec)

    # if help was asked for print a friendly message and exit
    if (!is.null(opt$help)) {
      cat(getusage(spec))
      quit(status = 0)
    }

    # set reasonable defaults for options that were not specified
    if (is.null(opt$mean)) opt$mean <- 0
    if (is.null(opt$sd)) opt$sd <- 1
    if (is.null(opt$count)) opt$count <- 10L
    if (is.null(opt$verbose)) opt$verbose <- FALSE

    # print some progress messages to stderr, if requested
    if (opt$verbose) write("writing...", stderr())

    # do some operation based on user input
    cat(rnorm(opt$count, mean = opt$mean, sd = opt$sd), sep = "\n")
