# easydata [![Travis-CI Build Status](https://travis-ci.org/bfatemi/easydata.svg?branch=master)](https://travis-ci.org/bfatemi/easydata) [![Coverage Status](https://img.shields.io/codecov/c/github/bfatemi/easydata/master.svg)](https://codecov.io/github/bfatemi/easydata?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/easydata)](https://cran.r-project.org/package=easydata)

easydata contains utility and helper functions aimed at making data cleaning easier. These functions try to simplify common cleaning tasks by reducing the number of lines of code required to wrangle data, while doing it efficiently at scale. My goal is to define clearly what the task is, to hopefully reduce barriers for new R analysts and improve workflow for data scientists.

## Installation

You can install easydata from github with:

```R
# install.packages("devtools")
devtools::install_github("bfatemi/easydata")
```

## Case Study on Column Classes

Column classes serve a vital role in keeping data analysis organized while reducing potential for human error. That being said, it is sometimes desirable to use class manipulation for side effects. To illustrate, see below where a `factor` column with all numerical values is coerced to be of class `numeric`:

```R
library(data.table)
DT <- data.table(A = as.factor(runif(5, 0, 1)))

# Factor class A looks like this:
#                     A
# 1:   0.39041341887787
# 2: 0.0126065234653652
# 3:  0.994340784382075
# 4:  0.750496516004205
# 5:  0.655289251124486

DT[, A_numeric := as.numeric(A)]

# Numeric class A looks like:
#                     A A_numeric
# 1:   0.39041341887787         2
# 2: 0.0126065234653652         1
# 3:  0.994340784382075         5
# 4:  0.750496516004205         4
# 5:  0.655289251124486         3
```

#### Working with Classes

Another example that comes up frequently is the situation where one or more columns are not of the *expected* class. For example, suppose column A is unexpectedly of class 'factor' when all entries are suppose to be values between 0 and 1. 

> This could happen when one or more entries have character values mixed in.

The naive fix would be to simply coerce column A with `as.numeric` (shown in example above). Recall that this won't work because factors are not represented to the system as the same numerical values that we see.

Instead, we can turn `A` into a character, **and then** into a numeric. This however causes **data loss... BUT with a warning**.

```R
library(data.table)
# contrived data.table
DT <- data.table(A = as.factor(c(runif(2, 0, 1), paste0("ERROR-", runif(1, 0, 1)), runif(2, 0, 1))))

# DT looks like this:
#                          A
# 1:       0.849180618766695
# 2:       0.758721823338419
# 3: ERROR-0.674287669593468
# 4:       0.290968825109303
# 5:     0.00292029627598822

DT[, A := as.numeric(as.character(A))] # warning generated

# Warning message:
# In eval(expr, envir, enclos) : NAs introduced by coercion

# DT now looks like 
#              A
# 1: 0.849180619
# 2: 0.758721823
# 3:          NA
# 4: 0.290968825
# 5: 0.002920296
```

To summarize the important notes:

- Column classes are vital for robust and accurate data analysis
- You can lose information if you ignore them 
    - E.g. negative effect of `as.numeric` shown above can go unnoticed!

### easydata for column class manipulation

Functions that help working with classes include:

- `ClassMorph()`: converts column classes in batches and takes care to avoid situations that may be unexpected, unless explicitly directed by user
- `NumMorph()`: builds on the example above to convert all columns in a table to the numeric class *only if* it can be done safely (default)

For memory efficiency, data is operated on without additional copies being made in memory... That being said, both tools gives users the flexibility to force conversion, or force a copy of the data to protect the original.

#### Usage

- Change all factors to integers in-place (memory efficient)
- Specify `force` because we know the data well
- Coerce all *potential* numeric classes

```R
DT <- ... a data.table
ClassMorph(DT, "factor", "integer", force = TRUE)
NumMorph(DT, columns to change.. or leave NULL for all)
```




