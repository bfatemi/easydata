# easydata 

[![Travis-CI Build Status](https://travis-ci.org/bfatemi/easydata.svg?branch=master)](https://travis-ci.org/bfatemi/easydata)

[![Coverage Status](https://img.shields.io/codecov/c/github/bfatemi/easydata/master.svg)](https://codecov.io/github/bfatemi/easydata?branch=master)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/easydata)](https://cran.r-project.org/package=easydata)


easydata contains utility and helper functions aimed at making data cleaning easier. These functions try to abstract away challenges associated with R's syntax that many novice R users spend the most time struggling through, while providing a flexible interface to clean and wrangle data.

## Installation

You can install easydata from github with:

```R
# install.packages("devtools")
devtools::install_github("bfatemi/easydata")
```

## Example: set class of columns in batch

One of the functions in easydata is called `ClassMorph`. It's purpose is to change classes of columns in your data table in batch. 

> E.g. change all *factor* columns to *character* columns 

This can often be frustrating for novice R users, and a bit annoying for intermediate users. In either case, `ClassMorph` strives to reduce frustration and/or reduce typing.

***

First, let's create example data in a tedious way for no good reason:

```R
dt <- data.table(A = as.factor(c(1:2, "-", 4:5)),
                 B = rep(2.0, 5),
                 C = as.factor(c(1, "garbage", 2, "", 3)),
                 D = as.factor(rep(999, 5)),
                 E = letters[1:5],
                 F = rep("2020-01-22", 5))
dt
#    A B       C   D E          F
# 1: 1 2       1 999 a 2020-01-22
# 2: 2 2 garbage 999 b 2020-01-22
# 3: - 2       2 999 c 2020-01-22
# 4: 4 2         999 d 2020-01-22
# 5: 5 2       3 999 e 2020-01-22
```

Take a look at the current column classes:

```R
pclass(dt)
#        A           B           C           D           E           F 
# "factor"   "numeric"    "factor"    "factor" "character" "character" 
```

Change all factors to integers in-place (memory efficient) specify force because we see in the data that there is "garbage" in a column we'd like to convert to an integer. Default behavior of `ClassMorph` is to warn the user to prevent unintended transformations:

```R
ClassMorph(dt, "factor", "integer", force = TRUE)
```

Again, notice that there is no return object due to the fact that the data is being transformed **in place**. See the new data and column classes:

```R
dt
pclass(dt)
#     A B  C   D E          F
# 1:  1 2  1 999 a 2020-01-22
# 2:  2 2 NA 999 b 2020-01-22
# 3: NA 2  2 999 c 2020-01-22
# 4:  4 2 NA 999 d 2020-01-22
# 5:  5 2  3 999 e 2020-01-22
#
#           A           B           C           D           E           F 
#   "integer"   "numeric"   "integer"   "integer" "character" "character" 
```

***

### Note
Use all data cleaning tools responsibly. Understand your data deeply to ensure any transformations are doing what you think they are doing. For example, if you'd like to use the function `ClassMorph` to change all factor columns to numerics (see example below), understand why they are factors in the first place. 