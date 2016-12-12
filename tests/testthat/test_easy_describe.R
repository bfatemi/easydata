context("Function easy_describe")


test_that(desc = "Testing the function easy_describe, which summarizes any dataset's columns in a basic way", 
          code = {
              tmp <- data.table(iris)[, Log := TRUE][]
              res <- easy_describe(tmp)
              
              ## Expect the correct columns
              cols <- c("CName", "Class", "Pos", "count_nona", "count_na", "count_unique", "range_value", "pct_true")
              expect_equal(colnames(res), cols) 
              
              ## Spot check a particular expected entry
              val <- "(FALSE):(TRUE)"
              expect_identical(res[6, range_value], val)
              
              ## Expect proportion of true's to be 50%
              tmp[1:75, Log := FALSE]
              res <- easy_describe(tmp)
              expect_identical(res[6, pct_true], .5)
              
              ## Expect proportion of true's to be 50%
              res <- easy_describe(tmp)
              expect_identical(length(res[, Pos]), ncol(tmp))
              expect_identical(res[, unique(count_nona)], nrow(tmp))
          })
