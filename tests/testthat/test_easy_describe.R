context("Function easy_describe")


test_that(desc = "Testing the function easy_describe, which summarizes any dataset's columns in a basic way", 
          code = {
              tmp <- data.table(iris)[, Log := TRUE][]
              res <- easy_describe(tmp)
              
              ## Expect the correct columns
              cols <- c("col_position", "col_name", "col_class", "count_unique", "count_NA", "count_nonNA", "range_values", "pct_true", "pct_NA")
              expect_equal(colnames(res), cols) 
              
              ## Spot check a particular expected entry
              val <- "FALSE : TRUE"
              expect_true(val %in% res[, range_values])
              
              ## Expect proportion of true's to be 50%
              tmp[1:75, Log := FALSE]
              res <- easy_describe(tmp)
              expect_true(.5 %in% res[, pct_true])
              # expect_identical(res[6, pct_true], .5)
              
              ## Expect proportion of true's to be 50%
              res <- easy_describe(tmp)
              expect_identical(length(res[, col_position]), ncol(tmp))
              expect_identical(res[, unique(count_nonNA)], nrow(tmp))
          })
