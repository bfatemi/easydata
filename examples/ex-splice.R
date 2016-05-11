# Create example data
dt <- data.table(ColA = c(1, 2, 3, 4, 5, 4, 4, 3, 3),
                 ColB = c("a", "b", "b", "c", "c", "c", "d", "e", "f"),
                 ColC = rnorm(9))


# Use "&" to specify a "split"
# EX: Split dt by two values from ColB
splice(DT = dt, ColB = "c" & "b")


# Splitting always takes priority.
# Logical operators like "|" (or), "<", etc. are performed after any splitting.


# EX: Split ColB by two values, and restrict ColA to 3 values
splice(DT = dt, ColB = "c" & "b", ColA = 1 | 2 | 3 | 4)


# The above line is equivalent to the following two:
splice(DT = dt, ColB = "c" & "b", ColA < 5)
splice(DT = dt, ColB = "c" & "b", ColA = 1:4)


# For convenience, we provide the ability to apply a function to a column after 
# slicing the data


# EX: mean of ColC after grouping ColB by two values and subsetting ColA
splice(DT = dt, ColA = "b" & "c", ColA < 5, apply = mean(ColC))


# Splitting is optional


# EX: Subset for ColA < 5 and two values of ColB
splice(DT = dt, ColA < 5, ColB = c("b","c"))