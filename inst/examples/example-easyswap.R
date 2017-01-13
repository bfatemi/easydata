library(data.table)

# Create sample data.table
dt <- data.table(A = c("A", NA, "A", "A", NA),
                 B = c(NA, "B", "B", "B", "B"),
                 C = rep(NA, 5),
                 D = c("D", "D", "D", NA, "D"))
# Simple use
easyswap(dt, find="A", swap = "XXX")

# ERROR: swaping a numeric into char column. USE FORCE = TRUE
# easyswap(dt, swap = 0)
easyswap(dt, swap = 0, force = TRUE) # No error

# Make a copy of the table rather than an "IN-MEMORY" swap. This will return a NEW dt
easyswap(dt, swap = 0, force = TRUE, copy = TRUE)

# swap in 1 or more specific columns
easyswap(dt,
         find = "0",
         swap = "999",
         cols = c("A", "B"))
