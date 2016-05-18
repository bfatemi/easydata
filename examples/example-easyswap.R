# Create sample data.table
dt <- data.table(A = c("A", NA, "A", "A", NA),
                 B = c(NA, "B", "B", "B", "B"),
                 C = rep(NA, 5),
                 D = c("D", "D", "D", NA, "D"))
# Simple use
easyswap(dt, find="A", swap = "AA")

# ERROR: swaping a numeric into char column. USE FORCE = TRUE
# xSwap(dt, swap = 0)

easyswap(dt, swap = 0, force = T) # No error

# Make a copy of the table rather than an "IN-MEMORY" swap. This will return a NEW dt
easyswap(dt, swap = 0, force = T, copy = T)

# swap in 1 or more specific columns
easyswap(dt,
         find = "0",
         swap = "999",
         cols = c("A", "B"))
