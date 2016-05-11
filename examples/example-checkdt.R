#### dt is NULL
checkdt(dt = NULL) # Prints "Error: no data.table provided"
checkdt(dt = NULL, noerror = TRUE) # Returns 999

#### dt not class data.table(data.table)
checkdt(dt = "test") # Prints "Error: dt not of class data.table or data.frame"
checkdt(dt = "test", noerror = TRUE) # Returns 101

#### dt has 0 rows
checkdt(dt = data.table()) # Prints "Error: no data in dt"
checkdt(dt = data.table(), noerror = TRUE) # Returns 555

#### check columns also
DT <- data.table(X = 1, Y = 2)

checkdt(dt = DT, cols = c("A", "B")) # Prints "Error: the following cols not in data.table: A, B"
checkdt(dt = DT, noerror = TRUE) # Returns 422
