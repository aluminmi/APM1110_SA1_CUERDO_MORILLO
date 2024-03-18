SA1_Number 1
================
Cuerdo, NHA., Morillo, JMS
2024-03-18

## Number 1

``` r
menu <- function() {
  cat("Enter the proportions of products produced by factories:\n")
  cat("Factory 1(x1): ")
  x1 <- as.numeric(stdin())
  cat("Factory 2 (x2): ")
  x2 <- as.numeric(stdin())
  cat("Factory 3 (x3): ")
  x3 <- as.numeric(stdin())
  
  cat("Enter the defective rates of products produced by factories:\n")
  cat("Defective rate of Factory 1(y1): ")
  y1 <- as.numeric(stdin())
  cat("Defective rate of Factory 2 (y2): ")
  y2 <- as.numeric(stdin())
  cat("Defective rate of Factory 3 (y3): ")
  y3 <- as.numeric(stdin())
  
  if (!all(c(0.10 <= x1, x1 <= 0.40, 0.10 <= x2, x2 <= 0.40, 0.10 <= x3, x3 <= 0.40))) {
    cat("Invalid input for proportions. Proportions must be between 0.10 and 0.40.\n")
    return()
  }
  
  if (abs(x1 + x2 + x3 - 1) > 1e-6) {
    cat("Invalid input for proportions. Proportions must sum to 1.\n")
    return()
  }
  
  if (!all(c(0.01 <= y1, y1 <= 0.05, 0.01 <= y2, y2 <= 0.05, 0.01 <= y3, y3 <= 0.05))) {
    cat("Invalid input for defective rates. Defective rates must be between 0.01 and 0.05.\n")
    return()
  }
  
  if (abs(y1 + y2 + y3 - 0.12) > 1e-6) {
    cat("Invalid input for defective rates. Defective rates must sum to 0.12.\n")
    return()
  }
  
  prob_defect <- x1 * y1 + x2 * y2 + x3 * y3
  cat("Probability of selecting a defective product: ", prob_defect, "\n")
}

menu()
```

    ## Enter the proportions of products produced by factories:
    ## Factory 1(x1): Factory 2 (x2): Factory 3 (x3): Enter the defective rates of products produced by factories:
    ## Defective rate of Factory 1(y1): Defective rate of Factory 2 (y2): Defective rate of Factory 3 (y3): Invalid input for proportions. Proportions must be between 0.10 and 0.40.

    ## NULL
