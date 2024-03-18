SA1_Number 1
================
Cuerdo, NHA., Morillo, JMS
2024-03-18

# Summative Assessment 1 - Number 1

A company has three factories producing a product. Factory 1 produces x1
of the product, factory 2 produces x2, and factory 3 produces x3, where
∑i = 1, 3, xi = 1. The defective rates of the products are y1, y2, and
y3 respectively, where ∑i = 1, 3, yi = 0.12. Write a program (user input
for xi) to calculate the probability that a randomly selected product is
defective. Note that your program should render prompt message to
satisfy the following conditions: 1. 0.10 ≤ xi ≤ 0.40 or 10% ≤ xi ≤ 40%
and ∑i = 1, 3, xi = 1.

2.  0.01 ≤ xi ≤ 0.05 or 1% ≤ yi ≤ 5% and ∑i = 1, 3, yi = 0.12.

## Number 1 - Without Sample Input

***this is the raw code from the R script, before encoding as r
markdown.***

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

## Number 1 - With Sample Input

\*\*\*this is the code with provided sample input

``` r
menu1 <- function(x1, x2, x3, y1, y2, y3) {
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
```

``` r
x1 <- 0.3
x2 <- 0.3
x3 <- 0.4

y1 <- 0.03
y2 <- 0.05
y3 <- 0.04
menu1(x1, x2, x3, y1, y2, y3)
```

    ## Probability of selecting a defective product:  0.04
