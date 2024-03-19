menu <- function() {
  cat("Enter the proportions of products produced by factories:")
  x1 <- as.numeric(readline("Factory 1(x1): "))
  x2 <- as.numeric(readline("Factory 2 (x2): "))
  x3 <- as.numeric(readline("Factory 3 (x3): "))
  
  cat("Enter the defective rates of products produced by factories:")
  y1 <- as.numeric(readline("Defective rate of Factory 1(y1): "))
  y2 <- as.numeric(readline("Defective rate of Factory 2 (y2): "))
  y3 <- as.numeric(readline("Defective rate of Factory 3 (y3): "))
  
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

