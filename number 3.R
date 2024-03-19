p <-as.numeric(readline(prompt = "Enter the probability: ")) / 100

simulate_search <- function(p, search_num) {
  search_sim <- rgeom(search_num, p)
  return(search_sim)
}

mean_var <- function(data) {
  mean_val <- mean(data)
  var_val <- var(data)
  return(list(mean = mean_val, variance = var_val))
  
}

search_num <- 10000
search_sim <- simulate_search(p, search_num)

hist(search_sim, breaks = max(search_sim),
     freq = FALSE, main = "Simulated PDF of Searches",
     xlab = "No. of Searches", 
     ylab = "Probability Density",
     col = "violet")


stat_complete <- mean_var(search_sim)
cat("\nMean of Complete set: ", stat_complete$mean, "\n")
cat("\nVariance of Complete set: ", stat_complete$variance, "\n")

condi_search <- search_sim[search_sim > 3]

condi_stats <- mean_var(condi_search)
cat("\nMean of Conditioned set: ", condi_stats$mean, "\n")
cat("\nVariance of Conditioned set: ", condi_stats$variance, "\n")

p4_3 <-sum(search_sim == 4) / sum(search_sim > 3)
p1 <- sum(search_sim == 1) / length(search_sim)

cat("\nP(X = 4 | X > 3: ", p4_3, "\n")
cat("\nP(X = 1)", p1, "\n")

p5_3 <-sum(search_sim == 5) / sum(search_sim > 3)
p2 <- sum(search_sim == 2) / length(search_sim)

cat("\nP(X = 5 | X > 3: ", p5_3, "\n")
cat("\nP(X = 2)", p2, "\n")