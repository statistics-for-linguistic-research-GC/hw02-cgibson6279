d.frame <- function(){
  
  success <- as.numeric(readline(prompt= "How many times did your experiment succeed? "))
  failure <- as.numeric(readline(prompt = "What are the total number of failures? " ))
  s.size <- success + failure
  d <- data.frame("Success" = success, "Total" = success + failure)
  return(d)
}

btest <- function(df){
  s <- df$Success
  f <- df$Total
  b <- binom.test(s, f, p = 0.5,
                  alternative = c("two.sided", "less", "greater"),
                  conf.level = 0.95)
  
  return(b)
}

density <-function(df, prob = 0.5){
  binom <- dbinom(df$Success, df$Total, prob)
  
  return(binom)
}

#this doesn't feel correct (don't like this at all)
plotter <- function(df, prob = 0.5){
  x <- df$Success
  y <- df$Total
  successes <- 1:x
  
  # do each calculation
  probs <- dbinom(successes, size = y, prob)
  
  # make a table from those two values
  probTable <- data.frame("Success" = successes, "Probs" = probs) 
#  print(probTable)
  # display the table
  p <- barplot(height = probTable$Probs, 
          names.arg = x, 
          space = 0, las = 1, 
          ylab = "Probability", 
          xlab = "Syntactic Distribution")
#  p <-hist(probTable$Probs, 
#           main = "Syntactic Distribution",
#           col = "Green",
#           border = "Blue",
#           xlab = "Number of DO selections",
#           ylab = "Probabilities")
#  return(p)
}