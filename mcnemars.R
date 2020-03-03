
#opens a tsv file
file_open <-function(filename){
  file <-as.data.frame(fread(filename)) #open file using fread(in data.table), converts to data frame 
  return(file)
}
setwd('/Users/camerongibson/Dropbox/UNR/Spring 2020/R for Linguistics/hw02-cgibson6279')
tfile <- file_open("PTB.tsv") # creates global variable for opened file

#takes two inputs and sums the c
tag_checker <-function(file, check1, check2){
  s1.correct <- file$gold.tag == check1 #checks correct tags for system 1
  
  s2.correct <-file$gold.tag == check2 # check correct tags for system 2
  #print(s1.correct)
  #print(s2.correct)
  comp_check <- sum(s1.correct & !s2.correct) #check how many times system one is correct and system 2 is incorrect
  return(comp_check)
}

#creates global variables for each system correct count
Stanford <- tag_checker(tfile, check1 = tfile$Stanford.tag, check2 = tfile$NLP4J.tag)
NLP4J <- tag_checker(tfile, check1 = tfile$NLP4J.tag, check2 = tfile$Stanford.tag)

#run mcnemars test on each system
mcn_test <- function(s1, s2){
  x = min(s1,s2)
  n = s1 + s2
  m = binom.test(x, n, p = 0.5,
             alternative = c("two.sided", "less", "greater"),
             conf.level = 0.95)
  
  return(m)
}

results <- mcn_test(Stanford, NLP4J)
print(results)