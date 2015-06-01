# Statistical tests and computations
# ==================================
# Start date: 1st June 2015
# 1. Load in console as
# source("/Users/jamesmungall/git/jScripts/j/stat/statScript.R")
# or 
# in RStudio, menu bar->Session->set working directory->Users/jamesmungall/git
# then you need > source("jScripts/j/stat/stat/Script.R")
# 2. Run j.stat.startup() to load
# dependent packages: none
j.vm.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/stat/statScript.R");
    print("dependent packages: none");
  }
}
# 
# Aim: To perform statistical tests and operations on data.
# Functions: (all preceeded by j.stat. as a namespace)

#   1. sumOfSquares


j.stat.sumOfSquares <- function(dataVector){
  # 1. sumOfSquares
  # Function to compute sum of squares of a vector.
  # input: c(30,25,35);
  # output: 50
  if(!is.vector(dataVector)){
    stop('Argument to j.stat.sumOfSquares is not a vector.');
  }
  sumOfSquares <- sum((dataVector - mean(dataVector))^2);
  return(sumOfSquares);
}
