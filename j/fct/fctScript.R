# Factorial Design
# ==================================
# Start date: 29th July 2012

j.fct.startup=function(showPrints=FALSE){
  # dependent packages: none
  if(showPrints){
    print("loading j/fct/fctScript.R");
    print("no dependencies");
  }
}

# 
# Aim: Work with factorial design of tables
# Functions: (all preceeded by j.fct. as a namespace)
#
# 1. allPairs
j.fct.allPairs = function(){
  vi=c(17,217,231,410);
  vj=c(7,17,23,41);
  allPairs = expand.grid(FactorI = vi, FactorJ = vj);
  
  # sum function
  sumVector = c();
  for(i in 1:nrow(allPairs)){
    sumVector = c(sumVector,prod(allPairs[i,]));
  }
  allPairs = cbind(allPairs,sumVector);
  
  # sieve1 function
  booleanSieve1 = c();
  for(i in 1:nrow(allPairs)){
    singleValue = allPairs$FactorJ[[i]];
    singleBool = ((singleValue > 20) & (singleValue < 200));
    booleanSieve1 = c(booleanSieve1,singleBool);
  }
  allPairs = cbind(allPairs,booleanSieve1);
  # sieve1 function
  booleanSieve2 = c();
  for(i in 1:nrow(allPairs)){
    singleValue = allPairs$sumVector[[i]];
    singleBool = (singleValue > 2000);
    booleanSieve2 = c(booleanSieve2,singleBool);
  }
  allPairs = cbind(allPairs,booleanSieve2);
  sieves = booleanSieve1&booleanSieve2;
  allPairs = cbind(allPairs,sieves);
  
  #combine sieves
  return(allPairs);
}
