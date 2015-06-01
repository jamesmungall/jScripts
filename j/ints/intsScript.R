# Integer Functions
# ==================================
# Start date: 21st July 2012

j.ints.startup=function(showPrints=FALSE){
# dependent packages: sfsmisc
  library(sfsmisc);
 if(showPrints){
   print("loading j/ints/intsScript.R");
   print("dependent packages: sfsmisc");

  }
}
# 
# Aim: Functions for integer work
# Functions: (all preceeded by j.ints. as a namespace)
# 1. toDigitVector
# 2. intToBin
# 3. binToInt
# 4. digitMap
# 5. unique digits only - identifies numbers which have no repeating digits

j.ints.toDigitVector = function(int){
# input: 23648
# output: c(2,3,6,4,8)
  j.check.isInteger(int);
  result = digitsBase(int,10);
  return(result[,1]);    
  
}

j.ints.intToBin = function(i){
# input: 7, i is integer to be converted
# ouput: 111
# code:  digitsBase(7,2) (sfsmisc package, returns a matrix)
  if(!j.check.isInteger(i))return();
  m = digitsBase(i,2);
  v = as.vector(m);
  return(v);
}

j.ints.binToInt <- function(b) { 
# input: c(1,1,1)
# output: 7
# code: (copied from http://tolstoy.newcastle.edu.au/R/e2/help/07/02/10596.html)
  if(!j.check.isBoolean(b)){warning("input should contain only 1,0,TRUE,FALSE");return();}
  result = sum(2^(which(rev(as.numeric(b))==TRUE)-1));
  return(result)
}

j.ints.digitMap1701 = function(d){
  if(d==0) return(6);
  if(d==1) return(2);
  if(d==2) return(5);
  if(d==3) return(5);
  if(d==4) return(4);
  if(d==5) return(5);
  if(d==6) return(6);
  if(d==7) return(3);
  if(d==8) return(7);
  if(d==9) return(6);
  return("digit not found");
}
j.ints.uniqueDigitsVector = function(v){
# input: c(123,1432,222,98723,992)
# output:   c(123,1432,98723) i.e. all elements with duplicate digits are removed
  uniqueDigitsVectorBoolean = sapply(v,j.ints.uniqueDigitsElement);
  return(v[uniqueDigitsVectorBoolean]);
}
j.ints.uniqueDigitsElement= function(e){
#  input: c(11); output: FALSE
#  input: c(1234567890); output: TRUE  
  eDigitVector = j.ints.toDigitVector(e);
  uniqueEDigitVector = unique(eDigitVector);
  allDigitsUniqueBoolean = (length(eDigitVector)==length(uniqueEDigitVector));
  return(allDigitsUniqueBoolean);
}
j.ints.uniqueDigitsVector.test = function(){
  v = c(1234,14312,13512,35612,577532);
  # should return c(1234,35612); since these are the numbers with unique digits
  uniqueVector=j.ints.uniqueDigitsVector(v);
  return(uniqueVector);
}
