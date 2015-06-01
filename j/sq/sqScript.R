# Integer Sequences
# ==================================
# Start date: 17th July 2012

j.sq.startup=function(showPrints=FALSE){
# dependent packages: none
  if(showPrints){
    print("loading j/sq/sqScript.R");
    print("no dependencies");
  }
}
 
# Aim: To generate sequences of integers
# Functions: (all preceeded by j.kin. as a namespace)
# 1. fib - returns first n fibonacci numbers
# 2. perfectSquare  - returns first n square numbers
# 2b. perfectCubes
# 3. primes
# 4. perfectTriangles
# 5. triangular numbers
# 6. power

j.sq.fib = function(n=15){
  y = c(1,1);
  for(i in 1:n){
    y =c(y,y[[length(y)-1]]+y[[length(y)]]);
  }
  return(y);
}

j.sq.perfectSquare=function(n=15){
  y= c();
  for(i in 1:n){
    y = c(y,i*i);
  }
  return(y);
}
j.sq.perfectCube=function(n=15){
  y= c();
  for(i in 1:n){
    y = c(y,i*i*i);
  }
  return(y);
}

j.sq.primes = function(max=300){
  primes = j.io.readCsvToVector("j/data/primes.txt");
  primes = j.vm.dataFrameToVector(primes);
  if(max<300){
    primes = primes[1:max];
  }
  return(primes);
}

j.sq.perfectTriangles = function(max=52){
  perfTri = j.io.readCsvToVector("j/data/perfectTrianglesSmall.txt");
  perfTriMatrix =data.matrix(perfTri);
  
  if(max<52){
    result = perfTriMatrix[1:max,];
  }
  else{
    result = perfTriMatrix;
  }
  return(result);
}

j.sq.triangNumber= function(n = 15){
  y= c();
  for(i in 1:n){
    y = c(y,i*(i+1)/2);
  }
  return(y);
}

j.sq.powerElement = function(el,x){
  return(el^x);
}
j.sq.powerVector = function(v, x){
  listPowers = sapply(v,j.sq.powerElement,x);
  return(listPowers);
}
