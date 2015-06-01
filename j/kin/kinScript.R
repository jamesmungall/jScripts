# Kinetics Modelling
# ==================================
# Start date: 12th July 2012
# Work on variable p values in binomial: 23rd July 2012

j.kin.startup=function(showPrints=FALSE){
# dependent packages: none
  if(showPrints){
    print("loading j/kin/kinScript.R");
    print("no dependencies");
  }
}
# 
# Aim: To simulate kinetics for fast, slow, reversible and irreversible reactions
# Functions: (all preceeded by j.kin. as a namespace)

j.kin.mm = function(){

  initA = 1000;#A is enzyme
  initB = 8000;#B is substrate
  initC = 0;
  initD = 0;
  kbind = 0.00003;
  kunbind = 0.01;
  kreact = 0.1;
  v = c("time" = 0, "A"=initA, "B"=initB, "C"=initC,"D" = initD, "eq" = 0);
  m = matrix(nrow=0,ncol=6);


  for(i in 1:300){
    m  = rbind(m,v);

    A= v["A"]-v["A"]*v["B"]*kbind + v["C"]*kreact + v["C"]*kunbind;
    B = v["B"]-v["A"]*v["B"]*kbind + v["C"]*kunbind;
    C = v["C"] - v["C"]*kunbind -v["C"]*kreact + v["A"]*v["B"]*kbind;
    D = v["D"] + v["C"]*kreact
    eq = v["C"]/(v["A"]*v["B"]);
    v = c(i,A,B,C,D,eq);
  }
  x = m;
  a = x[,"A"];
  b = x[,"B"];
  c = x[,"C"];
  d = x[,"D"];
  eq = x[,"eq"];
  plot(a, ylim = c(0,1000));
  lines(b,col="blue");
  lines(c,col="red");
  lines(d,col="green");
  return(m);
}

j.kin.eq = function(){

  initA = 1000;
  initB = 1000;
  initC = 0;
  kbind = 0.0003;
  kunbind = 0.1;
  kreact = 0.01;
  v = c("A"=initA, "B"=initB, "C"=initC);
  m = matrix(nrow=0,ncol=3);

  for(i in 1:100){
    m  = rbind(m,v);

    A = v["A"] - v["A"]*v["B"]*kbind + v["C"]*kunbind;
    B = v["B"] - v["A"]*v["B"]*kbind + v["C"]*kunbind;
    C = v["C"] + v["A"]*v["B"]*kbind - v["C"]*kunbind;
    v = c(A,B,C);
  }
  a = m[,"A"];
  b = m[,"B"];
  c = m[,"C"];
  plot(a, ylim = c(0,1000));
  lines(b,col="blue");
  lines(c,col="red");
  return(m);
}

# Variable p parameter in binomial distribution
# Theory: One of the prerequesites of the binomial distribution is that the probability
# of success remains the same over all of the trials. However, in many areas of life,
# it may be anticipated that the success of a trial *is* affected by previous
# successes or failures.
#    A simple variation is therefore to alter p slightly depending upon the previous
# trial.

j.kin.bin1 = function(n=20,p=0.5,incSuccess=0.01,decFailure=0.01){
  results = c();
  pList = c(p);
  for(i in 1:n){
    singleTrial = rbinom(1,1,p);
    if(j.check.isTrue(singleTrial)){
      results<-c(results,1);
      p = p+incSuccess;
    }
    else{
      results<-c(results,0);
      p = p-decFailure;
    }
    if(p>1)p=1;
    if(p<0)p=0;
    pList = c(pList,p);
  }
  average = c(); 
  for(i in 1:n){
    singleMean = mean(results[1:i]);
    average = c(average, singleMean);
  }
  index = c(1:n);
  oResult = list(index,results,average,pList);
  names(oResult) = c('index','results','average','pList');
  return(oResult);
}

